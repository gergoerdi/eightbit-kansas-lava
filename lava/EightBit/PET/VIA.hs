{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module EightBit.PET.VIA where

import MOS6502.Types
import MOS6502.Utils
import Language.Literals.Binary
import EightBit.PET.Utils

import Language.KansasLava

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix)
import qualified Data.Sized.Matrix as Matrix
import Data.Bits
import Prelude hiding (foldr1, sequence, sequence_)
import Data.Foldable (foldr1, sequence_)
import Data.Traversable (sequence)

data VIAIn clk = VIAIn{ viaA :: Signal clk (Enabled U4)
                      , viaW :: Signal clk (Enabled Byte)
                      , viaInputA, viaInputB :: Signal clk (Bool, Bool, Byte)
                      }
               deriving Show

data VIAOut clk = VIAOut{ viaR :: Signal clk Byte
                        , viaIRQ :: Signal clk ActiveLow
                        , viaOutputA :: Signal clk (Bool, Byte)
                        , viaOutputB :: Signal clk (Bool, Bool, Byte)
                        }
                deriving Show

data ShiftMode = SRFree
               | SRTimer2
               | SRClock
               | SRExternal
               deriving (Eq, Ord, Show, Enum, Bounded)
$(repBitRep ''ShiftMode 2); instance BitRep ShiftMode where bitRep = bitRepEnum

via :: forall clk. (Clock clk) => VIAIn clk -> VIAOut clk
via VIAIn{..} = runRTL $ do
    acr <- newReg (0 :: U8)
    let [_acr0, _acr1, acr2, acr3, acr4, _acr5, _acr6, _acr7] =
            Matrix.toList (unpackMatrix $ bitwise (reg acr) :: Matrix X8 (Signal clk Bool))

    let acr432 = bitwise $ packMatrix (Matrix.fromList [acr2, acr3, acr4] :: Matrix X3 (Signal clk Bool))

    (timer1Int, _trigger1, _, timer1R) <- component isTimer1 $ timer1 low
    (timer2Int, _trigger2, timerLo2, timer2R) <- component isTimer2 $ timer2 high
    (shiftInt, _shiftOut, _shiftClk, shiftR) <- component isShifter $ shifter acr432 cb2In cb1In timerLo2

    let ints = Matrix.fromList
               [ undefined
               , undefined
               , shiftInt
               , undefined
               , undefined
               , timer2Int
               , timer1Int
               ]
    (irq, intR) <- component isInterruptor $ interruptor ints

    let viaR = muxN [ (isTimer1, timer1R)
                    , (isTimer2, timer2R)
                    , (isInterruptor, intR)
                    , (isShifter, shiftR)
                    ]
        viaIRQ = bitNot irq
        viaOutputA = undefined

        cb1Out = undefined
        cb2Out = undefined
        pbOut = undefined
        viaOutputB = pack (cb1Out, cb2Out, pbOut)
    return VIAOut{..}
  where
    component :: (Rep a, Num a)
              => Signal clk Bool
              -> (Signal clk (Enabled a) -> Signal clk (Enabled U8) -> RTL s clk r)
              -> RTL s clk r
    component sel mkPart = mkPart (packEnabled (cs .&&. sel) (unsigned addr)) viaW

    (cs, addr) = unpackEnabled viaA
    (_we, _written) = unpackEnabled viaW

    (_ca1In, _ca2In, _paIn) = unpack viaInputA
    (cb1In, cb2In, _pbIn) = unpack viaInputB

    [_rs0, rs1, rs2, rs3] = Matrix.toList . unpackMatrix $ bitwise addr

    isTimer1 = foldr1 (.&&.) $ zipWith (.==.) [rs3, rs2] [low, high]
    isTimer2 = foldr1 (.&&.) $ zipWith (.==.) [rs3, rs2, rs1] [high, low, low]

    isShifter = addr .==. [b|1010|]

    isIER = addr .==. [b|1110|]
    isIFR = addr .==. [b|1101|]
    isInterruptor = isIER .||. isIFR

interruptor :: forall clk s. (Clock clk)
            => Matrix X7 (Reg s clk Bool)
            -> Signal clk (Enabled U1)
            -> Signal clk (Enabled Byte)
            -> RTL s clk (Signal clk Bool, Signal clk Byte)
interruptor ints a w = do
    ier <- newReg (0 :: U7)
    let ifr0 = bitwise . packMatrix $ fmap reg ints
        irq = (ifr0 .&. reg ier) ./=. 0
        ifr = appendS ifr0 irq

    WHEN cs $
      CASE [ match w $ \w -> do
                  let (val, mode) = unappendS w
                      val' = unpackMatrix . bitwise $ val
                  switch addr $ \sel -> case sel of
                      0x0 -> ier := mux mode (reg ier .&. negate val, reg ier .|. val)
                      0x1 -> sequence_ $ Matrix.zipWith (\b -> WHEN b . (:= low)) val' ints
           ]

    let read = switchS addr [ (0x0, unsigned $ reg ier)
                            , (0x1, ifr)
                           ]
    return (irq, read)
  where
    (cs, addr) = unpackEnabled a

timer :: forall clk s. (Clock clk)
      => Signal clk Bool
      -> Signal clk Bool
      -> Signal clk (Enabled U2)
      -> Signal clk (Enabled Byte)
      -> RTL s clk (Reg s clk Bool, Signal clk Bool, Signal clk Bool, Signal clk Byte)
timer countee freeRun a w = do
    latchLo <- newReg 0
    latchHi <- newReg 0
    let latch = appendS (var latchLo) (var latchHi)

    counter <- newReg (0 :: U16)
    let (counterLo, counterHi) = unappendS (reg counter)
    int <- newReg False
    let restart = int := low

    WHEN cs . CASE $
      [ IF we $ switch addr $ \sel -> case sel of
             0x0 -> latchLo := written
             0x1 -> do
                 latchHi := written
                 counter := latch
                 restart
             0x2 -> latchLo := written
             0x3 -> do
                 latchHi := written
                 restart
      , OTHERWISE $ WHEN (addr .==. 0x0) restart
      ]

    let counter' = reg counter - 1
        triggered = countee .&&. counter' .==. 0 .&&. reg counter ./=. 0
    WHEN triggered $ int := high
    WHEN countee $ counter := mux (triggered .&&. freeRun) (counter', latch)

    let read = switchS addr [ (0x0, counterLo)
                            , (0x1, counterHi)
                            , (0x2, reg latchLo)
                            , (0x3, reg latchHi)
                            ]

    return (int, triggered, counterLo .==. 0, read)
  where
    (cs, addr) = unpackEnabled a
    (we, written) = unpackEnabled w

timer1 :: forall clk s. (Clock clk)
       => Signal clk Bool
       -> Signal clk (Enabled U2)
       -> Signal clk (Enabled Byte)
       -> RTL s clk (Reg s clk Bool, Signal clk Bool, Signal clk Bool, Signal clk Byte)
timer1 = timer high

timer1Test :: Seq (Bool, Bool)
timer1Test = runRTL $ do
    (int, triggered, _, _) <- timer1 high a w
    return $ pack (triggered, var int)
  where
    pipe = [Just (0x0, 0x0A), Just (0x1, 0x00)] ++ replicate 30 Nothing
    a = toS $ map (fmap fst) pipe
    w = toS $ map (fmap snd) pipe

timer2 :: forall clk s. (Clock clk)
       => Signal clk Bool
       -> Signal clk (Enabled U1)
       -> Signal clk (Enabled Byte)
       -> RTL s clk (Reg s clk Bool, Signal clk Bool, Signal clk Bool, Signal clk Byte)
timer2 countee a = timer countee low (mapEnabled unsigned a)

timer2Test :: Seq (Bool, Bool, Bool)
timer2Test = runRTL $ do
    (int, triggered, _, _) <- timer2 countee a w
    return $ pack (triggered, var int, countee)
  where
    pipe = [Just (0x0, 3), Just (0x1, 0)] ++ replicate 30 Nothing
    a = toS $ map (fmap fst) pipe
    w = toS $ map (fmap snd) pipe

    countee = toS $ replicate 3 True ++ replicate 4 False ++ cycle (True : replicate 3 False)

shifter :: forall clk s. (Clock clk)
        => Signal clk U3
        -> Signal clk Bool
        -> Signal clk Bool
        -> Signal clk Bool
        -> Signal clk (Enabled (Unsigned X0))
        -> Signal clk (Enabled U8)
        -> RTL s clk (Reg s clk Bool, Signal clk Bool, Signal clk Bool, Signal clk Byte)
shifter acr432 input clkTimer clkExternal a w = do
    sr <- newReg 0
    counter <- newReg (0 :: W U8)
    int <- newReg False
    let triggered = var counter .==. 0 .&&. reg counter ./=. 0

    let clk = switchS mode [ (SRFree,     clkTimer)
                           , (SRTimer2,   clkTimer)
                           , (SRClock,    iterateS bitNot True)
                           , (SRExternal, clkExternal)
                           ]

        -- held high unless shiftIn is active
        clkIn = out .||. bitNot (mode .==. pureS SRFree) .||. clk

        -- held high unless shiftOut is active
        clkOut = bitNot out .||. clk

    WHEN (bitNot out) $ shiftIn input sr clkIn
    output <- shiftOut sr clkOut

    WHEN cs $ do
        -- a read or a write both restart the counter
        counter := 0
        CASE [ match w $ \v -> sr := v ]
    WHEN (risingEdge clk .&&. mode ./=. pureS SRFree) $ do
        counter := mux (reg counter .==. 0) (reg counter - 1, pureS maxBound)
    WHEN triggered $ int := high

    return (int, output, clk, var sr)
  where
    (cs, _) = unpackEnabled a
    (mode, out) = unappendS acr432 :: (Signal clk ShiftMode, Signal clk Bool)

shiftIn :: forall clk s. (Clock clk)
        => Signal clk Bool
        -> Reg s clk U8
        -> Signal clk Bool
        -> RTL s clk ()
shiftIn sig sr tick = do
    let (_, srHi) = unappendS (reg sr) :: (Signal clk Bool, Signal clk U7)
        (srLo, _) = unappendS (reg sr) :: (Signal clk U7, Signal clk Bool)

    let load = do
            sr := appendS sig srHi
        shift = do
            sr := appendS low srLo

    CASE [ IF (risingEdge tick) load
         , IF (fallingEdge tick) shift
         ]

shiftTest :: forall a clk s. (Clock clk)
          => Signal clk (Enabled (Enabled U8))
          -> Signal clk Bool
          -> (Reg s clk U8 -> Signal clk Bool -> RTL s clk a)
          -> RTL s clk (Signal clk Bool, Signal clk U8, a)
shiftTest cmd tick mkShifter = do
    sr <- newReg 0
    counter <- newReg (0 :: W U8)
    let reset = do
            counter := 0
        count = do
            counter := mux (reg counter .==. 0) (reg counter - 1, pureS maxBound)
        triggered = var counter .==. 0 .&&. reg counter ./=. 0

    CASE [ match cmd $ \w -> do
                reset
                CASE [ match w $ \v -> sr := v ]
         , IF (risingEdge tick) count
         ]

    res <- mkShifter sr tick
    return (triggered, var sr, res)

shiftInTest :: Seq (Bool, U8)
shiftInTest = runRTL $ do
    (finished, read, ()) <- shiftTest disabledS tick $ shiftIn sig
    return $ pack (finished, read)
  where
    tick = iterateS bitNot True
    input = [True, True, True, False, False, False, False, True] ++ repeat False
    sig = toS . concatMap (replicate 2) $ False : input

shiftOut :: forall clk s. (Clock clk)
         => Reg s clk U8
         -> Signal clk Bool
         -> RTL s clk (Signal clk Bool)
shiftOut sr tick = do
    let (srLo, rot) = unappendS (reg sr) :: (Signal clk U7, Signal clk Bool)

    let shift = do
            sr := appendS rot srLo

    WHEN (fallingEdge tick) shift

    let out = var sr `testABit` 0
    return out

shiftOutTest :: Seq (Bool, Bool, Bool)
shiftOutTest = runRTL $ do
    (finished, _, written) <- shiftTest cmds tick shiftOut
    return $ pack (fallingEdge tick, finished, written)
  where
    tick = toS $ cycle [True, True, False]
    cmds = toS $ (Just $ Just 123) : repeat Nothing
