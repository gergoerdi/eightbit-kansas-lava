{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EightBit.PET.Board (board, fromImage) where

import EightBit.PET.Utils
import EightBit.PET.PIA
import MOS6502.Types
import MOS6502.Utils
import MOS6502.CPU

import Language.KansasLava

import Data.Sized.Ix
import Data.Sized.Unsigned
import qualified Data.Sized.Matrix as Matrix
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

fromImage :: (Integral a) => ByteString -> a -> Byte
fromImage bs addr | addr' < BS.length bs = fromIntegral . BS.index bs $ addr'
                  | otherwise = 0x00
  where
    addr' = fromIntegral addr

board :: forall clk. (Clock clk)
      => ByteString
      -> Signal clk Bool
      -> Signal clk Byte
      -> (Signal clk (U10 -> Byte), Signal clk X10, (CPUIn clk, CPUOut clk, CPUDebug clk))
board kernalImage vsync kbRow = (vRAM, kbRowSelect, (cpuIn, cpuOut, cpuDebug))
  where
    cpuIn = CPUIn{..}
    (cpuOut@CPUOut{..}, cpuDebug) = cpu cpuIn

    cpuNMI = high

    -- Slow down CPU 50-fold
    -- cpuWait = runRTL $ do
    --     counter <- newReg (0 :: Byte)
    --     counter := mux (reg counter .==. 50) (reg counter + 1, 0)
    --     return $ reg counter ./=. 0
    cpuWait = low

    page = cpuMemA `shiftR` 12

    vAddr :: Signal clk U10
    vAddr = unsigned cpuMemA

    isVideo = page .==. 0x8
    vPipe = forceDefined Nothing $
            packEnabled (isEnabled cpuMemW .&&. isVideo) $
            forceDefined (0, 0) $
            pack (vAddr, enabledVal cpuMemW)
    vRAM = writeMemory vPipe
    vRead = syncRead vRAM vAddr

    mAddr :: Signal clk U13
    mAddr = unsigned cpuMemA

    isMemory = page .<. 0x2 -- 8K RAM
    mPipe = packEnabled (isEnabled cpuMemW .&&. isMemory) $
            pack (mAddr, enabledVal cpuMemW)
    mRAM = writeMemory mPipe
    mRead = syncRead mRAM mAddr

    kernalAddr :: Signal clk U14
    kernalAddr = unsigned cpuMemA
    isKernal = 0xC .<=. page
    kernalROM = rom kernalAddr (Just . fromImage kernalImage)

    piaAddr :: Signal clk U2
    piaAddr = delay $ unsigned cpuMemA

    viaAddr :: Signal clk U4
    viaAddr = delay $ unsigned cpuMemA

    isPIA1 = (cpuMemA .&. 0xFFF0) .==. 0xE810
    PIAOut{ piaR = readPIA1, piaIRQB = cpuIRQ, piaOutputA = unsigned -> kbRowSelect } =
        pia PIAIn{ piaA = packEnabled isPIA1 (unsigned cpuMemA)
                 , piaW = cpuMemW
                 , piaTriggerA = (low, low)
                 , piaTriggerB = (vsync, low)
                 , piaInputA = 0xFF
                 , piaInputB = complement kbRow
                 }

    isPIA2 = (cpuMemA .&. 0xFFF0) .==. 0xE820
    readPIA2 = flip muxMatrix piaAddr . packMatrix . Matrix.fromList $
               [ pureS 0xFF -- $0 PIA2_PA
               , pureS 0xFF -- $1 PIA2_CRA
               , pureS 0xFF -- $2 PIA2_PB
               , pureS 0xFF -- $3 PIA2_CRB
               ]

    isVIA = (cpuMemA .&. 0xFFF0) .==. 0xE840
    readVIA = flip muxMatrix viaAddr . packMatrix . Matrix.fromList $
              [ pureS 0xDF -- $0 VIA_PRB
              , pureS 0xDF -- $1 VIA_PRA
              , pureS 0xDF -- $2 VIA_DDRB
              , pureS 0xDF -- $3 VIA_DDRA
              , pureS 0xDF -- $4 VIA_T1CL
              , pureS 0xDF -- $5 VIA_T1CH
              , pureS 0xDF -- $6 VIA_T1LL
              , pureS 0xDF -- $7 VIA_T1LH
              , pureS 0xDF -- $8 VIA_T2CL
              , pureS 0xDF -- $9 VIA_T2CH
              , pureS 0xDF -- $A VIA_SR
              , pureS 0xDF -- $B VIA_ACR
              , pureS 0xDF -- $C VIA_PCR
              , pureS 0xDF -- $D VIA_IFR
              , pureS 0xDF -- $E VIA_IER
              , pureS 0xDF -- $F VIA_PRA_NHS
              ]

    cpuMemR = forceDefined 0x20 $
              memoryMapping
              [ (isPIA1,   readPIA1)
              , (isPIA2,   readPIA2)
              , (isVIA,    readVIA)
              , (isKernal, kernalROM)
              , (isVideo,  vRead)
              , (isMemory, mRead)
              ]
