{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Development.KansasLava.Shake
import Development.KansasLava.Shake.Xilinx
import Development.Shake
import Development.Shake.FilePath
import System.Directory
import System.Console.GetOpt
import System.Exit

import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Monoid
import Control.Monad

import Language.KansasLava
import Language.KansasLava.VHDL
import Language.Netlist.GenVHDL

import Hardware.KansasLava.Boards.Papilio.Arcade (Model(..))
import EightBit.PET.Video (synthesize)
import qualified EightBit.PET.Machine as Machine

data Flag = ImageFile FilePath
          | XilinxRoot FilePath
          | PapilioModel String

mkXilinxConfig :: [Flag] -> IO (XilinxConfig, Model)
mkXilinxConfig flags = do
    xilinxRoot <- case [path | XilinxRoot path <- flags] of
        [] -> return "/home/cactus/prog/fpga/Xilinx/14.2/ISE_DS/ISE/bin/lin64"
        [path] -> return path
        _ -> do
            putStrLn "Conflicting flags: --xilinx"
            exitFailure

    (xilinxPlatform, papilioModel) <- do
        model <- case [model | PapilioModel model <- flags] of
            [] -> do
                putStrLn "Defaulting to Papilio Pro"
                return "pro"
            [model] -> return model
            _ -> do
                putStrLn "Conflicting flags: --papilio"
                exitFailure
        return $ case map toLower model of
            "one" -> ("XC3S500E-VQ100-5", PapilioOne)
            "pro" -> ("XC6SLX9-TQG144-2", PapilioPro)

    return (XilinxConfig{..}, papilioModel)

toPETSCII :: Word8 -> Word8
toPETSCII c | 0x40 <= c && c <= 0x90 = c - 0x40
            | otherwise = c

unlinesPETSCII :: [ByteString] -> ByteString
unlinesPETSCII = BS.concat . map (extendTo 40)
  where
    extendTo n bs = bs <> BS.replicate k 0x20
      where
        k = n - (BS.length bs `mod` n)

loadKernal :: FilePath -> IO ByteString
loadKernal fileName = do
    bs <- BS.readFile fileName
    return $ bs <>
      BS.replicate (16 * 256 - BS.length bs - 6) 0 <>
      BS.pack [0x00, 0x00, 0x00, 0xF0, 0x00, 0x00] -- NMI, Reset, IRQ

main :: IO ()
main = do
    font <- BS.readFile "characters-1.901447-08.bin"
    -- kernal <- loadKernal "image/hello.obj"
    kernal <- loadKernal "image/kernal/901447/rom-1.bin"
    -- kernal <- loadKernal "image/AllSuiteA/main.prg"

    createDirectoryIfMissing True "ise"
    setCurrentDirectory "ise"

    let (boardFabric, videoFabric) = Machine.machine font kernal
    forM_ [("MainBoard", boardFabric), ("Video", videoFabric)] $ \(modName, fabric) -> do
        mod <- netlistCircuit modName =<< reifyFabric fabric
        let vhdl = genVHDL mod ["work.lava.all", "work.all"]
        writeFile ("gensrc" </> modName <.> "vhdl") vhdl


  --   shakeArgsWith shakeOptions flags $ \flags targets -> do
  --       (xilinxConfig, model) <- mkXilinxConfig flags

  --       (vhdl, ucf, xaws) <- synthesize model modName (Machine.machine font kernal)
  --       return $ Just $ do
  --           want $ if null targets then [modName <.> "bit"] else targets

  --           lavaRules modName vhdl ucf
  --           xilinxRules xilinxConfig modName xaws
  -- where
  --   flags = [ Option [] ["xilinx"] (ReqArg (Right . XilinxRoot) "path") "Path to Xilinx toolchain"
  --           , Option [] ["papilio"] (ReqArg (Right . PapilioModel) "model") "Target Papilio model (One/Pro)"
  --           ]
  where
    modName = "pet"
