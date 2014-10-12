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
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Monoid

import Hardware.KansasLava.Boards.Papilio.Arcade (Model(..))
import EightBit.PET.Video (synthesize)
import qualified EightBit.PET.Board as Board

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

unlinesPETSCII :: [BS.ByteString] -> BS.ByteString
unlinesPETSCII = BS.concat . map (extendTo 40)
  where
    extendTo n bs = bs <> BS.replicate k 0x20
      where
        k = n - (BS.length bs `mod` n)

main :: IO ()
main = do
    font <- BS.readFile "characters-1.901447-08.bin"
    kernal <- BS.readFile "image/FillScreen.obj"
    basic <- return ""

    createDirectoryIfMissing True "ise"
    setCurrentDirectory "ise"
    shakeArgsWith shakeOptions flags $ \flags targets -> do
        (xilinxConfig, model) <- mkXilinxConfig flags

        (vhdl, ucf) <- synthesize model modName (Board.board font kernal basic)
        return $ Just $ do
            want $ if null targets then [modName <.> "bit"] else targets

            lavaRules modName vhdl ucf
            xilinxRules xilinxConfig modName xaws
  where
    flags = [ Option [] ["xilinx"] (ReqArg (Right . XilinxRoot) "path") "Path to Xilinx toolchain"
            , Option [] ["papilio"] (ReqArg (Right . PapilioModel) "model") "Target Papilio model (One/Pro)"
            ]

    modName = "pet"
    xaws = ["dcm_32_to_80"]