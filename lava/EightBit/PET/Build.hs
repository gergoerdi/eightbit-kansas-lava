{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

-- import Development.KansasLava.Shake
-- import Development.KansasLava.Shake.Xilinx
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

-- mkXilinxConfig :: [Flag] -> IO (XilinxConfig, Model)
-- mkXilinxConfig flags = do
--     xilinxRoot <- case [path | XilinxRoot path <- flags] of
--         [] -> return "/home/cactus/prog/fpga/Xilinx/14.2/ISE_DS/ISE/bin/lin64"
--         [path] -> return path
--         _ -> do
--             putStrLn "Conflicting flags: --xilinx"
--             exitFailure

--     (xilinxPlatform, papilioModel) <- do
--         model <- case [model | PapilioModel model <- flags] of
--             [] -> do
--                 putStrLn "Defaulting to Papilio Pro"
--                 return "pro"
--             [model] -> return model
--             _ -> do
--                 putStrLn "Conflicting flags: --papilio"
--                 exitFailure
--         return $ case map toLower model of
--             "one" -> ("XC3S500E-VQ100-5", PapilioOne)
--             "pro" -> ("XC6SLX9-TQG144-2", PapilioPro)

--     return (XilinxConfig{..}, papilioModel)

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

    let (boardFabric, videoFabric) = Machine.machine font kernal
    genVHDLs <- forM [("MainBoard", boardFabric), ("Video", videoFabric)] $ \(modName, fabric) -> do
        mod <- netlistCircuit modName =<< reifyFabric fabric
        return (modName, genVHDL mod ["work.lava.all", "work.all"])
    let ipcores = ["ClockMan.xco"]
        srcs = ["PET.vhdl", "PET.ucf", "ToggleSync.vhdl", "DualRAM.vhdl"]

    shakeArgs shakeOptions{ shakeFiles = "build/.shake" } $ do
        want [ "build" </> projName <.> "bit" ]

        "build" </> projName <.> "bit" %> \out -> do
            need . concat $ [ [ "build" </> "gensrc" </> modName <.> "vhdl" | (modName, _) <- genVHDLs ]
                            , [ "build" </> "gensrc/lava-prelude.vhdl" ]
                            , [ "build" </> "src" </> src | src <- srcs ]
                            , [ "build" </> "ipcore_dir" </> xco | xco <- ipcores ]
                            , [ "build" </> projName <.> "tcl" ]
                            ]

            xilinx "xtclsh" [projName <.> "tcl", "rebuild_project"]

        "build" </> "gensrc/lava-prelude.vhdl" %> \out -> do
            alwaysRerun
            withTempFile $ \tempFile -> do
                liftIO $ writeVhdlPrelude tempFile
                copyFileChanged tempFile out
        forM_ genVHDLs $ \(modName, vhdl) -> do
            "build" </> "gensrc" </> modName <.> "vhdl" %> \out -> do
                alwaysRerun
                writeFileChanged out vhdl

        let copy out = do
                alwaysRerun
                copyFileChanged ("ise" </> dropDirectory1 out) out

        "build" </> "ipcore_dir//*.xco" %> copy
        "build" </> "src//*" %> copy
        "build" </> "*.tcl" %> copy

    -- createDirectoryIfMissing True "ise"
    -- setCurrentDirectory "ise"
    -- writeFileChanged ("gensrc" </> modName <.> "vhdl") vhdl



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
    projName = "PET"
    xilinx tool args = cmd (Cwd "build") (xilinxRoot </> tool) args
    xilinxRoot = "/home/cactus/prog/fpga/Xilinx/14.2/ISE_DS/ISE/bin/lin64"
