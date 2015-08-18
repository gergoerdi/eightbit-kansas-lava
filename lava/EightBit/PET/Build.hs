{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

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
        -- want [ "build" </> projName <.> "tcl" ]

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
  where
    projName = "PET"
    xilinx tool args = cmd (Cwd "build") (xilinxRoot </> tool) args
    xilinxRoot = "/home/cactus/prog/fpga/Xilinx/14.2/ISE_DS/ISE/bin/lin64"
