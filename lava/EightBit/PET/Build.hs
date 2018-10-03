{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Development.Shake
import Development.Shake.FilePath

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid
import Control.Monad
import Data.Maybe (fromMaybe)

import Language.KansasLava
import Language.KansasLava.VHDL
import Language.Netlist.GenVHDL
import Development.KansasLava.Shake
import Development.KansasLava.Shake.Xilinx

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

        let allSrcs = concat [ [ "gensrc" </> "lava-prelude.vhdl" ]
                             , [ "gensrc" </> modName <.> "vhdl" | (modName, _) <- genVHDLs ]
                             , [ "src" </> src | src <- srcs ]
                             ]

        lavaRules "build" genVHDLs
        xilinxRules (Just papilioPro) "build" projName allSrcs ipcores

        let copy out = do
                alwaysRerun
                copyFileChanged ("ise" </> dropDirectory1 out) out

        "build" </> "ipcore_dir//*.xco" %> copy
        "build" </> "src//*" %> copy

  where
    projName = "PET"
