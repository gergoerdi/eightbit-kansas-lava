{-# LANGUAGE OverloadedStrings #-}
module EightBit.PET.Build.Shake where

import Development.Shake
import Development.Shake.FilePath
import Control.Monad
import Language.KansasLava

xilinxRules :: String -> FilePath -> [(FilePath, String)] -> [FilePath] -> [FilePath] -> Rules ()
xilinxRules projName xilinxRoot genVHDLs srcs ipcores = do
    "build" </> projName <.> "bit" %> \_out -> do
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
    xilinx tool args = cmd (Cwd "build") (xilinxRoot </> tool) args
