{-# LANGUAGE OverloadedStrings #-}
module EightBit.PET.Build.Shake where

import Development.Shake
import Development.Shake.FilePath
import Control.Monad
import Language.KansasLava

import qualified Data.Text.Lazy as TL
import qualified Data.Text as TS
import Text.Hastache

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
    -- "build" </> "*.tcl" %> copy

    let projCtxt key = return $ case key of
            "project" -> MuVariable projName
            "ipcores" -> MuList [ \key -> return $ case key of
                                       "name" -> MuVariable name
                                       _ -> MuNothing
                                | xco <- ipcores
                                , let name = dropExtension xco
                                ]
            "gensrcs" -> MuList [ \key -> return $ case key of
                                       "name" -> MuVariable name
                                       _ -> MuNothing
                                | name <- "lava-prelude" : map fst genVHDLs
                                ]
            "srcs" -> MuList [ \key -> return $ case key of
                                    "name" -> MuVariable name
                                    _ -> MuNothing
                             | name <- srcs
                             ]
            _ -> MuNothing
    "build" </> "*.tcl" %> hastache projCtxt
  where
    xilinx tool args = cmd (Cwd "build") (xilinxRoot </> tool) args

hastache :: MuContext IO -> FilePath -> Action ()
hastache ctxt target = do
    alwaysRerun
    -- templateFile <- liftIO $ getDataFileName ("ise.template" </> templateName)
    let templateFile = "ise" </> templateName
    t <- liftIO $ hastacheFile hastacheConfig templateFile ctxt
    writeFileChanged target $ TL.unpack t
  where
    hastacheConfig = MuConfig{ muEscapeFunc = emptyEscape
                             , muTemplateFileDir = Nothing
                             , muTemplateFileExt = Just "mustache"
                             , muTemplateRead = const $ return Nothing
                             }

    ext = drop 1 . takeExtension $ target
    templateName = ext <.> "mustache"

listTemplate :: TS.Text -> [MuContext IO] -> FilePath -> Action ()
listTemplate key0 entities = hastache ctxt
  where
    ctxt key = return $ if key == key0 then MuList entities else MuNothing

textTemplate :: [(TS.Text, TL.Text)] -> FilePath -> Action ()
textTemplate replacements = hastache ctxt
  where
    ctxt key = return $ case lookup key replacements of
        Just value -> MuVariable value
        Nothing -> MuNothing
