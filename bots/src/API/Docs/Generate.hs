{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Docs.Generate where

import API.Docs.Commands
import Control.Monad
import Data.Char (isSpace, toLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO

commandsDocFile :: FilePath
commandsDocFile = "./bots/api/COMMANDS.md"

generateCommandsDoc :: IO ()
generateCommandsDoc =
  withFile commandsDocFile WriteMode $ \h -> do
    hPutStrLn h "# API Commands and Responses\n\n## Command categories\n"
    forM_ chatCommandsDocs $ \CCCategory {categoryName} ->
      T.hPutStrLn h $ "- [" <> categoryName <> "](#" <> headerAnchor categoryName <> ")"
    hPutStrLn h ""
    forM_ chatCommandsDocs $ \CCCategory {categoryName, categoryDescr, commands} -> do
      T.hPutStrLn h $ "\n## " <> categoryName <> "\n\n" <> categoryDescr
      forM_ commands $ \CCDoc {consName, commandDescr} -> do
        T.hPutStrLn h $ "\n### " <> T.pack consName <> "\n\n" <> commandDescr

headerAnchor :: Text -> Text
headerAnchor = T.pack . map (\c -> if isSpace c then '-' else toLower c) . T.unpack
