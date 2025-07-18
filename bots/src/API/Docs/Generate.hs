{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Docs.Generate where

import API.Docs.Commands
import API.Docs.Responses
import API.TypeInfo
import Control.Monad
import Data.Char (isSpace, isUpper, toLower, toUpper)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Simplex.Messaging.Parsers (dropPrefix)
import System.IO

commandsDocFile :: FilePath
commandsDocFile = "./bots/api/COMMANDS.md"

generateCommandsDoc :: IO ()
generateCommandsDoc =
  withFile commandsDocFile WriteMode $ \h -> do
    hPutStrLn h "# API Commands and Responses\n"
    hPutStrLn h "This file is generated automatically.\n"
    hPutStrLn h "## Command categories\n"
    forM_ chatCommandsDocs $ \CCCategory {categoryName} ->
      T.hPutStrLn h $ "- [" <> categoryName <> "](#" <> headerAnchor categoryName <> ")"
    hPutStrLn h ""
    forM_ chatCommandsDocs $ \CCCategory {categoryName, categoryDescr, commands} -> do
      T.hPutStrLn h $ "\n## " <> categoryName <> "\n\n" <> categoryDescr
      forM_ commands $ \CCDoc {consName, commandDescr, responses} -> do
        T.hPutStrLn h $ "\n\n### " <> T.pack consName <> "\n\n" <> commandDescr
        case find ((consName ==) . consName') chatCommandsTypeInfo of
          Just RecordTypeInfo {fieldInfos = params}
            | length params == 0 -> pure ()
            | otherwise -> do
                hPutStrLn h "\n**Parameters**:"
                printFields h params
          Nothing -> error "Missing command type info"
        if length responses > 1
          then hPutStrLn h "\n**Responses**:"
          else hPutStr h "\n**Response**: "
        forM_ responses $ \name ->
          case (find ((name ==) . consName') chatResponsesTypeInfo, find ((name ==) . consName') chatResponsesDocs) of
            (Just RecordTypeInfo {fieldInfos}, Just CRDoc {responseDescr}) -> do
              let respType = dropPrefix "CR" name
                  respDescr = if T.null responseDescr then camelToSpace respType else T.unpack responseDescr
              when (length responses > 1) $ hPutStrLn h ""
              hPutStrLn h $ respDescr <> "."
              hPutStrLn h $ "- type: \"" <> respType <> "\""
              printFields h fieldInfos
            _ -> error "Missing type info or response description"
  where
    printFields h =
      mapM_ $ \FieldInfo {fieldName, typeInfo} ->
        hPutStrLn h $ "- " <> fieldName <> ": " <> fieldType typeInfo
    fieldType = \case
      TIType t -> t
      TIOptional t -> fieldType t <> "?"
      TIArray {elemType, nonEmpty} -> "[" <> fieldType elemType <> "]" <> (if nonEmpty then " // (non-empty)" else "")
      TIMap {keyType, valueType} -> "{" <> keyType <> " : " <> fieldType valueType <> "}"
    camelToSpace :: String -> String
    camelToSpace [] = []
    camelToSpace (x : xs) = toUpper x : go xs
      where
        go [] = []
        go (y : ys)
          | isUpper y = ' ' : toLower y : go ys
          | otherwise = y : go ys

headerAnchor :: Text -> Text
headerAnchor = T.pack . map (\c -> if isSpace c then '-' else toLower c) . T.unpack
