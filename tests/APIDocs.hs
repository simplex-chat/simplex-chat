{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module APIDocs where

import API.Docs.Commands
import API.Docs.Generate
import API.TypeInfo
import Control.Monad
import Data.List (intercalate, sort)
import qualified Data.Set as S
import qualified Data.Text.IO as T
import Simplex.Messaging.Util (ifM)
import System.Directory (doesFileExist)
import Test.Hspec

apiDocsTest :: Spec
apiDocsTest = do
  fit "should document commands" testCommandsHaveDocs
  fit "should have field names in documented commands" testCommandsHaveNamedFields
  fit "generate API docs" generateAPIDocs

documentedCmds :: [String]
documentedCmds = concatMap (\CCCategory {commands} -> map (\CCDoc {consName} -> consName) commands) chatCommandsDocs

testCommandsHaveDocs :: IO ()
testCommandsHaveDocs = do
  let typeCmds = sort $ map fst chatCommandTypeInfo
      allCmds = sort $ documentedCmds ++ cliCommands ++ undocdCommands
  putStrLn $ "Documented commands: " <> show (length documentedCmds) <> "/" <> show (length allCmds)
  allCmds `shouldBe` typeCmds

testCommandsHaveNamedFields :: IO ()
testCommandsHaveNamedFields = do
  let docCmds = S.fromList documentedCmds
      unnamedFields = filter (\(cn, fields) -> cn `S.member` docCmds && any (\FieldInfo {fieldName} -> null fieldName) fields) chatCommandTypeInfo
  unless (null unnamedFields) $ expectationFailure $ "Documented commands with unnamed fields: " <> intercalate ", " (map fst unnamedFields)

generateAPIDocs :: IO ()
generateAPIDocs = do
  cmdsDoc <- ifM (doesFileExist commandsDocFile) (T.readFile commandsDocFile) (pure "")
  generateCommandsDoc
  newCmdsDoc <- T.readFile commandsDocFile
  cmdsDoc `shouldBe` newCmdsDoc
