{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module APIDocs where

import API.Docs.Commands
import API.Docs.Generate
import API.Docs.Responses
import API.TypeInfo
import Control.Monad
import Data.List (foldl', intercalate, sort, (\\))
import qualified Data.Set as S
import qualified Data.Text.IO as T
import Simplex.Messaging.Util (ifM)
import System.Directory (doesFileExist)
import Test.Hspec

apiDocsTest :: Spec
apiDocsTest = do
  fdescribe "API commands" $ do
    it "should be documented" testCommandsHaveDocs
    it "should have field names" testCommandsHaveNamedFields
    it "should have defined responses" testCommandsHaveResponses
    it "generate docs" generateAPIDocs
  fdescribe "API responses" $ do
    it "should be documented" testResponsesHaveDocs


documentedCmds :: [String]
documentedCmds = concatMap (map (\CCDoc {consName} -> consName) . commands) chatCommandsDocs

documentedResps :: [String]
documentedResps = map (\CRDoc {consName} -> consName) chatResponsesDocs

testCommandsHaveDocs :: IO ()
testCommandsHaveDocs = do
  let typeCmds = sort $ map fst chatCommandsTypeInfo
      allCmds = sort $ documentedCmds ++ cliCommands ++ undocumentedCommands
      missingCommands = typeCmds \\ allCmds
  unless (null missingCommands) $ expectationFailure $ "Undocumented commands: " <> intercalate ", " missingCommands
  putStrLn $ "Documented commands: " <> show (length documentedCmds) <> "/" <> show (length allCmds)
  allCmds `shouldBe` typeCmds -- sanity check

testCommandsHaveNamedFields :: IO ()
testCommandsHaveNamedFields = do
  let docCmds = S.fromList documentedCmds
      unnamedFields = filter (\(cn, fields) -> cn `S.member` docCmds && any (\FieldInfo {fieldName} -> null fieldName) fields) chatCommandsTypeInfo
  unless (null unnamedFields) $ expectationFailure $ "Commands with unnamed fields: " <> intercalate ", " (map fst unnamedFields)

testResponsesHaveDocs :: IO ()
testResponsesHaveDocs = do
  let typeResps = sort $ map fst chatResponsesTypeInfo
      allResps = sort $ documentedResps ++ undocumentedResponses
      missingResponses = typeResps \\ allResps
  unless (null missingResponses) $ expectationFailure $ "Undocumented responses: " <> intercalate ", " missingResponses
  allResps `shouldBe` typeResps -- sanity check

testCommandsHaveResponses :: IO ()
testCommandsHaveResponses = do
  let analyzeCmd (cmdsNoResp, cmdResps) CCDoc {consName, responses}
        | null responses = (consName : cmdsNoResp, cmdResps)
        | otherwise = (cmdsNoResp, S.union cmdResps $ S.fromList responses)
      (cmdsNoResponses, cmdResponses) = foldl' analyzeCmd ([], S.empty) $ concatMap commands chatCommandsDocs
      typeResps = S.fromList $ map fst chatResponsesTypeInfo
      undefinedResps = S.toList $ cmdResponses `S.difference` typeResps
      undocResps = S.toList $ cmdResponses `S.difference` (S.fromList documentedResps)
  unless (null cmdsNoResponses) $ expectationFailure $ "Commands without responses: " <> intercalate ", " (reverse cmdsNoResponses)
  unless (null undefinedResps) $ expectationFailure $ "Undefined command reponses: " <> intercalate ", " undefinedResps
  unless (null undocResps) $ expectationFailure $ "Undocumented command responses: " <> intercalate ", " undocResps

generateAPIDocs :: IO ()
generateAPIDocs = do
  cmdsDoc <- ifM (doesFileExist commandsDocFile) (T.readFile commandsDocFile) (pure "")
  generateCommandsDoc
  newCmdsDoc <- T.readFile commandsDocFile
  newCmdsDoc `shouldBe` cmdsDoc
