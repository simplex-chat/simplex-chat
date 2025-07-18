{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module APIDocs where

import API.Docs.Commands
import API.Docs.Generate
import API.Docs.Responses
import API.Docs.Types
import API.TypeInfo
import Control.Monad
import Data.List (foldl', intercalate, sort, (\\))
import Data.Containers.ListUtils
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
  fdescribe "API types" $ do
    it "should be documented" testTypesHaveDocs


documentedCmds :: [String]
documentedCmds = concatMap (map consName' . commands) chatCommandsDocs

documentedResps :: [String]
documentedResps = map consName' chatResponsesDocs

documentedTypes :: [String]
documentedTypes = map consName' chatTypesDocs

testCommandsHaveDocs :: IO ()
testCommandsHaveDocs = do
  let typeCmds = sort $ map consName' chatCommandsTypeInfo
      allCmds = sort $ documentedCmds ++ cliCommands ++ undocumentedCommands
      missingCommands = typeCmds \\ allCmds
  unless (null missingCommands) $ expectationFailure $ "Undocumented commands: " <> intercalate ", " missingCommands
  putStrLn $ "Documented commands: " <> show (length documentedCmds) <> "/" <> show (length allCmds)
  allCmds `shouldBe` typeCmds -- sanity check

testCommandsHaveNamedFields :: IO ()
testCommandsHaveNamedFields = do
  let docCmds = S.fromList documentedCmds
      unnamedFields = filter (\RecordTypeInfo {consName, fieldInfos} -> consName `S.member` docCmds && any (\FieldInfo {fieldName} -> null fieldName) fieldInfos) chatCommandsTypeInfo
  unless (null unnamedFields) $ expectationFailure $ "Commands with unnamed fields: " <> intercalate ", " (map consName' unnamedFields)

testResponsesHaveDocs :: IO ()
testResponsesHaveDocs = do
  let typeResps = sort $ map consName' chatResponsesTypeInfo
      allResps = sort $ documentedResps ++ undocumentedResponses
      missingResponses = typeResps \\ allResps
  unless (null missingResponses) $ expectationFailure $ "Undocumented responses: " <> intercalate ", " missingResponses
  allResps `shouldBe` typeResps -- sanity check

testTypesHaveDocs :: IO ()
testTypesHaveDocs = do
  let docCmds = S.fromList documentedCmds
      docResps = S.fromList documentedResps
      cmds = filter ((`S.member` docCmds) . consName') chatCommandsTypeInfo
      resps = filter ((`S.member` docResps) . consName') chatResponsesTypeInfo
      rts = sort $ nubOrd $ concatMap respTypes $ cmds ++ resps
      allTypes = sort $ documentedTypes ++ primitiveTypes
      missingTypes = rts \\ allTypes
  unless (null missingTypes) $ expectationFailure $ "Undocumented types: " <> intercalate ", " missingTypes
  where
    respTypes RecordTypeInfo {fieldInfos} = concatMap (\FieldInfo {typeInfo} -> types typeInfo) fieldInfos
    types = \case
      TIType t -> [t]
      TIOptional t -> types t
      TIArray {elemType} -> types elemType
      TIMap {keyType, valueType} -> keyType : types valueType

testCommandsHaveResponses :: IO ()
testCommandsHaveResponses = do
  let analyzeCmd (cmdsNoResp, rs) CCDoc {consName, responses}
        | null responses = (consName : cmdsNoResp, rs)
        | otherwise = (cmdsNoResp, rs `S.union` S.fromList responses)
      (cmdsNoResponses, cmdResponses) = foldl' analyzeCmd ([], S.empty) $ concatMap commands chatCommandsDocs
      typeResps = S.fromList $ map consName' chatResponsesTypeInfo
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
