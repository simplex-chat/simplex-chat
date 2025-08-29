{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module APIDocs where

import API.Docs.Commands
import API.Docs.Events
import API.Docs.Generate
import qualified API.Docs.Generate.TypeScript as TS
import API.Docs.Responses
import API.Docs.Types
import API.TypeInfo
import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.List (foldl', intercalate, sort, (\\))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Simplex.Messaging.Util (ifM)
import System.Directory (doesFileExist)
import Test.Hspec

apiDocsTest :: Spec
apiDocsTest = do
  describe "API commands" $ do
    it "should be documented" testCommandsHaveDocs
    it "should have field names" testCommandsHaveNamedFields
    it "should have defined responses" testCommandsHaveResponses
    it "generate markdown" $ testGenerate commandsDocFile commandsDocText
  describe "API responses" $ do
    it "should be documented" testResponsesHaveDocs
  describe "API events" $ do
    it "should be documented" testEventsHaveDocs
    it "generate markdown" $ testGenerate eventsDocFile eventsDocText
  describe "API types" $ do
    it "should be documented" testTypesHaveDocs
    it "generate markdown" $ testGenerate typesDocFile typesDocText
  describe "TypeScript" $ do
    it "generate typescript commands code" $ testGenerate TS.commandsCodeFile TS.commandsCodeText
    it "generate typescript responses code" $ testGenerate TS.responsesCodeFile TS.responsesCodeText
    it "generate typescript events code" $ testGenerate TS.eventsCodeFile TS.eventsCodeText
    it "generate typescript types code" $ testGenerate TS.typesCodeFile TS.typesCodeText

documentedCmds :: [String]
documentedCmds = concatMap (map consName' . commands) chatCommandsDocs

documentedCmdTypes :: [ATUnionMember]
documentedCmdTypes = concatMap (map commandType . commands) chatCommandsDocs

documentedResps :: [String]
documentedResps = map consName' chatResponsesDocs

documentedRespTypes :: [ATUnionMember]
documentedRespTypes = map responseType chatResponsesDocs

documentedEvts :: [String]
documentedEvts = concatMap (\cat -> map consName' $ mainEvents cat ++ otherEvents cat) chatEventsDocs

documentedEvtTypes :: [ATUnionMember]
documentedEvtTypes = concatMap (\cat -> map eventType $ mainEvents cat ++ otherEvents cat) chatEventsDocs

documentedTypes :: [String]
documentedTypes = map docTypeName chatTypesDocs

testCommandsHaveDocs :: IO ()
testCommandsHaveDocs = do
  let typeCmds = sort $ map consName' chatCommandsTypeInfo
      allCmds = sort $ documentedCmds ++ cliCommands ++ undocumentedCommands
      missingCmds = typeCmds \\ allCmds
      extraCmds = allCmds \\ typeCmds
  unless (null missingCmds) $ expectationFailure $ "Undocumented commands: " <> intercalate ", " missingCmds
  unless (null extraCmds) $ expectationFailure $ "Unused commands: " <> intercalate ", " extraCmds
  putStrLn $ "Documented commands: " <> show (length documentedCmds) <> "/" <> show (length allCmds)
  allCmds `shouldBe` typeCmds -- sanity check

testCommandsHaveNamedFields :: IO ()
testCommandsHaveNamedFields = do
  let docCmds = S.fromList documentedCmds
      unnamedFields = filter (\RecordTypeInfo {consName, fieldInfos} -> consName `S.member` docCmds && any (\FieldInfo {fieldName} -> null fieldName) fieldInfos) chatCommandsTypeInfo
  unless (null unnamedFields) $ expectationFailure $ "Commands with unnamed fields: " <> intercalate ", " (map consName' unnamedFields)

testResponsesHaveDocs :: IO ()
testResponsesHaveDocs = do
  let typeResps = sort $ "CRChatCmdError" : map consName' chatResponsesTypeInfo
      allResps = sort $ documentedResps ++ undocumentedResponses
      missingResps = typeResps \\ allResps
      extraResps = allResps \\ typeResps
  unless (null missingResps) $ expectationFailure $ "Undocumented responses: " <> intercalate ", " missingResps
  unless (null extraResps) $ expectationFailure $ "Unused responses: " <> intercalate ", " extraResps
  putStrLn $ "Documented responses: " <> show (length documentedResps) <> "/" <> show (length allResps)
  allResps `shouldBe` typeResps -- sanity check

testEventsHaveDocs :: IO ()
testEventsHaveDocs = do
  let typeEvts = sort $ "CEvtChatError" : map consName' chatEventsTypeInfo
      allEvts = sort $ documentedEvts ++ undocumentedEvents
      missingEvts = typeEvts \\ allEvts
      extraEvts = allEvts \\ typeEvts
  unless (null missingEvts) $ expectationFailure $ "Undocumented events: " <> intercalate ", " missingEvts
  unless (null extraEvts) $ expectationFailure $ "Unused events: " <> intercalate ", " extraEvts
  putStrLn $ "Documented events: " <> show (length documentedEvts) <> "/" <> show (length allEvts)
  allEvts `shouldBe` typeEvts -- sanity check

testTypesHaveDocs :: IO ()
testTypesHaveDocs = do
  let allDocTypes = sort $ documentedTypes ++ primitiveTypes
      apiTypes = sort $ nubOrd $ concatMap unionMemberTypes $ documentedCmdTypes ++ documentedRespTypes ++ documentedEvtTypes
      extraTypes = allDocTypes \\ apiTypes
      missingTypes = apiTypes \\ allDocTypes
  unless (null extraTypes) $ expectationFailure $ "Unused types: " <> intercalate ", " extraTypes
  unless (null missingTypes) $ expectationFailure $ "Undocumented types: " <> intercalate ", " missingTypes
  allDocTypes `shouldBe` apiTypes
  putStrLn $ "Documented types: " <> show (length allDocTypes)
  where
    unionMemberTypes :: ATUnionMember -> [ConsName]
    unionMemberTypes (ATUnionMember _ fields) = concatMap recordFiledTypes fields
    recordFiledTypes :: APIRecordField -> [ConsName]
    recordFiledTypes (APIRecordField _ t) = apiTypeTypes t
    apiTypeTypes :: APIType -> [ConsName]
    apiTypeTypes = \case
      ATPrim (PT t) -> [t]
      ATDef td -> typeDefTypes td
      ATRef t -> [t] -- ??
      ATOptional t -> apiTypeTypes t
      ATArray t _ -> apiTypeTypes t
      ATMap (PT t) v -> t : apiTypeTypes v
    typeDefTypes :: APITypeDef -> [ConsName]
    typeDefTypes (APITypeDef t td) = t : case td of
      ATDRecord fields -> concatMap recordFiledTypes fields
      ATDUnion members -> concatMap unionMemberTypes members
      ATDEnum _ -> []

testCommandsHaveResponses :: IO ()
testCommandsHaveResponses = do
  let analyzeCmd (cmdsNoResp, rs) CCDoc {consName, responses}
        | null responses = (consName : cmdsNoResp, rs)
        | otherwise = (cmdsNoResp, rs `S.union` S.fromList (map consName' responses))
      (cmdsNoResponses, cmdResponses) = foldl' analyzeCmd ([], S.empty) $ concatMap commands chatCommandsDocs
      undocResps = S.toList $ cmdResponses `S.difference` S.fromList documentedResps
      extraResps = S.toList $ S.fromList documentedResps `S.difference` cmdResponses
  unless (null cmdsNoResponses) $ expectationFailure $ "Commands without responses: " <> intercalate ", " (reverse cmdsNoResponses)
  unless (null undocResps) $ expectationFailure $ "Undocumented command responses: " <> intercalate ", " undocResps
  unless (null extraResps) $ expectationFailure $ "Unused documented command responses: " <> intercalate ", " extraResps

testGenerate :: FilePath -> T.Text -> IO ()
testGenerate file text = do
  current <- ifM (doesFileExist file) (T.readFile file) (pure "")
  T.writeFile file text
  text `shouldBe` current
