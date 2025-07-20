{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module APIDocs where

import API.Docs.Commands
import API.Docs.Events
import API.Docs.Generate
import API.Docs.Responses
import API.Docs.Types
import API.TypeInfo
import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.List (find, foldl', intercalate, sort, (\\))
import qualified Data.Map.Strict as M
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
    it "generate markdown" testGenerateCommandsMD
  fdescribe "API responses" $ do
    it "should be documented" testResponsesHaveDocs
  fdescribe "API events" $ do
    it "should be documented" testEventsHaveDocs
    it "generate markdown" testGenerateEventsMD
  fdescribe "API types" $ do
    it "should be documented" testTypesHaveDocs
    it "generate markdown" testGenerateTypesMD

documentedCmds :: [String]
documentedCmds = concatMap (map consName' . commands) chatCommandsDocs

documentedResps :: [String]
documentedResps = map consName' chatResponsesDocs

documentedEvts :: [String]
documentedEvts = concatMap (\cat -> map consName' $ mainEvents cat ++ otherEvents cat) chatEventsDocs

documentedTypes :: [String]
documentedTypes = map docTypeName chatTypesDocs

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
      missingResps = typeResps \\ allResps
      extraResps = allResps \\ typeResps
  unless (null missingResps) $ expectationFailure $ "Undocumented responses: " <> intercalate ", " missingResps
  unless (null extraResps) $ expectationFailure $ "Unused responses: " <> intercalate ", " extraResps
  putStrLn $ "Documented responses: " <> show (length documentedResps) <> "/" <> show (length allResps)
  allResps `shouldBe` typeResps -- sanity check

testEventsHaveDocs :: IO ()
testEventsHaveDocs = do
  let typeEvts = sort $ map consName' chatEventsTypeInfo
      allEvts = sort $ documentedEvts ++ undocumentedEvents
      missingEvts = typeEvts \\ allEvts
      extraEvts = allEvts \\ typeEvts
  unless (null missingEvts) $ expectationFailure $ "Undocumented events: " <> intercalate ", " missingEvts
  unless (null extraEvts) $ expectationFailure $ "Unused events: " <> intercalate ", " extraEvts
  putStrLn $ "Documented events: " <> show (length documentedEvts) <> "/" <> show (length allEvts)
  allEvts `shouldBe` typeEvts -- sanity check

testTypesHaveDocs :: IO ()
testTypesHaveDocs = do
  let docCmds = S.fromList documentedCmds
      docResps = S.fromList documentedResps
      docEvts = S.fromList documentedEvts
      cmds = filter ((`S.member` docCmds) . consName') chatCommandsTypeInfo
      resps = filter ((`S.member` docResps) . consName') chatResponsesTypeInfo
      evts = filter ((`S.member` docEvts) . consName') chatEventsTypeInfo
      allDocTypes = sort $ documentedTypes ++ primitiveTypes
      mainApiTypes = M.unions $ map recTypes $ cmds ++ resps ++ evts
      (mft1, fieldTypeNames) = getFieldTypes S.empty $ M.keys mainApiTypes
      (mft2, fieldTypeDocs) = getTypeDocs $ sort fieldTypeNames
      mft = S.union mft1 mft2
      fieldTypes = concatMap docTypeConstructors fieldTypeDocs
      apiTypes = M.unions $ mainApiTypes : map recTypes fieldTypes
      allTypes = sort $ nubOrd $ M.keys apiTypes ++ fieldTypeNames
      extraTypes = allDocTypes \\ allTypes
      missingTypes = allTypes \\ allDocTypes
  unless (null extraTypes) $ expectationFailure $ "Unused types: " <> intercalate ", " extraTypes
  unless (null missingTypes) $ expectationFailure $ "Undocumented types: " <> intercalate ", " (map (\t -> maybe t (((t <> ": ") <>) . show . S.toList) $ M.lookup t apiTypes) missingTypes)
  unless (null mft) $ expectationFailure $ "Missing field types: " <> intercalate ", " (S.toList mft) -- sanity check?
  allTypes `shouldBe` allDocTypes -- sanity check
  putStrLn $ "Documented types: " <> show (length allTypes)
  where
    recTypes :: RecordTypeInfo -> M.Map ConsName (S.Set ConsName)
    recTypes RecordTypeInfo {consName, fieldInfos} = foldl' (\m FieldInfo {typeInfo} -> foldl' (\m' t -> M.alter (Just . maybe (S.singleton consName) (S.insert consName)) t m') m $ types typeInfo) M.empty fieldInfos
    getFieldTypes :: S.Set ConsName -> [ConsName] -> (S.Set ConsName, [ConsName])
    getFieldTypes missing ts
      | null (sort fts \\ sort ts) = (missing'', fts) -- all field types found
      | otherwise =
          let (missing3, fts') = getFieldTypes missing'' (ts ++ fts)
           in (S.union missing missing3, nubOrd (fts ++ fts'))
      where
        (missing', ds) = getTypeDocs ts
        missing'' = S.union missing missing'
        fts = nubOrd $ concatMap childTypeNames ds
        childTypeNames = concatMap (\RecordTypeInfo {fieldInfos} -> concatMap (\FieldInfo {typeInfo} -> types typeInfo) fieldInfos) . docTypeConstructors
    docTypeConstructors CTDoc {typeInfo = STI {recordTypes}} = recordTypes
    getTypeDocs :: [ConsName] -> (S.Set ConsName, [CTDoc]) -- (not found types and their parents, found types)
    getTypeDocs = foldl' (\acc@(s, ds) t -> if t `elem` primitiveTypes then acc else maybe (S.insert t s, ds) (\d -> (s, d : ds)) $ find ((t ==) . docTypeName) chatTypesDocs) (S.empty, [])
    types = \case
      TIType t -> [consName' t]
      TIOptional t -> types t
      TIArray {elemType} -> types elemType
      TIMap {keyType, valueType} -> consName' keyType : types valueType

testCommandsHaveResponses :: IO ()
testCommandsHaveResponses = do
  let analyzeCmd (cmdsNoResp, rs) CCDoc {consName, responses}
        | null responses = (consName : cmdsNoResp, rs)
        | otherwise = (cmdsNoResp, rs `S.union` S.fromList responses)
      (cmdsNoResponses, cmdResponses) = foldl' analyzeCmd ([], S.empty) $ concatMap commands chatCommandsDocs
      typeResps = S.fromList $ map consName' chatResponsesTypeInfo
      undefinedResps = S.toList $ cmdResponses `S.difference` typeResps
      undocResps = S.toList $ cmdResponses `S.difference` S.fromList documentedResps
      extraResps = S.toList $ S.fromList documentedResps `S.difference` cmdResponses
  unless (null cmdsNoResponses) $ expectationFailure $ "Commands without responses: " <> intercalate ", " (reverse cmdsNoResponses)
  unless (null undefinedResps) $ expectationFailure $ "Undefined command reponses: " <> intercalate ", " undefinedResps
  unless (null undocResps) $ expectationFailure $ "Undocumented command responses: " <> intercalate ", " undocResps
  unless (null extraResps) $ expectationFailure $ "Unused documented command responses: " <> intercalate ", " extraResps

testGenerateCommandsMD :: IO ()
testGenerateCommandsMD = do
  cmdsDoc <- ifM (doesFileExist commandsDocFile) (T.readFile commandsDocFile) (pure "")
  generateCommandsDoc
  newCmdsDoc <- T.readFile commandsDocFile
  newCmdsDoc `shouldBe` cmdsDoc

testGenerateEventsMD :: IO ()
testGenerateEventsMD = do
  evtsDoc <- ifM (doesFileExist eventsDocFile) (T.readFile eventsDocFile) (pure "")
  generateEventsDoc
  newEvtsDoc <- T.readFile eventsDocFile
  newEvtsDoc `shouldBe` evtsDoc

testGenerateTypesMD :: IO ()
testGenerateTypesMD = do
  typesDoc <- ifM (doesFileExist typesDocFile) (T.readFile typesDocFile) (pure "")
  generateTypesDoc
  newTypesDoc <- T.readFile typesDocFile
  newTypesDoc `shouldBe` typesDoc
