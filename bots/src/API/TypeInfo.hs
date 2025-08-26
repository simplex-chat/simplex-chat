{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.TypeInfo where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable
import GHC.Generics
import Simplex.Messaging.Parsers (fstToLower)

data APIType
  = ATPrim PrimitiveType
  | ATDef APITypeDef
  | ATRef String -- to support recursive types
  | ATOptional APIType
  | ATArray {elemType :: APIType, nonEmpty :: Bool}
  | ATMap {keyType :: PrimitiveType, valueType :: APIType}

data APITypeDef = APITypeDef {typeName' :: String, typeDef :: APITypeDefinition}

data APITypeDefinition
  = ATDRecord [APIRecordField]
  | ATDUnion (NonEmpty ATUnionMember)
  | ATDEnum (NonEmpty String)

type TypeAndFields = (String, [APIRecordField])

data APIRecordField = APIRecordField {fieldName' :: String, typeInfo :: APIType}

data ATUnionMember = ATUnionMember {memberTag :: String, memberFields :: [APIRecordField]}

newtype PrimitiveType = PT String

pattern TBool :: String
pattern TBool = "bool"

pattern TString :: String
pattern TString = "string"

pattern TInt :: String
pattern TInt = "int"

pattern TInt64 :: String
pattern TInt64 = "int64"

pattern TWord32 :: String
pattern TWord32 = "word32"

pattern TDouble :: String
pattern TDouble = "double"

pattern TJSONObject :: String
pattern TJSONObject = "JSONObject"

pattern TUTCTime :: String
pattern TUTCTime = "UTCTime"

primitiveTypes :: [ConsName]
primitiveTypes = [TBool, TString, TInt, TInt64, TWord32, TDouble, TJSONObject, TUTCTime]

data SumTypeInfo = STI {typeName :: String, recordTypes :: [RecordTypeInfo]}
  deriving (Show)

sti :: forall t. (GTypeInfo (Rep t), GetDatatypeName (Rep t)) => SumTypeInfo
sti = STI {typeName = getDatatypeName @(Rep t), recordTypes = gTypeInfo @(Rep t)}

class GetDatatypeName (f :: Type -> Type) where getDatatypeName :: String

instance (Datatype d) => GetDatatypeName (D1 d g) where
  getDatatypeName = datatypeName (undefined :: D1 d g p)

recordTypesInfo :: forall t. (GTypeInfo (Rep t)) => [RecordTypeInfo]
recordTypesInfo = gTypeInfo @(Rep t)

data RecordTypeInfo = RecordTypeInfo {consName :: ConsName, fieldInfos :: [FieldInfo]}
  deriving (Show)

class ConstructorName t where consName' :: t -> ConsName

instance ConstructorName RecordTypeInfo where consName' RecordTypeInfo {consName} = consName

type ConsName = String

data FieldInfo = FieldInfo {fieldName :: String, typeInfo :: TypeInfo}
  deriving (Show)

data SimpleType = ST {tcName :: ConsName, tcParams :: [String]}
  deriving (Show)

data TypeInfo
  = TIType SimpleType -- for simple types
  | TIOptional TypeInfo -- for Maybe
  | TIArray {elemType :: TypeInfo, nonEmpty :: Bool} -- for [] and NonEmpty
  | TIMap {keyType :: SimpleType, valueType :: TypeInfo} -- keys are only base types
  deriving (Show)

ti :: ConsName -> TypeInfo
ti n = TIType $ ST n []

class GTypeInfo (f :: Type -> Type) where
  gTypeInfo :: [RecordTypeInfo]

instance GTypeInfo U1 where
  gTypeInfo = []

instance GTypeInfo V1 where
  gTypeInfo = []

instance (GTypeInfo f) => GTypeInfo (D1 d f) where
  gTypeInfo = gTypeInfo @f

instance (Constructor c, GFieldsInfo f) => GTypeInfo (C1 c f) where
  gTypeInfo = [RecordTypeInfo {consName = conName (undefined :: M1 C c f p), fieldInfos = gfieldsInfo @f}]

instance (GTypeInfo l, GTypeInfo r) => GTypeInfo (l :+: r) where
  gTypeInfo = gTypeInfo @l ++ gTypeInfo @r

class GFieldsInfo (f :: Type -> Type) where
  gfieldsInfo :: [FieldInfo]

instance GFieldsInfo U1 where
  gfieldsInfo = []

instance GFieldsInfo V1 where
  gfieldsInfo = []

instance (GFieldsInfo l, GFieldsInfo r) => GFieldsInfo (l :*: r) where
  gfieldsInfo = gfieldsInfo @l ++ gfieldsInfo @r

instance forall s i c. (Selector s, Typeable c) => GFieldsInfo (S1 s (K1 i c)) where
  gfieldsInfo = [FieldInfo {fieldName = selName (undefined :: S1 s (K1 i c) p), typeInfo = toTypeInfo $ typeRep (Proxy :: Proxy c)}]

toTypeInfo :: TypeRep -> TypeInfo
toTypeInfo tr =
  let tc = typeRepTyCon tr
      args = typeRepArgs tr
      name = tyConName tc
   in case name of
        "List" -> listType args
        "ListDef" -> listType args
        "NonEmpty" -> case args of
          [elemTr] -> TIArray {elemType = toTypeInfo elemTr, nonEmpty = True}
          _ -> TIType (simpleType tr)
        "Maybe" -> case args of
          [innerTr] -> TIOptional (toTypeInfo innerTr)
          _ -> TIType (simpleType tr)
        "Map" -> case args of
          [keyTr, valTr] -> TIMap {keyType = simpleType keyTr, valueType = toTypeInfo valTr}
          _ -> TIType (simpleType tr)
        _ -> TIType (simpleType tr)
  where
    listType = \case
      [elemTr]
        | elemTr == typeRep (Proxy @Char) -> TIType (ST TString [])
        | otherwise -> TIArray {elemType = toTypeInfo elemTr, nonEmpty = False}
      _ -> TIType (simpleType tr)
    simpleType tr' = primitiveToLower $ case tyConName (typeRepTyCon tr') of
      "AgentUserId" -> ST TInt64 []
      "Integer" -> ST TInt64 []
      "Version" -> ST TInt []
      "BoolDef" -> ST TBool []
      "PQEncryption" -> ST TBool []
      "PQSupport" -> ST TBool []
      "ACreatedConnLink" -> ST "CreatedConnLink" []
      "CChatItem" -> ST "ChatItem" []
      "FormatColor" -> ST "Color" []
      "CustomData" -> ST "JSONObject" []
      "KeyMap" -> ST "JSONObject" []
      "Value" -> ST "JSONObject" []
      "CIQDirection" -> ST "CIDirection" []
      "SendRef" -> ST "ChatRef" []
      t
        | t `elem` stringTypes -> ST TString []
        | t `elem` simplePrefTypes -> ST "SimplePreference" []
        | t `elem` groupPrefTypes -> ST "GroupPreference" []
        | t `elem` roleGroupPrefTypes -> ST "RoleGroupPreference" []
        | otherwise -> case words $ show tr' of
            (tcName : tcParams) -> ST {tcName, tcParams}
            _ -> ST "" []
    primitiveToLower st@(ST t ps) = let t' = fstToLower t in if t' `elem` primitiveTypes then ST t' ps else st
    stringTypes =
      [ "AConnectionLink",
        "AgentConnId",
        "AgentInvId",
        "AgentRcvFileId",
        "AgentSndFileId",
        "B64UrlByteString",
        "CbNonce",
        "ConnectionLink",
        "ConnShortLink",
        "ConnectionRequestUri",
        "FileDigest",
        "GroupLinkId",
        "ImageData",
        "MemberId",
        "Text",
        "MREmojiChar",
        "ProtocolServer",
        "SbKey",
        "SharedMsgId",
        "UIColor",
        "UserPwd",
        "XContactId"
      ]
    simplePrefTypes =
      [ "CallsPreference",
        "FilesPreference",
        "FullDeletePreference",
        "ReactionsPreference",
        "SessionsPreference",
        "VoicePreference"
      ]
    groupPrefTypes =
      [ "FullDeleteGroupPreference",
        "ReactionsGroupPreference",
        "ReportsGroupPreference",
        "HistoryGroupPreference"
      ]
    roleGroupPrefTypes =
      [ "DirectMessagesGroupPreference",
        "VoiceGroupPreference",
        "FilesGroupPreference",
        "SessionsGroupPreference",
        "SimplexLinksGroupPreference"
      ]
