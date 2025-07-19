{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.TypeInfo where

import Data.Kind (Type)
import Data.Typeable
import GHC.Generics

data SumTypeInfo = SumTypeInfo {typeName :: String, recordTypes :: [RecordTypeInfo]}
  deriving (Show)

sumTypeInfo :: forall t. (GTypeInfo (Rep t), GetDatatypeName (Rep t)) => SumTypeInfo
sumTypeInfo = SumTypeInfo {typeName = getDatatypeName @(Rep t), recordTypes = gTypeInfo @(Rep t)}

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

data SimpleTypeInfo = STI {consName :: ConsName, typeParams :: [String]}
  deriving (Show)

instance ConstructorName SimpleTypeInfo where consName' STI {consName} = consName

data TypeInfo
  = TIType SimpleTypeInfo -- for simple types
  | TIOptional TypeInfo -- for Maybe
  | TIArray {elemType :: TypeInfo, nonEmpty :: Bool} -- for [] and NonEmpty
  | TIMap {keyType :: SimpleTypeInfo, valueType :: TypeInfo} -- keys are only base types
  deriving (Show)

ti :: ConsName -> TypeInfo
ti n = TIType $ STI n []

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
        "List" -> case args of
          [elemTr]
            | elemTr == typeRep (Proxy @Char) -> TIType string
            | otherwise -> TIArray {elemType = toTypeInfo elemTr, nonEmpty = False}
          _ -> TIType (simpleType tr)
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
    string = STI "String" []
    simpleType tr' = case tyConName (typeRepTyCon tr') of
      "AgentUserId" -> STI "Int64" []
      "Integer" -> STI "Int64" []
      "Version" -> STI "Int" []
      "PQEncryption" -> STI "Bool" []
      "PQSupport" -> STI "Bool" []
      "ACreatedConnLink" -> STI "CreatedConnLink" []
      "CChatItem" -> STI "ChatItem" []
      "CustomData" -> STI "JSONObject" []
      "KeyMap" -> STI "JSONObject" []
      t
        | t `elem` stringTypes -> STI "String" []
        | t `elem` simplePrefTypes -> STI "SimplePreference" []
        | t `elem` groupPrefTypes -> STI "GroupPreference" []
        | t `elem` roleGroupPrefTypes -> STI "RoleGroupPreference" []
        | otherwise -> case words $ show tr' of
            (consName : typeParams) -> STI {consName, typeParams}
            _ -> STI "" []
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
        "SbKey",
        "SharedMsgId",
        "UIColor",
        "UserPwd",
        "XContactId"
      ]
    simplePrefTypes =
      [ "CallsPreference",
        "FullDeletePreference",
        "ReactionsPreference",
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
        "SimplexLinksGroupPreference"
      ]
