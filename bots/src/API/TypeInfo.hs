{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
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
import Simplex.Chat.Controller

type ConsName = String

data FieldInfo = FieldInfo
  { fieldName :: String,
    typeInfo :: TypeInfo
  }
  deriving (Show)

data TypeInfo
  = TIType String -- for simple types
  | TIOptional TypeInfo -- for Maybe
  | TIArray {elemType :: TypeInfo, nonEmpty :: Bool} -- for [] and NonEmpty
  | TIMap {keyType :: String, valueType :: TypeInfo} -- keys are only base types
  deriving (Show)

class GTypeInfo (f :: Type -> Type) where
  gTypeInfo :: [(ConsName, [FieldInfo])]

instance GTypeInfo U1 where
  gTypeInfo = []

instance GTypeInfo V1 where
  gTypeInfo = []

instance (GTypeInfo f) => GTypeInfo (D1 d f) where
  gTypeInfo = gTypeInfo @f

instance (Constructor c, GFieldsInfo f) => GTypeInfo (C1 c f) where
  gTypeInfo = [(conName (undefined :: M1 C c f p), gfieldsInfo @f)]

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
            | elemTr == typeRep (Proxy @Char) -> TIType "String"
            | otherwise -> TIArray {elemType = toTypeInfo elemTr, nonEmpty = False}
          _ -> TIType (typeName tr)
        "NonEmpty" -> case args of
          [elemTr] -> TIArray {elemType = toTypeInfo elemTr, nonEmpty = True}
          _ -> TIType (typeName tr)
        "Maybe" -> case args of
          [innerTr] -> TIOptional (toTypeInfo innerTr)
          _ -> TIType (typeName tr)
        "Map" -> case args of
          [keyTr, valTr] -> TIMap {keyType = typeName keyTr, valueType = toTypeInfo valTr}
          _ -> TIType (typeName tr)
        _ -> TIType (typeName tr)
  where
    typeName tr' = case tyConName (typeRepTyCon tr') of
      "Text" -> "String"
      "UserPwd" -> "String"
      _ -> show tr'

deriving instance Generic ChatCommand

deriving instance Generic ChatResponse

deriving instance Generic ChatEvent

chatCommandsTypeInfo :: [(ConsName, [FieldInfo])]
chatCommandsTypeInfo = gTypeInfo @(Rep ChatCommand)

chatResponsesTypeInfo :: [(ConsName, [FieldInfo])]
chatResponsesTypeInfo = gTypeInfo @(Rep ChatResponse)

chatEventConsInfo :: [(ConsName, [FieldInfo])]
chatEventConsInfo = gTypeInfo @(Rep ChatEvent)
