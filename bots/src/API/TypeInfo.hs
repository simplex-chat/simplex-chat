{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
  | TIOptional String -- for Maybe
  | TIArray {elemType :: TypeInfo, nonEmpty :: Bool} -- for [] and NonEmpty
  | TIMap {keyType :: String, valueType :: TypeInfo} -- keys are only base types
  | TIOther String String
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
          _ -> TIType (show tr)
        "NonEmpty" -> case args of
          [elemTr] -> TIArray {elemType = toTypeInfo elemTr, nonEmpty = True}
          _ -> TIType (show tr)
        "Maybe" -> case args of
          [innerTr] -> TIOptional (show innerTr)
          _ -> TIType (show tr)
        "Map" -> case args of
          [keyTr, valTr] -> TIMap {keyType = show keyTr, valueType = toTypeInfo valTr}
          _ -> TIType (show tr)
        _ -> TIType (show tr)

deriving instance Generic ChatCommand

deriving instance Generic ChatResponse

deriving instance Generic ChatEvent

chatCommandTypeInfo :: [(ConsName, [FieldInfo])]
chatCommandTypeInfo = gTypeInfo @(Rep ChatCommand)

chatResponseTypeInfo :: [(ConsName, [FieldInfo])]
chatResponseTypeInfo = gTypeInfo @(Rep ChatResponse)

chatEventConsInfo :: [(ConsName, [FieldInfo])]
chatEventConsInfo = gTypeInfo @(Rep ChatEvent)


-- class GExamples f where
--   gexamples :: [f p]

-- instance GExamples U1 where
--   gexamples = [U1]

-- instance (Default c) => GExamples (K1 i c) where
--   gexamples = [K1 def]

-- instance (GExamples f) => GExamples (M1 i t f) where
--   gexamples = map M1 gexamples

-- instance (GExamples l, GExamples r) => GExamples (l :*: r) where
--   gexamples = [l :*: r | l <- take 1 gexamples, r <- take 1 gexamples]

-- instance (GExamples l, GExamples r) => GExamples (l :+: r) where
--   gexamples = map L1 gexamples ++ map R1 gexamples

-- class Examples a where
--   examples :: [a]
--   default examples :: forall a k. (GenericK a, GExamplesK (RepK a)) => [a (LoT0 :: LoT k)]
--   examples = map toK gexamplesK

-- class GDefault f where
--   gdef :: f p

-- instance GDefault U1 where
--   gdef = U1

-- instance (Default c) => GDefault (K1 i c) where
--   gdef = K1 def

-- instance (GDefault f) => GDefault (M1 i t f) where
--   gdef = M1 gdef

-- instance (GDefault l, GDefault r) => GDefault (l :*: r) where
--   gdef = gdef :*: gdef

-- -- For sums (multiple constructors: pick the leftmost as canonical default)
-- instance GDefault l => GDefault (l :+: r) where
--   gdef = L1 gdef

-- genericDef :: (Generic a, GDefault (Rep a)) => a
-- genericDef = to gdef

-- instance Examples ChatResponse

-- instance Default (CreatedConnLink c) where def = genericDef

-- deriving instance Generic (CreatedConnLink c)

-- instance Default (ConnectionRequestUri c) where def = genericDef

-- deriving instance Generic (ConnectionRequestUri c)
