{-# LANGUAGE LambdaCase #-}

module API.Docs.Syntax.Types where

import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup
import Data.String

type ExprParam = String -- param name

data Expr
  = Concat (NonEmpty Expr)
  | Param ExprParam
  | Json ExprParam
  | OnOff ExprParam -- does not include leading space
  | OnOffParam String ExprParam (Maybe Bool) -- name, param, default. Includes leading space in all cases. Name must not be empty
  | Join Char ExprParam
  | Optional Expr Expr ExprParam -- Nothing expr, Just expr (using [$0] as ExprParam), optional param
  | Const String
  deriving (Eq, Show)

isConst :: Expr -> Bool
isConst = \case
  Const _ -> True
  _ -> False

instance IsString Expr where fromString = Const

instance Semigroup Expr where sconcat = Concat
