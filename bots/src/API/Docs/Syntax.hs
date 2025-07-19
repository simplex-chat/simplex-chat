{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Docs.Syntax where

import API.TypeInfo
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Semigroup
import Data.String

type ExprParam = String -- param name

data Expr where
  Concat :: NonEmpty Expr -> Expr
  Param :: ExprParam -> Expr
  Json :: ExprParam -> Expr
  OnOff :: ExprParam -> Expr
  ChatRefExpr :: ExprParam -> Expr
  EMaybe :: Expr -> Expr -> ExprParam -> Expr -- Nothing expr, Just expr (using [$0] as ExprParam), optional param
  Const :: String -> Expr
  deriving (Eq, Show)

isConst :: Expr -> Bool
isConst = \case
  Const _ -> True
  _ -> False

instance IsString Expr where fromString = Const

instance Semigroup Expr where sconcat = Concat

renderDocSyntax :: Expr -> String
renderDocSyntax = go Nothing
  where
    go param = \case
      Concat exs -> concatMap (go param) exs
      Param p -> "<" <> paramName param p <> ">"
      Json p -> "<json(" <> paramName param p <> ")>"
      OnOff _ -> "on|off"
      ChatRefExpr p -> let n = paramName param p in "@<" <> n <> ".contactId>|#<" <> n <> ".groupId>[(_support[:" <> n <> ".groupMemberId])]"
      EMaybe exN exJ p
        | exN == "" -> "[" <> go (Just p) exJ <> "]"
        | otherwise -> go Nothing exN <> "|" <> go (Just p) exJ
      Const s -> s

renderJSSyntax :: Expr -> String
renderJSSyntax = go Nothing
  where
    go param = \case
      Concat exs -> intercalate " + " $ map (go param) $ L.toList exs
      Param p -> paramName param p
      Json p -> "JSON.stringify(" <> paramName param p <> ")"
      OnOff p -> "(" <> paramName param p <> " ? 'on' : 'off')"
      ChatRefExpr p -> let n = paramName param p in "(" <> n <> ".contactId ? '@' + " <> n <> ".contactId : '#' + " <> n <> ".groupId + (" <> n <> ".scope ? '(_support' + (n.scope.groupMemberId ? ':' + n.scope.groupMemberId : '') + ')' : ''))"
      EMaybe exN exJ p
        | exN == "" -> "(!" <> n <> " || " <> n <> " == '' ? '' : " <> go (Just p) exJ <> ")"
        | otherwise -> "(!" <> n <> " ? " <> go Nothing exN <> " : " <> go (Just p) exJ <> ")"
        where
          n = paramName param p
      Const s -> "'" <> escape '\'' s
    escape c s
      | c `elem` s = concatMap (\c' -> if c' == c then ['\\', c] else [c]) s
      | otherwise = s

paramName :: Maybe ExprParam -> ExprParam -> String
paramName param_ p = case param_ of
  Just param | p == "$0" -> param
  _ -> p

validateExpr :: RecordTypeInfo -> [SumTypeInfo] -> [String] -> Expr -> Maybe String
validateExpr RecordTypeInfo {fieldInfos} _types funcs = \case
  Concat _ -> Nothing
  Param p -> case getParamType p of
    Left e -> Just e
    Right t
      | t == "String" -> Nothing
      | otherwise -> Just $ "unknown parameter: " <> p
  Json _ -> Nothing -- validate
  OnOff _p -> Nothing -- validate
  ChatRefExpr _p -> Nothing -- validate
  EMaybe {} -> Nothing -- validate
  Const _ -> Nothing
  where
    getParamType :: ExprParam -> Either String String
    getParamType = undefined
