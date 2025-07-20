{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Docs.Syntax where

import API.Docs.Types
import API.TypeInfo
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Semigroup
import Data.String
import qualified Data.Text as T
import Simplex.Messaging.Parsers (dropPrefix)

type ExprParam = String -- param name

data Expr where
  Assign :: ExprParam -> Expr -> Expr
  Func :: String -> ExprParam -> Expr -> Expr
  Call :: String -> ExprParam -> Expr
  Concat :: NonEmpty Expr -> Expr
  Param :: ExprParam -> Expr
  Json :: ExprParam -> Expr
  OnOff :: ExprParam -> Expr -- does not include leading space
  OnOffParam :: String -> ExprParam -> Maybe Bool -> Expr -- name, param, default. Includes leading space in all cases. Name must not be empty
  ChatRefExpr :: ExprParam -> Expr
  Join :: Char -> ExprParam -> Expr
  Optional :: Expr -> Expr -> ExprParam -> Expr -- Nothing expr, Just expr (using [$0] as ExprParam), optional param
  Const :: String -> Expr
  deriving (Eq, Show)

isConst :: Expr -> Bool
isConst = \case
  Const _ -> True
  _ -> False

instance IsString Expr where fromString = Const

instance Semigroup Expr where sconcat = Concat

renderDocSyntax :: RecordTypeInfo -> Expr -> String
renderDocSyntax cmd = go Nothing
  where
    go param = \case
      Assign p ex -> paramName param p <> " = " <> go param ex
      Func n p ex -> n <> "(" <> p <> ") = " <> go (Just p) ex
      Call n p -> withParamType cmd param p $ \_ -> "<" <> n <> "(" <> paramName param p <> ")>"
      Concat exs -> concatMap (go param) exs -- TODO validate that subexpressions of type Param have types Int or String
      Param p ->
        withParamType cmd param p $ \case
          (TIType _, Just CTDoc {typeInfo = STI _ constrs, jsonEncoding = Just STEnum, consPrefix})
            | all (\RecordTypeInfo {fieldInfos} -> null fieldInfos) constrs ->
                intercalate "|" $ map (\RecordTypeInfo {consName} -> dropPrefix consPrefix consName) constrs
          _ -> "<" <> paramName param p <> ">"
      Json p ->
        withParamType cmd param p $ \case
          (_, Just CTDoc {jsonEncoding = Nothing}) -> error $ consName' cmd <> ": " <> p <> " has no JSON encoding"
          _ -> "<json(" <> paramName param p <> ")>"
      OnOff p -> withBoolParam cmd param p "on|off"
      OnOffParam name p def_
        | null name -> error $ consName' cmd <> ": on/off parameter " <> paramName param p <> " has empty name"
        | otherwise -> case def_ of
            Just def -> withOptBoolParam cmd param p $ \_ -> "[ " <> name <> "=" <> onOff <> "]"
              where
                onOff = if def then "off" else "on"
            Nothing -> withOptBoolParam cmd param p $ \optional -> if optional then "[" <> res <> "]" else res
              where
                res = " " <> name <> "=on|off"
      ChatRefExpr p -> let n = paramName param p in "@<" <> n <> ".contactId>|#<" <> n <> ".groupId>[(_support[:" <> n <> ".groupMemberId])]"
      Join c p ->
        withParamType cmd param p $ \case
          (TIArray {}, _) -> let n = paramName param p in "<" <> n <> "[0]>[" <> [c] <> "<" <> n <> "[1]>...]"
          _ -> paramError cmd param p "is not array"
      Optional exN exJ p ->
        withParamType cmd param p $ \case
          (TIOptional {}, _)
            | exN == "" -> "[" <> go (Just p) exJ <> "]"
            | otherwise -> go param exN <> "|" <> go (Just p) exJ
          _ -> paramError cmd param p "is not optional"
      Const s -> s

paramError :: RecordTypeInfo -> Maybe ExprParam -> ExprParam -> String -> String
paramError cmd param p err = error $ consName' cmd <> ": " <> paramName param p <> " " <> err

withParamType :: RecordTypeInfo -> Maybe ExprParam -> ExprParam -> ((TypeInfo, Maybe CTDoc) -> String) -> String
withParamType cmd@RecordTypeInfo {fieldInfos = params} param p f = case getParamType (paramName param p) params of
  Left err -> paramError cmd param p err
  Right t -> f t

withBoolParam :: RecordTypeInfo -> Maybe ExprParam -> ExprParam -> String -> String
withBoolParam cmd param p s =
  withParamType cmd param p $ \case
    (TIType (ST TBool _), _) -> s
    _ -> paramError cmd param p "is not boolean"

withOptBoolParam :: RecordTypeInfo -> Maybe ExprParam -> ExprParam -> (Bool -> String) -> String
withOptBoolParam cmd param p f =
  withParamType cmd param p $ \case
    (TIType (ST TBool _), _) -> f False
    (TIOptional (TIType (ST TBool _)), _) -> f True
    _ -> paramError cmd param p "is not [optional] boolean"

getParamType :: ExprParam -> [FieldInfo] -> Either String (TypeInfo, Maybe CTDoc)
getParamType p params = case find ((p ==) . fieldName) params of
  Nothing -> Left "is unknown"
  Just FieldInfo {typeInfo} -> getTypeInfoDoc typeInfo
  where
    getTypeInfoDoc tInfo = case tInfo of
      TIType (ST t _) -> getTypeDoc tInfo t
      TIOptional (TIType (ST t _)) -> getTypeDoc tInfo t
      TIArray {elemType = TIType (ST t _)} -> getTypeDoc tInfo t
      TIMap {valueType = TIType (ST t _)} -> getTypeDoc tInfo t
      _ -> Right (tInfo, Nothing)
    getTypeDoc tInfo t
      | t `elem` primitiveTypes = Right (tInfo, Nothing)
      | otherwise = case find ((t ==) . docTypeName) chatTypesDocs of
          Nothing -> Left $ "has unknown type " <> t
          Just d -> Right (tInfo, Just d)

renderJSSyntax :: RecordTypeInfo -> Expr -> String
renderJSSyntax cmd = T.unpack . T.replace "' + '" "" . T.pack . go Nothing True
  where
    go param top = \case
      Assign p ex -> "let " <> paramName param p <> " = " <> go param top ex
      Func n p ex -> "let " <> n <> " = (" <> p <> ") => " <> go (Just p) top ex
      Call n p -> n <> "(" <> paramName param p <> ")"
      Concat exs -> intercalate " + " $ map (go param False) $ L.toList exs
      Param p -> paramName param p
      Json p -> "JSON.stringify(" <> paramName param p <> ")"
      OnOff p -> open <> paramName param p <> " ? 'on' : 'off'" <> close
      OnOffParam name p def_ -> case def_ of
        Nothing ->
          withOptBoolParam cmd param p $ \optional ->
            if optional
              then "(typeof " <> n <> " == 'boolean' ? " <> res <> " : '')"
              else res
          where
            n = paramName param p
            res = "' " <> name <> "=' + (" <> n <> " ? 'on' : 'off')"
        Just def
          | def -> open <> "!" <> n <> " ? ' " <> name <> "=off' : ''" <> close
          | otherwise -> open <> n <> " ? ' " <> name <> "=on' : ''" <> close
          where
            n = paramName param p
      ChatRefExpr p -> let n = paramName param p in open <> n <> ".contactId ? `@${" <> n <> ".contactId}` : `#${" <> n <> ".groupId}` + (" <> n <> ".scope ? '(_support' + (" <> n <> ".scope.groupMemberId ? `:${" <> n <> ".scope.groupMemberId}` : '') + ')' : '')" <> close
      Join c p -> let n = paramName param p in n <> ".join('" <> [c] <> "')"
      Optional exN exJ p -> open <> n <> " ? " <> go (Just p) False exJ <> " : " <> nothing <> close
        where
          n = paramName param p
          nothing = if exN == "" then "''" else go param False exN
      Const s -> "'" <> escape '\'' s <> "'"
      where
        open = if top then "" else "("
        close = if top then "" else ")"
    escape c s
      | c `elem` s = concatMap (\c' -> if c' == c then ['\\', c] else [c]) s
      | otherwise = s

paramName :: Maybe ExprParam -> ExprParam -> String
paramName param_ p = case param_ of
  Just param | p == "$0" -> param
  _ -> p

-- validateExpr :: [FieldInfo] -> [String] -> Expr -> Maybe String
-- validateExpr params funcs = \case
--   Assign {} -> Nothing
--   Func {} -> Nothing
--   Call {} -> Nothing
--   Concat _ -> Nothing
--   Param p -> case getParamType p params of
--     Left e -> Just e
--     Right _t -> Nothing
--       -- | t == "String" -> Nothing
--       -- | otherwise -> Just $ "unknown parameter: " <> p
--   Json _ -> Nothing -- validate
--   OnOff _p -> Nothing -- validate
--   ChatRefExpr _p -> Nothing -- validate
--   Join {} -> Nothing -- validate
--   Optional {} -> Nothing -- validate
--   Const _ -> Nothing
