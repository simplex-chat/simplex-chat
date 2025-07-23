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

import API.Docs.Syntax.Types
import API.Docs.Types
import API.TypeInfo
import Data.List (find, intercalate)
import qualified Data.List.NonEmpty as L
import Data.Text (Text)
import qualified Data.Text as T

docSyntaxText :: TypeAndFields -> Expr -> Text
docSyntaxText r@(tag, _) = T.pack . go Nothing
  where
    go param = \case
      Concat exs -> concatMap (go param) exs -- TODO validate that subexpressions of type Param have types Int or String
      Param p ->
        withParamType r param p $ \case
          ATDef td -> strSyntax td
          ATOptional (ATDef td) -> strSyntax td
          _ -> defSyntax Nothing
        where
          strSyntax (APITypeDef typeName t)
            | typeHasSyntax typeName = "<str(" <> paramName param p <> ")>"
            | otherwise = defSyntax (Just t)
          defSyntax = \case
            Just (ATDEnum ms) -> intercalate "|" $ L.toList ms
            _ -> "<" <> paramName param p <> ">"
      Json p ->
        withParamType r param p $ \_ -> "<json(" <> paramName param p <> ")>"
      OnOff p -> withBoolParam r param p "on|off"
      OnOffParam name p def_
        | null name -> error $ fstToUpper tag <> ": on/off parameter " <> paramName param p <> " has empty name"
        | otherwise -> case def_ of
            Just def -> withOptBoolParam r param p $ \_ -> "[ " <> name <> "=" <> onOff <> "]"
              where
                onOff = if def then "off" else "on"
            Nothing -> withOptBoolParam r param p $ \optional -> if optional then "[" <> res <> "]" else res
              where
                res = " " <> name <> "=on|off"
      Join c p ->
        withParamType r param p $ \case
          ATArray {} -> let n = paramName param p in "<" <> n <> "[0]>[" <> [c] <> "<" <> n <> "[1]>...]"
          _ -> paramError r param p "is not array"
      Optional exN exJ p ->
        withParamType r param p $ \case
          ATOptional {}
            | exN == "" -> "[" <> go (Just p) exJ <> "]"
            | otherwise -> go param exN <> "|" <> go (Just p) exJ
          _ -> paramError r param p "is not optional"
      Const s -> s

typeHasSyntax :: String -> Bool
typeHasSyntax typeName = case find ((typeName ==) . docTypeName) chatTypesDocs of
  Just CTDoc {typeSyntax} -> typeSyntax /= ""
  _ -> False

paramError :: TypeAndFields -> Maybe ExprParam -> ExprParam -> String -> String
paramError (tag, _) param p err = error $ fstToUpper tag <> ": " <> paramName param p <> " " <> err

withParamType :: TypeAndFields -> Maybe ExprParam -> ExprParam -> (APIType -> String) -> String
withParamType r@(_, params) param p f = case find ((paramName param p ==) . fieldName') params of
  Just APIRecordField {typeInfo} -> f typeInfo
  Nothing -> paramError r param p "is unknown"

withBoolParam :: TypeAndFields -> Maybe ExprParam -> ExprParam -> String -> String
withBoolParam r param p s =
  withParamType r param p $ \case
    ATPrim (PT TBool) -> s
    _ -> paramError r param p "is not boolean"

withOptBoolParam :: TypeAndFields -> Maybe ExprParam -> ExprParam -> (Bool -> String) -> String
withOptBoolParam r param p f =
  withParamType r param p $ \case
    ATPrim (PT TBool) -> f False
    (ATOptional (ATPrim (PT TBool))) -> f True
    _ -> paramError r param p "is not [optional] boolean"

jsSyntaxText :: TypeAndFields -> Expr -> Text
jsSyntaxText r = T.replace "' + '" "" . T.pack . go Nothing True
  where
    go param top = \case
      Concat exs -> intercalate " + " $ map (go param False) $ L.toList exs
      Param p ->
        withParamType r param p $ \case
          ATDef td -> toStringSyntax td
          ATOptional (ATDef td) -> toStringSyntax td
          _ -> paramName param p
        where
          toStringSyntax (APITypeDef typeName _)
            | typeHasSyntax typeName = paramName param p <> ".toString()"
            | otherwise = paramName param p
      Json p -> "JSON.stringify(" <> paramName param p <> ")"
      OnOff p -> open <> paramName param p <> " ? 'on' : 'off'" <> close
      OnOffParam name p def_ -> case def_ of
        Nothing ->
          withOptBoolParam r param p $ \optional ->
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
