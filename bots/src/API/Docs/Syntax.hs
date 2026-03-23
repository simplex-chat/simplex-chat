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
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

docSyntaxText :: TypeAndFields -> Expr -> Text
docSyntaxText r@(tag, _) = T.pack . go Nothing
  where
    go param = \case
      Concat exs -> concatMap (go param) exs
      Const s -> s
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
      Optional exN exJ p ->
        withParamType r param p $ \case
          ATOptional {}
            | exN == "" -> "[" <> go (Just p) exJ <> "]"
            | otherwise -> go param exN <> "|" <> go (Just p) exJ
          _ -> paramError r param p "is not optional"
      Choice p opts else' ->
        withParamType r param p $ \case
          ATDef td -> choiceSyntax td
          ATOptional (ATDef td) -> choiceSyntax td
          _ -> paramError r param p "is not union type"
        where
          choiceSyntax = \case
            APITypeDef _ (ATDUnion _) -> choices
            APITypeDef _ (ATDEnum _) -> choices
            _ -> paramError r param p "is not union or enum type"
          choices = (if null optsSyntax then "" else optsSyntax <> "|") <> go param else'
            where
              optsSyntax = intercalate "|" (mapMaybe ((\s -> if null s then Nothing else Just s) . go param . snd) (L.toList opts))
      Join c p ->
        withParamType r param p $ \case
          ATArray {} -> let n = paramName param p in "<" <> n <> "[0]>[" <> [c] <> "<" <> n <> "[1]>...]"
          _ -> paramError r param p "is not array"
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

jsSyntaxText :: Bool -> String -> TypeAndFields -> Expr -> Text
jsSyntaxText useSelf typeNamespace r = T.replace "' + '" "" . T.pack . go Nothing True
  where
    go param top = \case
      Concat exs -> intercalate " + " $ map (go param False) $ L.toList exs
      Const s -> "'" <> escapeChar '\'' s <> "'"
      Param p ->
        withParamType r param p $ \case
          ATDef td -> toStringSyntax td
          ATOptional (ATDef td) -> toStringSyntax td
          _ -> paramName' useSelf param p
        where
          toStringSyntax (APITypeDef typeName _)
            | typeHasSyntax typeName = typeNamespace <> typeName <> ".cmdString(" <> paramName' useSelf param p <> ")"
            | otherwise = paramName' useSelf param p
      Optional exN exJ p -> open <> n <> " ? " <> go (Just p) False exJ <> " : " <> nothing <> close
        where
          n = paramName' useSelf param p
          nothing = if exN == "" then "''" else go param False exN
      Choice p opts else' ->
        withParamType r param p $ \case
          ATDef td -> choiceSyntax td
          ATOptional (ATDef td) -> choiceSyntax td
          _ -> paramError r param p "is not union type"
        where
          choiceSyntax = \case
            APITypeDef _ (ATDUnion _) -> choices $ (if useSelf then "self." else "") <> "type"
            APITypeDef _ (ATDEnum _) -> choices "self"
            _ -> paramError r param p "is not union type"
          choices var = open <> optsSyntax <> " : " <> go param top else' <> close
            where
              optsSyntax = intercalate " : " $ map (\(tag, ex) -> var <> " == '" <> tag <> "' ? " <> go param top ex) $ L.toList opts
      Join c p -> paramName' useSelf param p <> ".join('" <> [c] <> "')"
      Json p -> "JSON.stringify(" <> paramName' useSelf param p <> ")"
      OnOff p -> open <> paramName' useSelf param p <> " ? 'on' : 'off'" <> close
      OnOffParam name p def_ -> case def_ of
        Nothing ->
          withOptBoolParam r param p $ \optional ->
            if optional
              then "(typeof " <> n <> " == 'boolean' ? " <> res <> " : '')"
              else res
          where
            n = paramName' useSelf param p
            res = "' " <> name <> "=' + (" <> n <> " ? 'on' : 'off')"
        Just def
          | def -> open <> "!" <> n <> " ? ' " <> name <> "=off' : ''" <> close
          | otherwise -> open <> n <> " ? ' " <> name <> "=on' : ''" <> close
          where
            n = paramName' useSelf param p
      where
        open = if top then "" else "("
        close = if top then "" else ")"

escapeChar :: Char -> String -> String
escapeChar c s
  | c `elem` s = concatMap (\c' -> if c' == c then ['\\', c] else [c]) s
  | otherwise = s

pySyntaxText :: TypeAndFields -> Expr -> Text
pySyntaxText r = T.pack . go Nothing True
  where
    go param top = \case
      Concat exs -> intercalate " + " $ map (go param False) $ L.toList exs
      Const s -> "'" <> escapeChar '\'' s <> "'"
      Param p ->
        withParamType r param p $ \case
          ATPrim (PT TString) -> paramName param p
          ATOptional (ATPrim (PT TString)) -> paramName param p
          _ -> "str(" <> paramName param p <> ")"
      Optional exN exJ p -> open <> "(" <> go (Just p) False exJ <> ") if " <> n <> " is not None else " <> nothing <> close
        where
          n = paramName param p
          nothing = if exN == "" then "''" else go param False exN
      Choice p opts else' ->
        withParamType r param p $ \case
          ATDef td -> choiceSyntax td
          ATOptional (ATDef td) -> choiceSyntax td
          _ -> paramError r param p "is not union type"
        where
          choiceSyntax = \case
            APITypeDef _ (ATDUnion _) -> choices "type"
            APITypeDef _ (ATDEnum _) -> choices "self"
            _ -> paramError r param p "is not union type"
          choices var = open <> optsSyntax <> " else " <> go param top else' <> close
            where
              optsSyntax = intercalate " else " $ map (\(tag, ex) -> go param top ex <> " if " <> var' <> " == '" <> tag <> "'") $ L.toList opts
              var' =
                withParamType r param var $ \case
                  ATPrim (PT TString) -> var
                  ATOptional (ATPrim (PT TString)) -> var
                  _ -> "str(" <> var <> ")"
      Join c p ->
        withParamType r param p $ \case
          ATArray {elemType = ATPrim (PT TString)} -> "'" <> [c] <> "'.join(" <> paramName param p <> ")"
          _ -> "'" <> [c] <> "'.join(map(str, " <> paramName param p <> "))"
      Json p -> "json.dumps(" <> paramName param p <> ")"
      OnOff p -> open <> "'on' if " <> paramName param p <> " else 'off'" <> close
      OnOffParam name p def_ -> case def_ of
        Nothing ->
          withOptBoolParam r param p $ \optional ->
            if optional
              then "((" <> res <> ") if " <> n <> " is not None else '')"
              else res
          where
            n = paramName param p
            res = "' " <> name <> "=' + ('on' if " <> n <> " else 'off')"
        Just def
          | def -> open <> "' " <> name <> "=off' if not " <> n <> " else ''" <> close
          | otherwise -> open <> "' " <> name <> "=on' if " <> n <> " else ''" <> close
          where
            n = paramName param p
      where
        open = if top then "" else "("
        close = if top then "" else ")"

paramName :: Maybe ExprParam -> ExprParam -> String
paramName = paramName' False

paramName' :: Bool -> Maybe ExprParam -> ExprParam -> String
paramName' useSelf param_ p
  | useSelf && p' /= "self" = "self." <> p'
  | otherwise = p'
  where
    p' = case param_ of
      Just param | p == "$0" -> param
      _ -> p
