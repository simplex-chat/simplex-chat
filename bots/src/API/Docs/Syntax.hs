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

docSyntaxText :: ATUnionMember -> Expr -> Text
docSyntaxText cmd@(ATUnionMember tag _) = T.pack . go Nothing
  where
    go param = \case
      Assign p ex -> paramName param p <> " = " <> go param ex
      Func n p ex -> n <> "(" <> p <> ") = " <> go (Just p) ex
      Call n p -> withParamType cmd param p $ \_ -> "<" <> n <> "(" <> paramName param p <> ")>"
      Concat exs -> concatMap (go param) exs -- TODO validate that subexpressions of type Param have types Int or String
      Param p ->
        withParamType cmd param p $ \case
          ATDef (APITypeDef _ (ATDEnum members)) -> intercalate "|" $ L.toList members
          ATDef (APITypeDef typeName _) -> case find ((typeName ==) . docTypeName) chatTypesDocs of
            Just CTDoc {typeSyntax} | typeSyntax /= "" -> "<cmdString(" <> paramName param p <> ")>"
            _ -> defSyntax
          _ -> defSyntax
        where
          defSyntax = "<" <> paramName param p <> ">"
      Json p ->
        withParamType cmd param p $ \_ -> "<json(" <> paramName param p <> ")>"
      OnOff p -> withBoolParam cmd param p "on|off"
      OnOffParam name p def_
        | null name -> error $ fstToUpper tag <> ": on/off parameter " <> paramName param p <> " has empty name"
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
          ATArray {} -> let n = paramName param p in "<" <> n <> "[0]>[" <> [c] <> "<" <> n <> "[1]>...]"
          _ -> paramError cmd param p "is not array"
      Optional exN exJ p ->
        withParamType cmd param p $ \case
          ATOptional {}
            | exN == "" -> "[" <> go (Just p) exJ <> "]"
            | otherwise -> go param exN <> "|" <> go (Just p) exJ
          _ -> paramError cmd param p "is not optional"
      Const s -> s

paramError :: ATUnionMember -> Maybe ExprParam -> ExprParam -> String -> String
paramError (ATUnionMember tag _) param p err = error $ fstToUpper tag <> ": " <> paramName param p <> " " <> err

withParamType :: ATUnionMember -> Maybe ExprParam -> ExprParam -> (APIType -> String) -> String
withParamType cmd@(ATUnionMember _ params) param p f = case find ((paramName param p ==) . fieldName') params of
  Just APIRecordField {typeInfo} -> f typeInfo
  Nothing -> paramError cmd param p "is unknown"

withBoolParam :: ATUnionMember -> Maybe ExprParam -> ExprParam -> String -> String
withBoolParam cmd param p s =
  withParamType cmd param p $ \case
    ATPrim (PT TBool) -> s
    _ -> paramError cmd param p "is not boolean"

withOptBoolParam :: ATUnionMember -> Maybe ExprParam -> ExprParam -> (Bool -> String) -> String
withOptBoolParam cmd param p f =
  withParamType cmd param p $ \case
    ATPrim (PT TBool) -> f False
    (ATOptional (ATPrim (PT TBool))) -> f True
    _ -> paramError cmd param p "is not [optional] boolean"

jsSyntaxText :: ATUnionMember -> Expr -> Text
jsSyntaxText cmd = T.replace "' + '" "" . T.pack . go Nothing True
  where
    go param top = \case
      Assign p ex -> "let " <> paramName param p <> " = " <> go param top ex
      Func n p ex -> "let " <> n <> " = (" <> p <> ") => " <> go (Just p) top ex
      Call n p -> n <> "(" <> paramName param p <> ")"
      Concat exs -> intercalate " + " $ map (go param False) $ L.toList exs
      Param p ->
        withParamType cmd param p $ \case
          ATDef (APITypeDef typeName _) -> case find ((typeName ==) . docTypeName) chatTypesDocs of
            Just CTDoc {typeSyntax} | typeSyntax /= "" -> "cmdString(" <> paramName param p <> ")"
            _ -> paramName param p
          _ -> paramName param p
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
