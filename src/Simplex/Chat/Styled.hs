{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Simplex.Chat.Styled
  ( StyledString (..),
    StyledFormat (..),
    styleMarkdown,
    styleMarkdownList,
    unStyle,
    sLength,
    sShow,
  )
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.Markdown
import System.Console.ANSI.Types

data StyledString = Styled [SGR] String | StyledString :<>: StyledString
  deriving (Show)

instance Semigroup StyledString where (<>) = (:<>:)

instance Monoid StyledString where mempty = plain ""

instance IsString StyledString where fromString = plain

data SingleStyled = SStyled [SGR] String

type MaybeSnippet = Either StyledString SingleStyled

styleMarkdown :: Markdown -> StyledString
styleMarkdown = \case
  (a :|: b) -> styleMarkdown a <> styleMarkdown b
  Markdown f t -> inlineSnippets . styleSMarkdown $ FormattedText f t
  where
    breakApart :: Markdown -> [FormattedText]
    breakApart = \case
      Markdown f s -> [FormattedText f s]
      a :|: b -> breakApart a ++ breakApart b

    unify :: MaybeSnippet -> StyledString
    unify = \case
      Left a -> a
      Right (SStyled a b) -> Styled a b

    inlineSnippets :: [MaybeSnippet] -> StyledString
    inlineSnippets = foldr (\nxt acc -> (<> acc) . unify $ nxt) (plain "")

    inherit :: [SGR] -> MaybeSnippet -> MaybeSnippet
    inherit f1 = \case
      Left snippet -> Left snippet
      Right (SStyled f2 s) -> Right $ SStyled (f1 ++ f2) s

    inheritAll :: [SGR] -> Markdown -> [MaybeSnippet]
    inheritAll f = concatMap (map (inherit f) . styleSMarkdown) . breakApart

    styleSMarkdown :: FormattedText -> [MaybeSnippet]
    styleSMarkdown = \case
      FormattedText (Just Snippet) s -> (: []) . Left $ '`' `wrap` styled Snippet s
      FormattedText (Just Secret) s -> (:) (Right $ SStyled [] "#") . (++ [Right $ SStyled [] "#"]) . inheritAll (sgr Secret) . parseMarkdown $ s
      FormattedText (Just f) s -> inheritAll (sgr f) . parseMarkdown $ s
      FormattedText Nothing s -> (: []) . Right . SStyled [] . T.unpack $ s

styleMarkdownList :: MarkdownList -> StyledString
styleMarkdownList = \case
  [] -> plain ""
  [FormattedText f s] -> styleMarkdown (Markdown f s)
  (FormattedText f s : ts) -> (<>) (styleMarkdown (Markdown f s)) . styleMarkdownList $ ts

wrap :: Char -> StyledString -> StyledString
wrap c s = plain [c] <> s <> plain [c]

class StyledFormat a where
  styled :: Format -> a -> StyledString
  plain :: a -> StyledString

instance StyledFormat String where
  styled = Styled . sgr
  plain = Styled []

instance StyledFormat ByteString where
  styled f = styled f . B.unpack
  plain = Styled [] . B.unpack

instance StyledFormat Text where
  styled f = styled f . T.unpack
  plain = Styled [] . T.unpack

sShow :: Show a => a -> StyledString
sShow = plain . show

sgr :: Format -> [SGR]
sgr = \case
  Bold -> [SetConsoleIntensity BoldIntensity]
  Italic -> [SetUnderlining SingleUnderline, SetItalicized True]
  StrikeThrough -> [SetSwapForegroundBackground True]
  Colored (FormatColor c) -> [SetColor Foreground Vivid c]
  Secret -> [SetColor Foreground Dull Black, SetColor Background Dull Black]
  _ -> []

unStyle :: StyledString -> String
unStyle (Styled _ s) = s
unStyle (s1 :<>: s2) = unStyle s1 <> unStyle s2

sLength :: StyledString -> Int
sLength (Styled _ s) = length s
sLength (s1 :<>: s2) = sLength s1 + sLength s2
