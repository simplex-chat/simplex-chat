{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Styled
  ( StyledString (..),
    StyledFormat (..),
    styleMarkdown,
    styleMarkdownList,
    unStyle,
    sLength,
    sShow,
    sTake,
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

instance Monoid StyledString where mempty = ""

instance IsString StyledString where fromString = plain

styleMarkdown :: Markdown -> StyledString
styleMarkdown (s1 :|: s2) = styleMarkdown s1 <> styleMarkdown s2
styleMarkdown (Markdown f s) = styleFormat f s

styleMarkdownList :: MarkdownList -> StyledString
styleMarkdownList [] = ""
styleMarkdownList [FormattedText f s] = styleFormat f s
styleMarkdownList (FormattedText f s : ts) = styleFormat f s <> styleMarkdownList ts

styleFormat :: Maybe Format -> Text -> StyledString
styleFormat (Just Snippet) s = '`' `wrap` styled Snippet s
styleFormat (Just Secret) s = '#' `wrap` styled Secret s
styleFormat (Just f) s = styled f s
styleFormat Nothing s = plain s

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

sTake :: Int -> StyledString -> StyledString
sTake n = go Nothing 0
  where
    go res len = \case
      Styled f s ->
        let s' = Styled f $ take (n - len) s
         in maybe id (<>) res s'
      s1 :<>: s2 ->
        let s1' = go res len s1
            len' = sLength s1'
         in if len' >= n then s1' else go (Just s1') len' s2
