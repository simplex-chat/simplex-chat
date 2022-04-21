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

styleMarkdown :: Markdown -> StyledString
styleMarkdown = \case 
    Markdown Nothing s -> plain s 
    Markdown (Just Snippet) s -> '`' `wrap` styled Snippet s -- TODO: This can't "undo" outer layers, meaning formatting still gets gorked if this is on the inside 
    Markdown f s -> merge . (<> styleFormat f s) . styleMarkdown . parseMarkdown $ s 
    (s1 :|: s2) -> styleMarkdown s1 <> styleMarkdown s2 

    where 
        merge :: StyledString -> StyledString 
        merge = \case 
            (Styled f1 s :<>: Styled f2 _) -> Styled (f1 ++ f2) s 
            (Styled f s :<>: a) -> (<> Styled f s) . merge $ a 
            sing -> sing

styleMarkdownList :: MarkdownList -> StyledString
styleMarkdownList = \case 
    [] -> plain ""
    [FormattedText f s] -> styleMarkdown (Markdown f s)
    (FormattedText f s : ts) -> (<> styleMarkdown (Markdown f s)) . styleMarkdownList $ ts

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
