{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Markdown where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Bifunctor (second)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Simplex.Messaging.Parsers (fstToLower, sumTypeJSON)
import System.Console.ANSI.Types

data Markdown = Markdown (Maybe Format) Text | Markdown :|: Markdown
  deriving (Eq, Show)

data Format
  = Bold
  | Italic
  | Underline
  | StrikeThrough
  | Snippet
  | Secret
  | Colored FormatColor
  deriving (Eq, Show, Generic)

colored :: Color -> Format
colored = Colored . FormatColor

markdown :: Format -> Text -> Markdown
markdown = Markdown . Just

instance ToJSON Format where toEncoding = J.genericToEncoding $ sumTypeJSON fstToLower

instance Semigroup Markdown where (<>) = (:|:)

instance Monoid Markdown where mempty = unmarked ""

instance IsString Markdown where fromString = unmarked . T.pack

newtype FormatColor = FormatColor Color
  deriving (Eq, Show)

instance ToJSON FormatColor where
  toJSON (FormatColor c) = case c of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"
    Yellow -> "yellow"
    Cyan -> "cyan"
    Magenta -> "magenta"
    Black -> "black"
    White -> "white"

data FormattedText = FormattedText {format :: Maybe Format, text :: Text}
  deriving (Eq, Show, Generic)

instance ToJSON FormattedText where
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

type MarkdownList = [FormattedText]

unmarked :: Text -> Markdown
unmarked = Markdown Nothing

colorMD :: Char
colorMD = '!'

secretMD :: Char
secretMD = '#'

formats :: Map Char Format
formats =
  M.fromList
    [ ('*', Bold),
      ('_', Italic),
      ('+', Underline),
      ('~', StrikeThrough),
      ('`', Snippet),
      (secretMD, Secret),
      (colorMD, colored White)
    ]

colors :: Map Text FormatColor
colors =
  M.fromList . map (second FormatColor) $
    [ ("red", Red),
      ("green", Green),
      ("blue", Blue),
      ("yellow", Yellow),
      ("cyan", Cyan),
      ("magenta", Magenta),
      ("r", Red),
      ("g", Green),
      ("b", Blue),
      ("y", Yellow),
      ("c", Cyan),
      ("m", Magenta),
      ("1", Red),
      ("2", Green),
      ("3", Blue),
      ("4", Yellow),
      ("5", Cyan),
      ("6", Magenta)
    ]

parseMarkdownList :: Text -> MarkdownList
parseMarkdownList = markdownToList . parseMarkdown

markdownToList :: Markdown -> MarkdownList
markdownToList (Markdown f s) = [FormattedText f s]
markdownToList (m1 :|: m2) = markdownToList m1 <> markdownToList m2

parseMarkdown :: Text -> Markdown
parseMarkdown s = fromRight (unmarked s) $ A.parseOnly (markdownP <* A.endOfInput) s

markdownP :: Parser Markdown
markdownP = merge <$> A.many' fragmentP
  where
    merge :: [Markdown] -> Markdown
    merge [] = ""
    merge fs = foldr1 (:|:) fs
    fragmentP :: Parser Markdown
    fragmentP =
      A.anyChar >>= \case
        ' ' -> unmarked . T.cons ' ' <$> A.takeWhile (== ' ')
        c -> case M.lookup c formats of
          Just Secret -> secretP
          Just (Colored (FormatColor White)) -> coloredP
          Just f -> formattedP c "" f
          Nothing -> unformattedP c
    formattedP :: Char -> Text -> Format -> Parser Markdown
    formattedP c p f = do
      s <- A.takeTill (== c)
      (A.char c $> md c p f s) <|> noFormat (c `T.cons` p <> s)
    md :: Char -> Text -> Format -> Text -> Markdown
    md c p f s
      | T.null s || T.head s == ' ' || T.last s == ' ' =
        unmarked $ c `T.cons` p <> s `T.snoc` c
      | otherwise = markdown f s
    secretP :: Parser Markdown
    secretP = secret <$> A.takeWhile (== secretMD) <*> A.takeTill (== secretMD) <*> A.takeWhile (== secretMD)
    secret :: Text -> Text -> Text -> Markdown
    secret b s a
      | T.null a || T.null s || T.head s == ' ' || T.last s == ' ' =
        unmarked $ secretMD `T.cons` ss
      | otherwise = markdown Secret $ T.init ss
      where
        ss = b <> s <> a
    coloredP :: Parser Markdown
    coloredP = do
      color <- A.takeWhile (\c -> c /= ' ' && c /= colorMD)
      case M.lookup color colors of
        Just c ->
          let f = Colored c
           in (A.char ' ' *> formattedP colorMD (color `T.snoc` ' ') f)
                <|> noFormat (colorMD `T.cons` color)
        _ -> noFormat (colorMD `T.cons` color)
    unformattedP :: Char -> Parser Markdown
    unformattedP c = unmarked . T.cons c <$> wordsP
    wordsP :: Parser Text
    wordsP = do
      s <- (<>) <$> A.takeTill (== ' ') <*> A.takeWhile (== ' ')
      A.peekChar >>= \case
        Nothing -> pure s
        Just c -> case M.lookup c formats of
          Just _ -> pure s
          Nothing -> (s <>) <$> wordsP
    noFormat :: Text -> Parser Markdown
    noFormat = pure . unmarked
