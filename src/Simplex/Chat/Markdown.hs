{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Markdown where

import Control.Applicative (optional, (<|>))
import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Simplex.Messaging.Parsers (fstToLower, sumTypeJSON)
import System.Console.ANSI.Types
import qualified Text.Email.Validate as Email

data Markdown = Markdown (Maybe Format) Text | Markdown :|: Markdown
  deriving (Eq, Show)

data Format
  = Bold
  | Italic
  | StrikeThrough
  | Snippet
  | Secret
  | Colored {color :: FormatColor}
  | Uri
  | Email
  | Phone
  deriving (Eq, Show, Generic)

colored :: Color -> Format
colored = Colored . FormatColor

markdown :: Format -> Text -> Markdown
markdown = Markdown . Just

instance ToJSON Format where toEncoding = J.genericToEncoding $ sumTypeJSON fstToLower

instance Semigroup Markdown where
  m <> (Markdown _ "") = m
  (Markdown _ "") <> m = m
  m1@(Markdown f1 s1) <> m2@(Markdown f2 s2)
    | f1 == f2 = Markdown f1 $ s1 <> s2
    | otherwise = m1 :|: m2
  m1@(Markdown f1 s1) <> ms@(Markdown f2 s2 :|: m3)
    | f1 == f2 = Markdown f1 (s1 <> s2) :|: m3
    | otherwise = m1 :|: ms
  ms@(m1 :|: Markdown f2 s2) <> m3@(Markdown f3 s3)
    | f2 == f3 = m1 :|: Markdown f2 (s2 <> s3)
    | otherwise = ms :|: m3
  m1 <> m2 = m1 :|: m2

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

parseMaybeMarkdownList :: Text -> Maybe MarkdownList
parseMaybeMarkdownList s =
  let m = markdownToList $ parseMarkdown s
   in if all (isNothing . format) m then Nothing else Just m

parseMarkdownList :: Text -> MarkdownList
parseMarkdownList = markdownToList . parseMarkdown

markdownToList :: Markdown -> MarkdownList
markdownToList (Markdown f s) = [FormattedText f s]
markdownToList (m1 :|: m2) = markdownToList m1 <> markdownToList m2

parseMarkdown :: Text -> Markdown
parseMarkdown s = fromRight (unmarked s) $ A.parseOnly (markdownP <* A.endOfInput) s

markdownP :: Parser Markdown
markdownP = mconcat <$> A.many' fragmentP
  where
    fragmentP :: Parser Markdown
    fragmentP =
      A.peekChar >>= \case
        Just ' ' -> unmarked <$> A.takeWhile (== ' ')
        Just '+' -> phoneP <|> wordP
        Just c
          | isDigit c -> phoneP <|> wordP
          | otherwise -> case M.lookup c formats of
            Just Secret -> A.char secretMD *> secretP
            Just (Colored (FormatColor White)) -> A.char colorMD *> coloredP
            Just f -> A.char c *> formattedP c "" f
            Nothing -> wordP
        Nothing -> fail ""
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
      cStr <- A.takeWhile (\c -> c /= ' ' && c /= colorMD)
      case M.lookup cStr colors of
        Just c ->
          let f = Colored c
           in (A.char ' ' *> formattedP colorMD (cStr `T.snoc` ' ') f)
                <|> noFormat (colorMD `T.cons` cStr)
        _ -> noFormat (colorMD `T.cons` cStr)
    phoneP = do
      country <- optional $ T.cons <$> A.char '+' <*> A.takeWhile1 isDigit
      code <- optional $ conc4 <$> phoneSep <*> "(" <*> A.takeWhile1 isDigit <*> ")"
      segments <- mconcat <$> A.many' ((<>) <$> phoneSep <*> A.takeWhile1 isDigit)
      let s = fromMaybe "" country <> fromMaybe "" code <> segments
          len = T.length s
      if 7 <= len && len <= 22 then pure $ markdown Phone s else fail "not phone"
    conc4 s1 s2 s3 s4 = s1 <> s2 <> s3 <> s4
    phoneSep = " " <|> "-" <|> "." <|> ""
    wordP :: Parser Markdown
    wordP = wordMD <$> A.takeTill (== ' ')
    wordMD :: Text -> Markdown
    wordMD s
      | T.null s = unmarked s
      | isUri s = markdown Uri s
      | isEmail s = markdown Email s
      | otherwise = unmarked s
    isUri s = "http://" `T.isPrefixOf` s || "https://" `T.isPrefixOf` s || "simplex:/" `T.isPrefixOf` s
    isEmail s = T.any (== '@') s && Email.isValid (encodeUtf8 s)
    noFormat = pure . unmarked
