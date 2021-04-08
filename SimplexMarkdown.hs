{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SimplexMarkdown where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Styled
import System.Console.ANSI.Types

data Markdown = Markdown Format Text | Markdown :|: Markdown
  deriving (Show)

data Format
  = Bold
  | Italic
  | Underline
  | StrikeThrough
  | Colored Color
  | NoFormat
  deriving (Show)

instance Semigroup Markdown where (<>) = (:|:)

instance Monoid Markdown where mempty = unmarked ""

instance IsString Markdown where fromString = unmarked . T.pack

unmarked :: Text -> Markdown
unmarked = Markdown NoFormat

styleMarkdown :: Markdown -> StyledString
styleMarkdown (s1 :|: s2) = styleMarkdown s1 <> styleMarkdown s2
styleMarkdown (Markdown f s) = Styled sgr $ T.unpack s
  where
    sgr = case f of
      Bold -> [SetConsoleIntensity BoldIntensity]
      Italic -> [SetUnderlining SingleUnderline, SetItalicized True]
      Underline -> [SetUnderlining SingleUnderline]
      StrikeThrough -> [SetSwapForegroundBackground True]
      Colored c -> [SetColor Foreground Vivid c]
      NoFormat -> []

formats :: Map Char Format
formats =
  M.fromList
    [ ('*', Bold),
      ('_', Italic),
      ('+', Underline),
      ('~', StrikeThrough),
      ('^', Colored White)
    ]

colors :: Map Text Color
colors =
  M.fromList
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
      ("m", Magenta)
    ]

parseMarkdown :: Text -> Markdown
parseMarkdown s = fromRight (unmarked s) $ A.parseOnly (markdownP <* A.endOfInput) s

markdownP :: Parser Markdown
markdownP = merge <$> A.many' fragmentP
  where
    merge :: [Markdown] -> Markdown
    merge [] = ""
    merge [f] = f
    merge (f : fs) = foldl (:|:) f fs
    fragmentP :: Parser Markdown
    fragmentP =
      A.anyChar >>= \case
        ' ' -> unmarked . (" " <>) <$> A.takeWhile (== ' ')
        c -> case M.lookup c formats of
          Just (Colored White) -> coloredP
          Just f -> formattedP c "" f
          Nothing -> unformattedP c
    formattedP :: Char -> Text -> Format -> Parser Markdown
    formattedP c p f = do
      s <- A.takeTill (== c)
      (A.char c $> Markdown f s) <|> noFormat (T.singleton c <> p <> s)
    coloredP :: Parser Markdown
    coloredP = do
      color <- A.takeWhile (\c -> c /= ' ' && c /= '^')
      case M.lookup color colors of
        Just c ->
          let f = Colored c
           in (A.char ' ' *> formattedP '^' (color <> " ") f)
                <|> (A.char '^' $> Markdown f color)
                <|> noFormat ("^" <> color)
        _ -> noFormat ("^" <> color)
    unformattedP :: Char -> Parser Markdown
    unformattedP c = unmarked . (T.singleton c <>) <$> wordsP
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
