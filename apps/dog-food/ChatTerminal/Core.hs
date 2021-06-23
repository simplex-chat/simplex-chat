{-# LANGUAGE OverloadedStrings #-}

module ChatTerminal.Core where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Simplex.Chat.Markdown
import Simplex.Chat.Styled
import System.Console.ANSI.Types

styleMessage :: String -> String -> StyledString
styleMessage time msg = do
  case msg of
    "" -> ""
    s@('@' : _) -> sentMessage s
    s@('#' : _) -> sentMessage s
    s -> markdown s
  where
    sentMessage :: String -> StyledString
    sentMessage s =
      let (c, rest) = span (/= ' ') s
       in styleTime time <> " " <> styled (Colored Cyan) c <> markdown rest
    markdown :: String -> StyledString
    markdown = styleMarkdownText . T.pack

styleTime :: String -> StyledString
styleTime = Styled [SetColor Foreground Vivid Black]

safeDecodeUtf8 :: ByteString -> Text
safeDecodeUtf8 = decodeUtf8With onError
  where
    onError _ _ = Just '?'
