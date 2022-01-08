{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Simplex.Chat.Protocol.Legacy where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import "simplexmq" Simplex.Messaging.Util (bshow)

data RawChatMessage = RawChatMessage
  { chatMsgEvent :: ByteString,
    chatMsgParams :: [ByteString],
    chatMsgBody :: [RawMsgBodyContent]
  }
  deriving (Eq, Show)

data RawMsgBodyContent = RawMsgBodyContent
  { contentType :: RawContentType,
    contentData :: ByteString
  }
  deriving (Eq, Show)

data RawContentType = RawContentType NameSpace ByteString
  deriving (Eq, Show)

type NameSpace = ByteString

rawChatMessageP :: Parser RawChatMessage
rawChatMessageP = do
  _ <- A.takeTill (== ' ') <* A.space
  chatMsgEvent <- B.intercalate "." <$> identifierP `A.sepBy1'` A.char '.' <* A.space
  chatMsgParams <- A.takeWhile1 (not . A.inClass ", ") `A.sepBy'` A.char ',' <* A.space
  chatMsgBody <- msgBodyContent =<< contentInfoP `A.sepBy'` A.char ',' <* A.space
  pure RawChatMessage {chatMsgEvent, chatMsgParams, chatMsgBody}
  where
    msgBodyContent :: [(RawContentType, Int)] -> Parser [RawMsgBodyContent]
    msgBodyContent [] = pure []
    msgBodyContent ((contentType, size) : ps) = do
      contentData <- A.take size <* A.space
      ((RawMsgBodyContent {contentType, contentData}) :) <$> msgBodyContent ps

contentInfoP :: Parser (RawContentType, Int)
contentInfoP = do
  contentType <- RawContentType <$> identifierP <* A.char '.' <*> A.takeTill (A.inClass ":, ")
  size <- A.char ':' *> A.decimal
  pure (contentType, size)

identifierP :: Parser ByteString
identifierP = B.cons <$> A.letter_ascii <*> A.takeWhile (\c -> A.isAlpha_ascii c || A.isDigit c)

serializeRawChatMessage :: RawChatMessage -> ByteString
serializeRawChatMessage RawChatMessage {chatMsgEvent, chatMsgParams, chatMsgBody} =
  B.unwords
    [ "",
      chatMsgEvent,
      B.intercalate "," chatMsgParams,
      B.unwords $ map serializeBodyContentInfo chatMsgBody,
      B.unwords $ map msgContentData chatMsgBody
    ]

serializeBodyContentInfo :: RawMsgBodyContent -> ByteString
serializeBodyContentInfo RawMsgBodyContent {contentType = t, contentData} =
  serializeContentInfo (t, B.length contentData)

serializeContentInfo :: (RawContentType, Int) -> ByteString
serializeContentInfo (RawContentType ns cType, size) = ns <> "." <> cType <> ":" <> bshow size

msgContentData :: RawMsgBodyContent -> ByteString
msgContentData RawMsgBodyContent {contentData} = contentData <> " "
