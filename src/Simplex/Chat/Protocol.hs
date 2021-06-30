{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Protocol where

import Control.Applicative (optional, (<|>))
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Parsers (base64P)

type ChatTransmission = (MsgMeta, ChatMessage)

data ChatMessage = DirectChatMsg Contact ContentMsg

newtype ContentMsg = NewContentMsg ContentData

newtype ContentData = ContentText Text

data RawChatMessage
  = RawChatMessage
      { chatMsgId :: Maybe Int64,
        chatMsgEvent :: ChatMsgEvent,
        chatMsgParams :: [ByteString],
        chatMsgBody :: [MsgBodyContent]
      }
  | RawChatMsgContinuation
      { prevChatMsgId :: Int64,
        continuationId :: Int,
        continuationData :: ByteString
      }
  deriving (Eq, Show)

data ChatMsgEvent
  = SimplexChatMsgEvent (NonEmpty ByteString)
  | ChatMsgEvent NameSpace (NonEmpty ByteString)
  deriving (Eq, Show)

data MsgBodyContent = MsgBodyContent
  { contentType :: ContentType,
    contentHash :: Maybe ByteString,
    contentData :: MsgBodyPartData
  }
  deriving (Eq, Show)

data ContentType
  = ContentType NameSpace ByteString -- unknown namespace
  | MimeContentType ByteString -- i. namespace for MIME content type
  | ChannelContentType ByteString -- c. namespace for SimpleX channel content type
  | SimplexContentType ByteString -- x. namespace
  | SimplexDAG -- x.dag content type
  deriving (Eq, Show)

type NameSpace = ByteString

data MsgBodyPartData
  = -- | fully loaded
    MBFull MsgData
  | -- | partially loaded
    MBPartial Int MsgData
  | -- | not loaded yet
    MBEmpty Int
  deriving (Eq, Show)

data MsgData
  = MsgData ByteString
  | MsgDataRec {dataId :: Int64, dataSize :: Int}
  deriving (Eq, Show)

rawChatMessageP :: Parser RawChatMessage
rawChatMessageP = A.char '#' *> chatMsgContP <|> chatMsgP
  where
    chatMsgContP :: Parser RawChatMessage
    chatMsgContP = do
      prevChatMsgId <- A.decimal <* A.char '.'
      continuationId <- A.decimal <* A.space
      continuationData <- A.takeByteString
      pure RawChatMsgContinuation {prevChatMsgId, continuationId, continuationData}
    chatMsgP :: Parser RawChatMessage
    chatMsgP = do
      chatMsgId <- optional A.decimal <* A.space
      chatMsgEvent <- chatMsgEventP <* A.space
      chatMsgParams <- A.takeWhile1 (not . A.inClass ", ") `A.sepBy'` A.char ',' <* A.space
      chatMsgBody <- msgBodyContent =<< contentInfo `A.sepBy'` A.char ',' <* A.space
      pure RawChatMessage {chatMsgId, chatMsgEvent, chatMsgParams, chatMsgBody}
    chatMsgEventP :: Parser ChatMsgEvent
    chatMsgEventP = do
      identifier <* A.char '.' >>= \case
        "x" -> SimplexChatMsgEvent <$> cEvent
        ns -> ChatMsgEvent ns <$> cEvent
      where
        cEvent :: Parser (NonEmpty ByteString)
        cEvent =
          identifier `A.sepBy1'` A.char '.' >>= \case
            [] -> fail "empty chat event"
            a : as -> pure (a :| as)
    identifier :: Parser ByteString
    identifier = B.cons <$> A.letter_ascii <*> A.takeWhile (\c -> A.isAlpha_ascii c || A.isDigit c)
    contentInfo :: Parser MsgBodyContent
    contentInfo = do
      contentType <- contentTypeP
      contentSize <- A.char ':' *> A.decimal
      contentHash <- optional (A.char ':' *> base64P)
      pure MsgBodyContent {contentType, contentHash, contentData = MBEmpty contentSize}
    contentTypeP :: Parser ContentType
    contentTypeP = do
      identifier <* A.char '.' >>= \case
        "i" -> MimeContentType <$> cType
        "c" -> ChannelContentType <$> cType
        "x" -> simplexContentType <$> cType
        ns -> ContentType ns <$> cType
      where
        cType = A.takeTill (A.inClass ":, ")
        simplexContentType = \case
          "dag" -> SimplexDAG
          s -> SimplexContentType s
    msgBodyContent :: [MsgBodyContent] -> Parser [MsgBodyContent]
    msgBodyContent [] = pure []
    msgBodyContent (p@MsgBodyContent {contentData = MBEmpty size} : ps) = do
      s <- A.take size <* A.space <|> A.takeByteString
      if B.length s == size
        then (p {contentData = MBFull $ MsgData s} :) <$> msgBodyContent ps
        else pure $ (if B.null s then p else p {contentData = MBPartial size $ MsgData s}) : ps
    msgBodyContent _ = fail "expected contentData = MBEmpty"
