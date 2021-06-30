{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Protocol where

import Control.Applicative (optional, (<|>))
import Control.Monad.Except (throwError)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.List (findIndex)
import Data.Text (Text)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Parsers (base64P)

data ChatTransmission = ChatTransmission
  { agentMsgMeta :: MsgMeta,
    chatDirection :: ChatDirection,
    chatMessage :: ChatMessage
  }

newtype ChatDag = ChatDag ByteString

data ChatDirection
  = ReceivedDirectMessage Contact
  | SentDirectMessage Contact
  | ReceivedGroupMessage Group Contact
  | SentGroupMessage Group

newtype ChatMsgEvent = XMsgNew MessageType

data MessageType = MTText | MTImage

toMsgType :: ByteString -> Either ByteString MessageType
toMsgType = \case
  "c.text" -> Right MTText
  "c.image" -> Right MTImage
  t -> Left $ "invalid message type " <> t

rawMsgType :: MessageType -> ByteString
rawMsgType = \case
  MTText -> "c.text"
  MTImage -> "c.image"

data ChatMessage = ChatMessage
  { chatMsgId :: Maybe Int64,
    chatMsgEvent :: ChatMsgEvent,
    chatMsgBody :: [MsgBodyContent],
    chatDAGIdx :: Maybe Int
  }

toChatMessage :: RawChatMessage -> Either ByteString ChatMessage
toChatMessage RawChatMessage {chatMsgId, chatMsgEvent, chatMsgParams, chatMsgBody} = do
  body <- mapM toMsgBodyContent chatMsgBody
  case chatMsgEvent of
    "x.msg.new" -> case chatMsgParams of
      [mt] -> do
        t <- toMsgType mt
        pure ChatMessage {chatMsgId, chatMsgEvent = XMsgNew t, chatMsgBody = body, chatDAGIdx = findDAG body}
      _ -> throwError "x.msg.new expects one parameter"
    _ -> throwError $ "unsupported event " <> chatMsgEvent
toChatMessage _ = Left "message continuation"

findDAG :: [MsgBodyContent] -> Maybe Int
findDAG = findIndex $ \MsgBodyContent {contentType = t} -> t == SimplexDAG

validContentTypes :: MessageType -> [MsgBodyContent] -> Either ByteString [MsgBodyContent]
validContentTypes _ = Right

rawChatMessage :: ChatMessage -> RawChatMessage
rawChatMessage ChatMessage {chatMsgId, chatMsgEvent = event, chatMsgBody = body} =
  case event of
    XMsgNew t ->
      let chatMsgBody = map rawMsgBodyContent body
       in RawChatMessage {chatMsgId, chatMsgEvent = "x.msg.new", chatMsgParams = [rawMsgType t], chatMsgBody}

toMsgBodyContent :: RawMsgBodyContent -> Either ByteString MsgBodyContent
toMsgBodyContent RawMsgBodyContent {contentType, contentHash, contentData} = do
  cType <- toContentType contentType
  pure MsgBodyContent {contentType = cType, contentHash, contentData}

rawMsgBodyContent :: MsgBodyContent -> RawMsgBodyContent
rawMsgBodyContent MsgBodyContent {contentType = t, contentHash, contentData} =
  RawMsgBodyContent {contentType = rawContentType t, contentHash, contentData}

data MsgBodyContent = MsgBodyContent
  { contentType :: ContentType,
    contentHash :: Maybe ByteString,
    contentData :: MsgBodyPartData
  }
  deriving (Eq, Show)

data ContentType
  = SimplexContentType XContentType
  | MimeContentType MContentType
  | SimplexDAG
  deriving (Eq, Show)

data XContentType = CCText | CCImage deriving (Eq, Show)

data MContentType = MCImageJPG | MCImagePNG deriving (Eq, Show)

toContentType :: RawContentType -> Either ByteString ContentType
toContentType (RawContentType ns cType) = case ns of
  "x" -> case cType of
    "text" -> Right $ SimplexContentType CCText
    "image" -> Right $ SimplexContentType CCImage
    "dag" -> Right SimplexDAG
    _ -> err
  "m" -> case cType of
    "image/jpg" -> Right $ MimeContentType MCImageJPG
    "image/png" -> Right $ MimeContentType MCImagePNG
    _ -> err
  _ -> err
  where
    err = Left $ "invalid content type " <> ns <> "." <> cType

rawContentType :: ContentType -> RawContentType
rawContentType t = case t of
  SimplexContentType t' -> RawContentType "x" $ case t' of
    CCText -> "text"
    CCImage -> "image"
  MimeContentType t' -> RawContentType "m" $ case t' of
    MCImageJPG -> "image/jpg"
    MCImagePNG -> "image/png"
  SimplexDAG -> RawContentType "x" "dag"

newtype ContentMsg = NewContentMsg ContentData

newtype ContentData = ContentText Text

data RawChatMessage
  = RawChatMessage
      { chatMsgId :: Maybe Int64,
        chatMsgEvent :: ByteString,
        chatMsgParams :: [ByteString],
        chatMsgBody :: [RawMsgBodyContent]
      }
  | RawChatMsgContinuation
      { prevChatMsgId :: Int64,
        continuationId :: Int,
        continuationData :: ByteString
      }
  deriving (Eq, Show)

data RawMsgBodyContent = RawMsgBodyContent
  { contentType :: RawContentType,
    contentHash :: Maybe ByteString,
    contentData :: MsgBodyPartData
  }
  deriving (Eq, Show)

data RawContentType = RawContentType NameSpace ByteString
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
      chatMsgEvent <- B.intercalate "." <$> identifier `A.sepBy1'` A.char '.' <* A.space
      chatMsgParams <- A.takeWhile1 (not . A.inClass ", ") `A.sepBy'` A.char ',' <* A.space
      chatMsgBody <- msgBodyContent =<< contentInfo `A.sepBy'` A.char ',' <* A.space
      pure RawChatMessage {chatMsgId, chatMsgEvent, chatMsgParams, chatMsgBody}
    identifier :: Parser ByteString
    identifier = B.cons <$> A.letter_ascii <*> A.takeWhile (\c -> A.isAlpha_ascii c || A.isDigit c)
    contentInfo :: Parser RawMsgBodyContent
    contentInfo = do
      contentType <- RawContentType <$> identifier <* A.char '.' <*> A.takeTill (A.inClass ":, ")
      contentSize <- A.char ':' *> A.decimal
      contentHash <- optional (A.char ':' *> base64P)
      pure RawMsgBodyContent {contentType, contentHash, contentData = MBEmpty contentSize}
    msgBodyContent :: [RawMsgBodyContent] -> Parser [RawMsgBodyContent]
    msgBodyContent [] = pure []
    msgBodyContent (p@RawMsgBodyContent {contentData = MBEmpty size} : ps) = do
      s <- A.take size <* A.space <|> A.takeByteString
      if B.length s == size
        then ((p {contentData = MBFull $ MsgData s} :: RawMsgBodyContent) :) <$> msgBodyContent ps
        else pure $ (if B.null s then p else p {contentData = MBPartial size $ MsgData s} :: RawMsgBodyContent) : ps
    msgBodyContent _ = fail "expected contentData = MBEmpty"
