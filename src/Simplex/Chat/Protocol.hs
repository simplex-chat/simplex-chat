{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Simplex.Chat.Protocol where

import Control.Applicative (optional, (<|>))
import Control.Monad.Except (throwError)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.List (findIndex)
import Data.Text (Text)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Parsers (base64P, parseAll)
import Simplex.Messaging.Util (bshow)

data ChatDirection (p :: AParty) where
  ReceivedDirectMessage :: Contact -> ChatDirection 'Agent
  SentDirectMessage :: Contact -> ChatDirection 'Client
  ReceivedGroupMessage :: Group -> Contact -> ChatDirection 'Agent
  SentGroupMessage :: Group -> ChatDirection 'Client

deriving instance Eq (ChatDirection p)

deriving instance Show (ChatDirection p)

data ChatMsgEvent
  = XMsgNew MessageType
  | XInfo
  | XGrpInv InvitationId MemberId GroupMemberRole
  | XGrpAcpt InvitationId SMPQueueInfo
  | XGrpMemNew MemberId GroupMemberRole
  | XGrpMemIntro MemberId GroupMemberRole
  deriving (Eq, Show)

type MemberId = ByteString

data MessageType = MTText | MTImage deriving (Eq, Show)

data GroupMemberRole = GROwner | GRAdmin | GRMember
  deriving (Eq, Show)

toMemberRole :: ByteString -> Either String GroupMemberRole
toMemberRole = \case
  "owner" -> Right GROwner
  "admin" -> Right GRAdmin
  "member" -> Right GRMember
  r -> Left $ "invalid group member role " <> B.unpack r

serializeMemberRole :: GroupMemberRole -> ByteString
serializeMemberRole = \case
  GROwner -> "owner"
  GRAdmin -> "admin"
  GRMember -> "member"

toMsgType :: ByteString -> Either String MessageType
toMsgType = \case
  "c.text" -> Right MTText
  "c.image" -> Right MTImage
  t -> Left $ "invalid message type " <> B.unpack t

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
  deriving (Eq, Show)

toChatMessage :: RawChatMessage -> Either String ChatMessage
toChatMessage RawChatMessage {chatMsgId, chatMsgEvent, chatMsgParams, chatMsgBody} = do
  body <- mapM toMsgBodyContent chatMsgBody
  let chatDAGIdx = findDAG body
  case chatMsgEvent of
    "x.msg.new" -> case chatMsgParams of
      [mt] -> do
        t <- toMsgType mt
        pure ChatMessage {chatMsgId, chatMsgEvent = XMsgNew t, chatMsgBody = body, chatDAGIdx}
      _ -> throwError "x.msg.new expects one parameter"
    "x.info" -> case chatMsgParams of
      [] -> pure ChatMessage {chatMsgId, chatMsgEvent = XInfo, chatMsgBody = body, chatDAGIdx}
      _ -> throwError "x.info expects no parameters"
    "x.grp.inv" -> case chatMsgParams of
      [invId', memId', role'] -> do
        invId <- B64.decode invId'
        memId <- B64.decode memId'
        role <- toMemberRole role'
        pure ChatMessage {chatMsgId, chatMsgEvent = XGrpInv invId memId role, chatMsgBody = body, chatDAGIdx}
      _ -> throwError "x.grp.inv expects 3 parameters"
    "x.grp.acpt" -> case chatMsgParams of
      [invId', qInfo'] -> do
        invId <- B64.decode invId'
        qInfo <- parseAll smpQueueInfoP qInfo'
        pure ChatMessage {chatMsgId, chatMsgEvent = XGrpAcpt invId qInfo, chatMsgBody = body, chatDAGIdx}
      _ -> throwError "x.grp.acpt expects 2 parameters"
    "x.grp.mem.new" -> case chatMsgParams of
      [memId', role'] -> do
        memId <- B64.decode memId'
        role <- toMemberRole role'
        pure ChatMessage {chatMsgId, chatMsgEvent = XGrpMemNew memId role, chatMsgBody = body, chatDAGIdx}
      _ -> throwError "x.grp.acpt expects 2 parameters"
    "x.grp.mem.intro" -> case chatMsgParams of
      [memId', role'] -> do
        memId <- B64.decode memId'
        role <- toMemberRole role'
        pure ChatMessage {chatMsgId, chatMsgEvent = XGrpMemIntro memId role, chatMsgBody = body, chatDAGIdx}
      _ -> throwError "x.grp.acpt expects 2 parameters"
    _ -> throwError $ "unsupported event " <> B.unpack chatMsgEvent
toChatMessage _ = Left "message continuation"

findDAG :: [MsgBodyContent] -> Maybe Int
findDAG = findIndex $ isContentType SimplexDAG

isContentType :: ContentType -> MsgBodyContent -> Bool
isContentType t MsgBodyContent {contentType = t'} = t == t'

isSimplexContentType :: XContentType -> MsgBodyContent -> Bool
isSimplexContentType = isContentType . SimplexContentType

rawChatMessage :: ChatMessage -> RawChatMessage
rawChatMessage ChatMessage {chatMsgId, chatMsgEvent = event, chatMsgBody = body} =
  case event of
    XMsgNew t -> RawChatMessage {chatMsgId, chatMsgEvent = "x.msg.new", chatMsgParams = [rawMsgType t], chatMsgBody}
    XInfo -> RawChatMessage {chatMsgId, chatMsgEvent = "x.info", chatMsgParams = [], chatMsgBody}
    XGrpInv invId memId role ->
      let chatMsgParams = [B64.encode invId, B64.encode memId, serializeMemberRole role]
       in RawChatMessage {chatMsgId, chatMsgEvent = "x.grp.inv", chatMsgParams, chatMsgBody}
    XGrpAcpt invId qInfo ->
      let chatMsgParams = [B64.encode invId, serializeSmpQueueInfo qInfo]
       in RawChatMessage {chatMsgId, chatMsgEvent = "x.grp.acpt", chatMsgParams, chatMsgBody}
    XGrpMemNew memId role ->
      let chatMsgParams = [B64.encode memId, serializeMemberRole role]
       in RawChatMessage {chatMsgId, chatMsgEvent = "x.grp.mem.new", chatMsgParams, chatMsgBody}
    XGrpMemIntro memId role ->
      let chatMsgParams = [B64.encode memId, serializeMemberRole role]
       in RawChatMessage {chatMsgId, chatMsgEvent = "x.grp.mem.intro", chatMsgParams, chatMsgBody}
  where
    chatMsgBody = map rawMsgBodyContent body

toMsgBodyContent :: RawMsgBodyContent -> Either String MsgBodyContent
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

data XContentType = XCText | XCImage | XCJson deriving (Eq, Show)

data MContentType = MCImageJPG | MCImagePNG deriving (Eq, Show)

toContentType :: RawContentType -> Either String ContentType
toContentType (RawContentType ns cType) = case ns of
  "x" -> case cType of
    "text" -> Right $ SimplexContentType XCText
    "image" -> Right $ SimplexContentType XCImage
    "json" -> Right $ SimplexContentType XCJson
    "dag" -> Right SimplexDAG
    _ -> err
  "m" -> case cType of
    "image/jpg" -> Right $ MimeContentType MCImageJPG
    "image/png" -> Right $ MimeContentType MCImagePNG
    _ -> err
  _ -> err
  where
    err = Left . B.unpack $ "invalid content type " <> ns <> "." <> cType

rawContentType :: ContentType -> RawContentType
rawContentType t = case t of
  SimplexContentType t' -> RawContentType "x" $ case t' of
    XCText -> "text"
    XCImage -> "image"
    XCJson -> "json"
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

class DataLength a where
  dataLength :: a -> Int

instance DataLength MsgBodyPartData where
  dataLength (MBFull d) = dataLength d
  dataLength (MBPartial l _) = l
  dataLength (MBEmpty l) = l

instance DataLength MsgData where
  dataLength (MsgData s) = B.length s
  dataLength MsgDataRec {dataSize} = dataSize

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

serializeRawChatMessage :: RawChatMessage -> ByteString
serializeRawChatMessage = \case
  RawChatMessage {chatMsgId, chatMsgEvent, chatMsgParams, chatMsgBody} ->
    B.unwords
      [ maybe "" bshow chatMsgId,
        chatMsgEvent,
        B.intercalate "," chatMsgParams,
        B.unwords $ map serializeContentInfo chatMsgBody,
        B.unwords $ map serializeContentData chatMsgBody
      ]
  RawChatMsgContinuation {prevChatMsgId, continuationId, continuationData} ->
    bshow prevChatMsgId <> "." <> bshow continuationId <> " " <> continuationData

serializeContentInfo :: RawMsgBodyContent -> ByteString
serializeContentInfo RawMsgBodyContent {contentType = RawContentType ns cType, contentHash, contentData} =
  ns <> "." <> cType <> ":" <> bshow (dataLength contentData) <> maybe "" (":" <>) contentHash

serializeContentData :: RawMsgBodyContent -> ByteString
serializeContentData RawMsgBodyContent {contentData = MBFull (MsgData s)} = s
serializeContentData _ = ""
