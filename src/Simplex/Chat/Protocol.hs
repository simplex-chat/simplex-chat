{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.Protocol where

import Control.Applicative (optional)
import Control.Monad ((<=<))
import Control.Monad.Except (throwError)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.List (find)
import Data.Text (Text)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (bshow)

data ChatDirection (p :: AParty) where
  ReceivedDirectMessage :: Contact -> ChatDirection 'Agent
  SentDirectMessage :: Contact -> ChatDirection 'Client
  ReceivedGroupMessage :: Group -> Contact -> ChatDirection 'Agent
  SentGroupMessage :: Group -> ChatDirection 'Client

deriving instance Eq (ChatDirection p)

deriving instance Show (ChatDirection p)

data ChatMsgEvent
  = XMsgNew {messageType :: MessageType, files :: [(ContentType, Int)], content :: [MsgBodyContent]}
  | XInfo Profile
  | XGrpInv InvitationId MemberId GroupMemberRole GroupProfile
  | XGrpAcpt InvitationId SMPQueueInfo
  | XGrpMemNew MemberId GroupMemberRole Profile
  | XGrpMemIntro MemberId GroupMemberRole Profile
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
    chatDAG :: Maybe ByteString
  }
  deriving (Eq, Show)

toChatMessage :: RawChatMessage -> Either String ChatMessage
toChatMessage RawChatMessage {chatMsgId, chatMsgEvent, chatMsgParams, chatMsgBody} = do
  (chatDAG, body) <- getDAG <$> mapM toMsgBodyContent chatMsgBody
  case chatMsgEvent of
    "x.msg.new" -> case chatMsgParams of
      mt : rawFiles -> do
        t <- toMsgType mt
        files <- mapM (toContentInfo <=< parseAll contentInfoP) rawFiles
        let msg = XMsgNew {messageType = t, files, content = body}
        pure ChatMessage {chatMsgId, chatMsgEvent = msg, chatDAG}
      [] -> throwError "x.msg.new expects at least one parameter"
    "x.info" -> case chatMsgParams of
      [] -> do
        profile <- getJSON body
        pure ChatMessage {chatMsgId, chatMsgEvent = XInfo profile, chatDAG}
      _ -> throwError "x.info expects no parameters"
    "x.grp.inv" -> case chatMsgParams of
      [invId', memId', role'] -> do
        invId <- B64.decode invId'
        memId <- B64.decode memId'
        role <- toMemberRole role'
        groupProfile <- getJSON body
        pure ChatMessage {chatMsgId, chatMsgEvent = XGrpInv invId memId role groupProfile, chatDAG}
      _ -> throwError "x.grp.inv expects 3 parameters"
    "x.grp.acpt" -> case chatMsgParams of
      [invId, qInfo] -> do
        msg <- XGrpAcpt <$> B64.decode invId <*> parseAll smpQueueInfoP qInfo
        pure ChatMessage {chatMsgId, chatMsgEvent = msg, chatDAG}
      _ -> throwError "x.grp.acpt expects 2 parameters"
    "x.grp.mem.new" -> case chatMsgParams of
      [memId, role] -> do
        msg <- XGrpMemNew <$> B64.decode memId <*> toMemberRole role <*> getJSON body
        pure ChatMessage {chatMsgId, chatMsgEvent = msg, chatDAG}
      _ -> throwError "x.grp.acpt expects 2 parameters"
    "x.grp.mem.intro" -> case chatMsgParams of
      [memId, role] -> do
        msg <- XGrpMemIntro <$> B64.decode memId <*> toMemberRole role <*> getJSON body
        pure ChatMessage {chatMsgId, chatMsgEvent = msg, chatDAG}
      _ -> throwError "x.grp.acpt expects 2 parameters"
    _ -> throwError $ "unsupported event " <> B.unpack chatMsgEvent
  where
    getDAG :: [MsgBodyContent] -> (Maybe ByteString, [MsgBodyContent])
    getDAG body = case break (isContentType SimplexDAG) body of
      (b, MsgBodyContent SimplexDAG dag : a) -> (Just dag, b <> a)
      _ -> (Nothing, body)
    toContentInfo :: (RawContentType, Int) -> Either String (ContentType, Int)
    toContentInfo (rawType, size) = (,size) <$> toContentType rawType
    getJSON :: FromJSON a => [MsgBodyContent] -> Either String a
    getJSON = J.eitherDecodeStrict' <=< getSimplexContentType XCJson

isContentType :: ContentType -> MsgBodyContent -> Bool
isContentType t MsgBodyContent {contentType = t'} = t == t'

isSimplexContentType :: XContentType -> MsgBodyContent -> Bool
isSimplexContentType = isContentType . SimplexContentType

getContentType :: ContentType -> [MsgBodyContent] -> Either String ByteString
getContentType t body = case find (isContentType t) body of
  Just MsgBodyContent {contentData} -> Right contentData
  Nothing -> Left "no required content type"

getSimplexContentType :: XContentType -> [MsgBodyContent] -> Either String ByteString
getSimplexContentType = getContentType . SimplexContentType

rawChatMessage :: ChatMessage -> RawChatMessage
rawChatMessage ChatMessage {chatMsgId, chatMsgEvent, chatDAG} =
  case chatMsgEvent of
    XMsgNew {messageType = t, files, content} ->
      let rawFiles = map (serializeContentInfo . rawContentInfo) files
          chatMsgParams = rawMsgType t : rawFiles
          chatMsgBody = rawWithDAG content
       in RawChatMessage {chatMsgId, chatMsgEvent = "x.msg.new", chatMsgParams, chatMsgBody}
    XInfo profile ->
      let chatMsgBody = rawWithDAG [jsonBody profile]
       in RawChatMessage {chatMsgId, chatMsgEvent = "x.info", chatMsgParams = [], chatMsgBody}
    XGrpInv invId memId role groupProfile ->
      let chatMsgParams = [B64.encode invId, B64.encode memId, serializeMemberRole role]
          chatMsgBody = rawWithDAG [jsonBody groupProfile]
       in RawChatMessage {chatMsgId, chatMsgEvent = "x.grp.inv", chatMsgParams, chatMsgBody}
    XGrpAcpt invId qInfo ->
      let chatMsgParams = [B64.encode invId, serializeSmpQueueInfo qInfo]
       in RawChatMessage {chatMsgId, chatMsgEvent = "x.grp.acpt", chatMsgParams, chatMsgBody = []}
    XGrpMemNew memId role profile ->
      let chatMsgParams = [B64.encode memId, serializeMemberRole role]
          chatMsgBody = rawWithDAG [jsonBody profile]
       in RawChatMessage {chatMsgId, chatMsgEvent = "x.grp.mem.new", chatMsgParams, chatMsgBody}
    XGrpMemIntro memId role profile ->
      let chatMsgParams = [B64.encode memId, serializeMemberRole role]
          chatMsgBody = rawWithDAG [jsonBody profile]
       in RawChatMessage {chatMsgId, chatMsgEvent = "x.grp.mem.intro", chatMsgParams, chatMsgBody}
  where
    rawContentInfo :: (ContentType, Int) -> (RawContentType, Int)
    rawContentInfo (t, size) = (rawContentType t, size)
    jsonBody :: ToJSON a => a -> MsgBodyContent
    jsonBody x =
      let json = LB.toStrict $ J.encode x
       in MsgBodyContent {contentType = SimplexContentType XCJson, contentData = json}
    rawWithDAG :: [MsgBodyContent] -> [RawMsgBodyContent]
    rawWithDAG body = map rawMsgBodyContent $ case chatDAG of
      Nothing -> body
      Just dag -> MsgBodyContent {contentType = SimplexDAG, contentData = dag} : body

toMsgBodyContent :: RawMsgBodyContent -> Either String MsgBodyContent
toMsgBodyContent RawMsgBodyContent {contentType, contentData} = do
  cType <- toContentType contentType
  pure MsgBodyContent {contentType = cType, contentData}

rawMsgBodyContent :: MsgBodyContent -> RawMsgBodyContent
rawMsgBodyContent MsgBodyContent {contentType = t, contentData} =
  RawMsgBodyContent {contentType = rawContentType t, contentData}

data MsgBodyContent = MsgBodyContent
  { contentType :: ContentType,
    contentData :: ByteString
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

data RawChatMessage = RawChatMessage
  { chatMsgId :: Maybe Int64,
    chatMsgEvent :: ByteString,
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

newtype MsgData = MsgData ByteString
  deriving (Eq, Show)

class DataLength a where
  dataLength :: a -> Int

rawChatMessageP :: Parser RawChatMessage
rawChatMessageP = do
  chatMsgId <- optional A.decimal <* A.space
  chatMsgEvent <- B.intercalate "." <$> identifierP `A.sepBy1'` A.char '.' <* A.space
  chatMsgParams <- A.takeWhile1 (not . A.inClass ", ") `A.sepBy'` A.char ',' <* A.space
  chatMsgBody <- msgBodyContent =<< contentInfoP `A.sepBy'` A.char ',' <* A.space
  pure RawChatMessage {chatMsgId, chatMsgEvent, chatMsgParams, chatMsgBody}
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
serializeRawChatMessage RawChatMessage {chatMsgId, chatMsgEvent, chatMsgParams, chatMsgBody} =
  B.unwords
    [ maybe "" bshow chatMsgId,
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
