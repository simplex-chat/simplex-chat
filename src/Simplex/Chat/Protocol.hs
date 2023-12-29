{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Protocol where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Type.Equality
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Simplex.Chat.Call
import Simplex.Chat.Types
import Simplex.Chat.Types.Util
import Simplex.Messaging.Encoding
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, fromTextField_, fstToLower, parseAll, sumTypeJSON, taggedObjectJSON)
import Simplex.Messaging.Util (eitherToMaybe, safeDecodeUtf8, (<$?>))
import Simplex.Messaging.Version hiding (version)

-- This should not be used directly in code, instead use `maxVersion chatVRange` from ChatConfig.
-- This indirection is needed for backward/forward compatibility testing.
-- Testing with real app versions is still needed, as tests use the current code with different version ranges, not the old code.
currentChatVersion :: Version
currentChatVersion = 5

-- This should not be used directly in code, instead use `chatVRange` from ChatConfig (see comment above)
supportedChatVRange :: VersionRange
supportedChatVRange = mkVersionRange 1 currentChatVersion

-- version range that supports skipping establishing direct connections in a group
groupNoDirectVRange :: VersionRange
groupNoDirectVRange = mkVersionRange 2 currentChatVersion

-- version range that supports establishing direct connection via x.grp.direct.inv with a group member
xGrpDirectInvVRange :: VersionRange
xGrpDirectInvVRange = mkVersionRange 2 currentChatVersion

-- version range that supports joining group via group link without creating direct contact
groupLinkNoContactVRange :: VersionRange
groupLinkNoContactVRange = mkVersionRange 3 currentChatVersion

-- version range that supports group forwarding
groupForwardVRange :: VersionRange
groupForwardVRange = mkVersionRange 4 currentChatVersion

-- version range that supports batch sending in groups
batchSendVRange :: VersionRange
batchSendVRange = mkVersionRange 5 currentChatVersion

data ConnectionEntity
  = RcvDirectMsgConnection {entityConnection :: Connection, contact :: Maybe Contact}
  | RcvGroupMsgConnection {entityConnection :: Connection, groupInfo :: GroupInfo, groupMember :: GroupMember}
  | SndFileConnection {entityConnection :: Connection, sndFileTransfer :: SndFileTransfer}
  | RcvFileConnection {entityConnection :: Connection, rcvFileTransfer :: RcvFileTransfer}
  | UserContactConnection {entityConnection :: Connection, userContact :: UserContact}
  deriving (Eq, Show)

$(JQ.deriveJSON (sumTypeJSON fstToLower) ''ConnectionEntity)

updateEntityConnStatus :: ConnectionEntity -> ConnStatus -> ConnectionEntity
updateEntityConnStatus connEntity connStatus = case connEntity of
  RcvDirectMsgConnection c ct_ -> RcvDirectMsgConnection (st c) ((\ct -> (ct :: Contact) {activeConn = Just $ st c}) <$> ct_)
  RcvGroupMsgConnection c gInfo m@GroupMember {activeConn = c'} -> RcvGroupMsgConnection (st c) gInfo m {activeConn = st <$> c'}
  SndFileConnection c ft -> SndFileConnection (st c) ft
  RcvFileConnection c ft -> RcvFileConnection (st c) ft
  UserContactConnection c uc -> UserContactConnection (st c) uc
  where
    st c = c {connStatus}

data MsgEncoding = Binary | Json

data SMsgEncoding (e :: MsgEncoding) where
  SBinary :: SMsgEncoding 'Binary
  SJson :: SMsgEncoding 'Json

deriving instance Show (SMsgEncoding e)

class MsgEncodingI (e :: MsgEncoding) where
  encoding :: SMsgEncoding e

instance MsgEncodingI 'Binary where encoding = SBinary

instance MsgEncodingI 'Json where encoding = SJson

instance TestEquality SMsgEncoding where
  testEquality SBinary SBinary = Just Refl
  testEquality SJson SJson = Just Refl
  testEquality _ _ = Nothing

checkEncoding :: forall t e e'. (MsgEncodingI e, MsgEncodingI e') => t e' -> Either String (t e)
checkEncoding x = case testEquality (encoding @e) (encoding @e') of
  Just Refl -> Right x
  Nothing -> Left "bad encoding"

data AppMessage (e :: MsgEncoding) where
  AMJson :: AppMessageJson -> AppMessage 'Json
  AMBinary :: AppMessageBinary -> AppMessage 'Binary

-- chat message is sent as JSON with these properties
data AppMessageJson = AppMessageJson
  { v :: Maybe ChatVersionRange,
    msgId :: Maybe SharedMsgId,
    event :: Text,
    params :: J.Object
  }

data AppMessageBinary = AppMessageBinary
  { msgId :: Maybe SharedMsgId,
    tag :: Char,
    body :: ByteString
  }

instance StrEncoding AppMessageBinary where
  strEncode AppMessageBinary {tag, msgId, body} = smpEncode (tag, msgId', Tail body)
    where
      msgId' = maybe B.empty (\(SharedMsgId mId') -> mId') msgId
  strP = do
    (tag, msgId', Tail body) <- smpP
    let msgId = if B.null msgId' then Nothing else Just (SharedMsgId msgId')
    pure AppMessageBinary {tag, msgId, body}

newtype SharedMsgId = SharedMsgId ByteString
  deriving (Eq, Show)

instance FromField SharedMsgId where fromField f = SharedMsgId <$> fromField f

instance ToField SharedMsgId where toField (SharedMsgId m) = toField m

instance StrEncoding SharedMsgId where
  strEncode (SharedMsgId m) = strEncode m
  strDecode s = SharedMsgId <$> strDecode s
  strP = SharedMsgId <$> strP

instance FromJSON SharedMsgId where
  parseJSON = strParseJSON "SharedMsgId"

instance ToJSON SharedMsgId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

$(JQ.deriveJSON defaultJSON ''AppMessageJson)

data MsgRef = MsgRef
  { msgId :: Maybe SharedMsgId,
    sentAt :: UTCTime,
    sent :: Bool,
    memberId :: Maybe MemberId -- must be present in all group message references, both referencing sent and received
  }
  deriving (Eq, Show)

$(JQ.deriveJSON defaultJSON ''MsgRef)

data LinkPreview = LinkPreview {uri :: Text, title :: Text, description :: Text, image :: ImageData, content :: Maybe LinkContent}
  deriving (Eq, Show)

data LinkContent = LCPage | LCImage | LCVideo {duration :: Maybe Int} | LCUnknown {tag :: Text, json :: J.Object}
  deriving (Eq, Show)

$(pure [])

instance FromJSON LinkContent where
  parseJSON v@(J.Object j) =
    $(JQ.mkParseJSON (taggedObjectJSON $ dropPrefix "LC") ''LinkContent) v
      <|> LCUnknown <$> j .: "type" <*> pure j
  parseJSON invalid =
    JT.prependFailure "bad LinkContent, " (JT.typeMismatch "Object" invalid)

instance ToJSON LinkContent where
  toJSON = \case
    LCUnknown _ j -> J.Object j
    v -> $(JQ.mkToJSON (taggedObjectJSON $ dropPrefix "LC") ''LinkContent) v
  toEncoding = \case
    LCUnknown _ j -> JE.value $ J.Object j
    v -> $(JQ.mkToEncoding (taggedObjectJSON $ dropPrefix "LC") ''LinkContent) v

$(JQ.deriveJSON defaultJSON ''LinkPreview)

data ChatMessage e = ChatMessage
  { chatVRange :: VersionRange,
    msgId :: Maybe SharedMsgId,
    chatMsgEvent :: ChatMsgEvent e
  }
  deriving (Eq, Show)

data AChatMessage = forall e. MsgEncodingI e => ACMsg (SMsgEncoding e) (ChatMessage e)

data ChatMsgEvent (e :: MsgEncoding) where
  XMsgNew :: MsgContainer -> ChatMsgEvent 'Json
  XMsgFileDescr :: {msgId :: SharedMsgId, fileDescr :: FileDescr} -> ChatMsgEvent 'Json
  XMsgUpdate :: {msgId :: SharedMsgId, content :: MsgContent, ttl :: Maybe Int, live :: Maybe Bool} -> ChatMsgEvent 'Json
  XMsgDel :: SharedMsgId -> Maybe MemberId -> ChatMsgEvent 'Json
  XMsgDeleted :: ChatMsgEvent 'Json
  XMsgReact :: {msgId :: SharedMsgId, memberId :: Maybe MemberId, reaction :: MsgReaction, add :: Bool} -> ChatMsgEvent 'Json
  XFile :: FileInvitation -> ChatMsgEvent 'Json -- TODO discontinue
  XFileAcpt :: String -> ChatMsgEvent 'Json -- direct file protocol
  XFileAcptInv :: SharedMsgId -> Maybe ConnReqInvitation -> String -> ChatMsgEvent 'Json
  XFileCancel :: SharedMsgId -> ChatMsgEvent 'Json
  XInfo :: Profile -> ChatMsgEvent 'Json
  XContact :: Profile -> Maybe XContactId -> ChatMsgEvent 'Json
  XDirectDel :: ChatMsgEvent 'Json
  XGrpInv :: GroupInvitation -> ChatMsgEvent 'Json
  XGrpAcpt :: MemberId -> ChatMsgEvent 'Json
  XGrpLinkInv :: GroupLinkInvitation -> ChatMsgEvent 'Json
  XGrpLinkMem :: Profile -> ChatMsgEvent 'Json
  XGrpMemNew :: MemberInfo -> ChatMsgEvent 'Json
  XGrpMemIntro :: MemberInfo -> ChatMsgEvent 'Json
  XGrpMemInv :: MemberId -> IntroInvitation -> ChatMsgEvent 'Json
  XGrpMemFwd :: MemberInfo -> IntroInvitation -> ChatMsgEvent 'Json
  XGrpMemInfo :: MemberId -> Profile -> ChatMsgEvent 'Json
  XGrpMemRole :: MemberId -> GroupMemberRole -> ChatMsgEvent 'Json
  XGrpMemCon :: MemberId -> ChatMsgEvent 'Json
  XGrpMemConAll :: MemberId -> ChatMsgEvent 'Json -- TODO not implemented
  XGrpMemDel :: MemberId -> ChatMsgEvent 'Json
  XGrpLeave :: ChatMsgEvent 'Json
  XGrpDel :: ChatMsgEvent 'Json
  XGrpInfo :: GroupProfile -> ChatMsgEvent 'Json
  XGrpDirectInv :: ConnReqInvitation -> Maybe MsgContent -> ChatMsgEvent 'Json
  XGrpMsgForward :: MemberId -> ChatMessage 'Json -> UTCTime -> ChatMsgEvent 'Json
  XInfoProbe :: Probe -> ChatMsgEvent 'Json
  XInfoProbeCheck :: ProbeHash -> ChatMsgEvent 'Json
  XInfoProbeOk :: Probe -> ChatMsgEvent 'Json
  XCallInv :: CallId -> CallInvitation -> ChatMsgEvent 'Json
  XCallOffer :: CallId -> CallOffer -> ChatMsgEvent 'Json
  XCallAnswer :: CallId -> CallAnswer -> ChatMsgEvent 'Json
  XCallExtra :: CallId -> CallExtraInfo -> ChatMsgEvent 'Json
  XCallEnd :: CallId -> ChatMsgEvent 'Json
  XOk :: ChatMsgEvent 'Json
  XUnknown :: {event :: Text, params :: J.Object} -> ChatMsgEvent 'Json
  BFileChunk :: SharedMsgId -> FileChunk -> ChatMsgEvent 'Binary

deriving instance Eq (ChatMsgEvent e)

deriving instance Show (ChatMsgEvent e)

data AChatMsgEvent = forall e. MsgEncodingI e => ACME (SMsgEncoding e) (ChatMsgEvent e)

deriving instance Show AChatMsgEvent

isForwardedGroupMsg :: ChatMsgEvent e -> Bool
isForwardedGroupMsg ev = case ev of
  XMsgNew mc -> case mcExtMsgContent mc of
    ExtMsgContent {file = Just FileInvitation {fileInline = Just _}} -> False
    _ -> True
  XMsgFileDescr _ _ -> True
  XMsgUpdate {} -> True
  XMsgDel _ _ -> True
  XMsgReact {} -> True
  XFileCancel _ -> True
  XInfo _ -> True
  XGrpMemNew _ -> True
  XGrpMemRole {} -> True
  XGrpMemDel _ -> True -- TODO there should be a special logic when deleting host member (e.g., host forwards it before deleting connections)
  XGrpLeave -> True
  XGrpDel -> True -- TODO there should be a special logic - host should forward before deleting connections
  XGrpInfo _ -> True
  _ -> False

forwardedGroupMsg :: forall e. MsgEncodingI e => ChatMessage e -> Maybe (ChatMessage 'Json)
forwardedGroupMsg msg@ChatMessage {chatMsgEvent} = case encoding @e of
  SJson | isForwardedGroupMsg chatMsgEvent -> Just msg
  _ -> Nothing

data MsgReaction = MREmoji {emoji :: MREmojiChar} | MRUnknown {tag :: Text, json :: J.Object}
  deriving (Eq, Show)

emojiTag :: IsString a => a
emojiTag = "emoji"

instance FromJSON MsgReaction where
  parseJSON (J.Object v) = do
    tag <- v .: "type"
    if tag == emojiTag
      then (MREmoji <$> v .: emojiTag) <|> pure (MRUnknown tag v)
      else pure $ MRUnknown tag v
  parseJSON invalid =
    JT.prependFailure "bad MsgContent, " (JT.typeMismatch "Object" invalid)

instance ToJSON MsgReaction where
  toJSON = \case
    MRUnknown {json} -> J.Object json
    MREmoji emoji -> J.object ["type" .= (emojiTag :: Text), emojiTag .= emoji]
  toEncoding = \case
    MRUnknown {json} -> JE.value $ J.Object json
    MREmoji emoji -> J.pairs $ "type" .= (emojiTag :: Text) <> emojiTag .= emoji

instance ToField MsgReaction where
  toField = toField . encodeJSON

instance FromField MsgReaction where
  fromField = fromTextField_ decodeJSON

newtype MREmojiChar = MREmojiChar Char
  deriving (Eq, Show)

instance ToJSON MREmojiChar where
  toEncoding (MREmojiChar c) = J.toEncoding c
  toJSON (MREmojiChar c) = J.toJSON c

instance FromJSON MREmojiChar where
  parseJSON v = mrEmojiChar <$?> J.parseJSON v

mrEmojiChar :: Char -> Either String MREmojiChar
mrEmojiChar c
  | c `elem` ("👍👎😀😢❤️🚀" :: String) = Right $ MREmojiChar c
  | otherwise = Left "bad emoji"

data FileChunk = FileChunk {chunkNo :: Integer, chunkBytes :: ByteString} | FileChunkCancel
  deriving (Eq, Show)

instance Encoding FileChunk where
  smpEncode = \case
    FileChunk {chunkNo, chunkBytes} -> smpEncode ('F', fromIntegral chunkNo :: Word32, Tail chunkBytes)
    FileChunkCancel -> smpEncode 'C'
  smpP =
    smpP >>= \case
      'F' -> do
        chunkNo <- fromIntegral <$> smpP @Word32
        Tail chunkBytes <- smpP
        pure FileChunk {chunkNo, chunkBytes}
      'C' -> pure FileChunkCancel
      _ -> fail "bad FileChunk"

newtype InlineFileChunk = IFC {unIFC :: FileChunk}

instance Encoding InlineFileChunk where
  smpEncode (IFC chunk) = case chunk of
    FileChunk {chunkNo, chunkBytes} -> smpEncode (w2c $ fromIntegral chunkNo, Tail chunkBytes)
    FileChunkCancel -> smpEncode '\NUL'
  smpP = do
    c <- A.anyChar
    IFC <$> case c of
      '\NUL' -> pure FileChunkCancel
      _ -> do
        Tail chunkBytes <- smpP
        pure FileChunk {chunkNo = fromIntegral $ c2w c, chunkBytes}

data QuotedMsg = QuotedMsg {msgRef :: MsgRef, content :: MsgContent}
  deriving (Eq, Show)

cmToQuotedMsg :: AChatMsgEvent -> Maybe QuotedMsg
cmToQuotedMsg = \case
  ACME _ (XMsgNew (MCQuote quotedMsg _)) -> Just quotedMsg
  _ -> Nothing

data MsgContentTag = MCText_ | MCLink_ | MCImage_ | MCVideo_ | MCVoice_ | MCFile_ | MCUnknown_ Text
  deriving (Eq)

instance StrEncoding MsgContentTag where
  strEncode = \case
    MCText_ -> "text"
    MCLink_ -> "link"
    MCImage_ -> "image"
    MCVideo_ -> "video"
    MCFile_ -> "file"
    MCVoice_ -> "voice"
    MCUnknown_ t -> encodeUtf8 t
  strDecode = \case
    "text" -> Right MCText_
    "link" -> Right MCLink_
    "image" -> Right MCImage_
    "video" -> Right MCVideo_
    "voice" -> Right MCVoice_
    "file" -> Right MCFile_
    t -> Right . MCUnknown_ $ safeDecodeUtf8 t
  strP = strDecode <$?> A.takeTill (== ' ')

instance FromJSON MsgContentTag where
  parseJSON = strParseJSON "MsgContentType"

instance ToJSON MsgContentTag where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data MsgContainer
  = MCSimple ExtMsgContent
  | MCQuote QuotedMsg ExtMsgContent
  | MCForward ExtMsgContent
  deriving (Eq, Show)

mcExtMsgContent :: MsgContainer -> ExtMsgContent
mcExtMsgContent = \case
  MCSimple c -> c
  MCQuote _ c -> c
  MCForward c -> c

isQuote :: MsgContainer -> Bool
isQuote = \case
  MCQuote {} -> True
  _ -> False

data MsgContent
  = MCText Text
  | MCLink {text :: Text, preview :: LinkPreview}
  | MCImage {text :: Text, image :: ImageData}
  | MCVideo {text :: Text, image :: ImageData, duration :: Int}
  | MCVoice {text :: Text, duration :: Int}
  | MCFile Text
  | MCUnknown {tag :: Text, text :: Text, json :: J.Object}
  deriving (Eq, Show)

msgContentText :: MsgContent -> Text
msgContentText = \case
  MCText t -> t
  MCLink {text} -> text
  MCImage {text} -> text
  MCVideo {text} -> text
  MCVoice {text, duration} ->
    if T.null text then msg else msg <> "; " <> text
    where
      msg = "voice message " <> durationText duration
  MCFile t -> t
  MCUnknown {text} -> text

toMCText :: MsgContent -> MsgContent
toMCText = MCText . msgContentText

durationText :: Int -> Text
durationText duration =
  let (mins, secs) = duration `divMod` 60 in T.pack $ "(" <> with0 mins <> ":" <> with0 secs <> ")"
  where
    with0 n
      | n <= 9 = '0' : show n
      | otherwise = show n

msgContentHasText :: MsgContent -> Bool
msgContentHasText = \case
  MCText t -> hasText t
  MCLink {text} -> hasText text
  MCImage {text} -> hasText text
  MCVideo {text} -> hasText text
  MCVoice {text} -> hasText text
  MCFile t -> hasText t
  MCUnknown {text} -> hasText text
  where
    hasText = not . T.null

isVoice :: MsgContent -> Bool
isVoice = \case
  MCVoice {} -> True
  _ -> False

msgContentTag :: MsgContent -> MsgContentTag
msgContentTag = \case
  MCText _ -> MCText_
  MCLink {} -> MCLink_
  MCImage {} -> MCImage_
  MCVideo {} -> MCVideo_
  MCVoice {} -> MCVoice_
  MCFile {} -> MCFile_
  MCUnknown {tag} -> MCUnknown_ tag

data ExtMsgContent = ExtMsgContent {content :: MsgContent, file :: Maybe FileInvitation, ttl :: Maybe Int, live :: Maybe Bool}
  deriving (Eq, Show)

$(JQ.deriveJSON defaultJSON ''QuotedMsg)

-- this limit reserves space for metadata in forwarded messages
-- 15780 (limit used for fileChunkSize) - 161 (x.grp.msg.forward overhead) = 15619, round to 15610
maxChatMsgSize :: Int64
maxChatMsgSize = 15610

data EncodedChatMessage = ECMEncoded L.ByteString | ECMLarge

encodeChatMessage :: MsgEncodingI e => ChatMessage e -> EncodedChatMessage
encodeChatMessage msg = do
  case chatToAppMessage msg of
    AMJson m -> do
      let body = J.encode m
      if LB.length body > maxChatMsgSize
        then ECMLarge
        else ECMEncoded body
    AMBinary m -> ECMEncoded . LB.fromStrict $ strEncode m

parseChatMessages :: ByteString -> [Either String AChatMessage]
parseChatMessages "" = [Left "empty string"]
parseChatMessages s = case B.head s of
  '{' -> [ACMsg SJson <$> J.eitherDecodeStrict' s]
  '[' -> case J.eitherDecodeStrict' s of
    Right v -> map parseItem v
    Left e -> [Left e]
  _ -> [ACMsg SBinary <$> (appBinaryToCM =<< strDecode s)]
  where
    parseItem :: J.Value -> Either String AChatMessage
    parseItem v = ACMsg SJson <$> JT.parseEither parseJSON v

parseMsgContainer :: J.Object -> JT.Parser MsgContainer
parseMsgContainer v =
  MCQuote <$> v .: "quote" <*> mc
    <|> (v .: "forward" >>= \f -> (if f then MCForward else MCSimple) <$> mc)
    <|> MCSimple <$> mc
  where
    mc = ExtMsgContent <$> v .: "content" <*> v .:? "file" <*> v .:? "ttl" <*> v .:? "live"

extMsgContent :: MsgContent -> Maybe FileInvitation -> ExtMsgContent
extMsgContent mc file = ExtMsgContent mc file Nothing Nothing

justTrue :: Bool -> Maybe Bool
justTrue True = Just True
justTrue False = Nothing

instance FromJSON MsgContent where
  parseJSON (J.Object v) =
    v .: "type" >>= \case
      MCText_ -> MCText <$> v .: "text"
      MCLink_ -> do
        text <- v .: "text"
        preview <- v .: "preview"
        pure MCLink {text, preview}
      MCImage_ -> do
        text <- v .: "text"
        image <- v .: "image"
        pure MCImage {text, image}
      MCVideo_ -> do
        text <- v .: "text"
        image <- v .: "image"
        duration <- v .: "duration"
        pure MCVideo {text, image, duration}
      MCVoice_ -> do
        text <- v .: "text"
        duration <- v .: "duration"
        pure MCVoice {text, duration}
      MCFile_ -> MCFile <$> v .: "text"
      MCUnknown_ tag -> do
        text <- fromMaybe unknownMsgType <$> v .:? "text"
        pure MCUnknown {tag, text, json = v}
  parseJSON invalid =
    JT.prependFailure "bad MsgContent, " (JT.typeMismatch "Object" invalid)

unknownMsgType :: Text
unknownMsgType = "unknown message type"

msgContainerJSON :: MsgContainer -> J.Object
msgContainerJSON = \case
  MCQuote qm mc -> o $ ("quote" .= qm) : msgContent mc
  MCForward mc -> o $ ("forward" .= True) : msgContent mc
  MCSimple mc -> o $ msgContent mc
  where
    o = JM.fromList
    msgContent (ExtMsgContent c file ttl live) = ("file" .=? file) $ ("ttl" .=? ttl) $ ("live" .=? live) ["content" .= c]

instance ToJSON MsgContent where
  toJSON = \case
    MCUnknown {json} -> J.Object json
    MCText t -> J.object ["type" .= MCText_, "text" .= t]
    MCLink {text, preview} -> J.object ["type" .= MCLink_, "text" .= text, "preview" .= preview]
    MCImage {text, image} -> J.object ["type" .= MCImage_, "text" .= text, "image" .= image]
    MCVideo {text, image, duration} -> J.object ["type" .= MCVideo_, "text" .= text, "image" .= image, "duration" .= duration]
    MCVoice {text, duration} -> J.object ["type" .= MCVoice_, "text" .= text, "duration" .= duration]
    MCFile t -> J.object ["type" .= MCFile_, "text" .= t]
  toEncoding = \case
    MCUnknown {json} -> JE.value $ J.Object json
    MCText t -> J.pairs $ "type" .= MCText_ <> "text" .= t
    MCLink {text, preview} -> J.pairs $ "type" .= MCLink_ <> "text" .= text <> "preview" .= preview
    MCImage {text, image} -> J.pairs $ "type" .= MCImage_ <> "text" .= text <> "image" .= image
    MCVideo {text, image, duration} -> J.pairs $ "type" .= MCVideo_ <> "text" .= text <> "image" .= image <> "duration" .= duration
    MCVoice {text, duration} -> J.pairs $ "type" .= MCVoice_ <> "text" .= text <> "duration" .= duration
    MCFile t -> J.pairs $ "type" .= MCFile_ <> "text" .= t

instance ToField MsgContent where
  toField = toField . encodeJSON

instance FromField MsgContent where
  fromField = fromTextField_ decodeJSON

data ACMEventTag = forall e. MsgEncodingI e => ACMEventTag (SMsgEncoding e) (CMEventTag e)

data CMEventTag (e :: MsgEncoding) where
  XMsgNew_ :: CMEventTag 'Json
  XMsgFileDescr_ :: CMEventTag 'Json
  XMsgUpdate_ :: CMEventTag 'Json
  XMsgDel_ :: CMEventTag 'Json
  XMsgDeleted_ :: CMEventTag 'Json
  XMsgReact_ :: CMEventTag 'Json
  XFile_ :: CMEventTag 'Json
  XFileAcpt_ :: CMEventTag 'Json
  XFileAcptInv_ :: CMEventTag 'Json
  XFileCancel_ :: CMEventTag 'Json
  XInfo_ :: CMEventTag 'Json
  XContact_ :: CMEventTag 'Json
  XDirectDel_ :: CMEventTag 'Json
  XGrpInv_ :: CMEventTag 'Json
  XGrpAcpt_ :: CMEventTag 'Json
  XGrpLinkInv_ :: CMEventTag 'Json
  XGrpLinkMem_ :: CMEventTag 'Json
  XGrpMemNew_ :: CMEventTag 'Json
  XGrpMemIntro_ :: CMEventTag 'Json
  XGrpMemInv_ :: CMEventTag 'Json
  XGrpMemFwd_ :: CMEventTag 'Json
  XGrpMemInfo_ :: CMEventTag 'Json
  XGrpMemRole_ :: CMEventTag 'Json
  XGrpMemCon_ :: CMEventTag 'Json
  XGrpMemConAll_ :: CMEventTag 'Json
  XGrpMemDel_ :: CMEventTag 'Json
  XGrpLeave_ :: CMEventTag 'Json
  XGrpDel_ :: CMEventTag 'Json
  XGrpInfo_ :: CMEventTag 'Json
  XGrpDirectInv_ :: CMEventTag 'Json
  XGrpMsgForward_ :: CMEventTag 'Json
  XInfoProbe_ :: CMEventTag 'Json
  XInfoProbeCheck_ :: CMEventTag 'Json
  XInfoProbeOk_ :: CMEventTag 'Json
  XCallInv_ :: CMEventTag 'Json
  XCallOffer_ :: CMEventTag 'Json
  XCallAnswer_ :: CMEventTag 'Json
  XCallExtra_ :: CMEventTag 'Json
  XCallEnd_ :: CMEventTag 'Json
  XOk_ :: CMEventTag 'Json
  XUnknown_ :: Text -> CMEventTag 'Json
  BFileChunk_ :: CMEventTag 'Binary

deriving instance Show (CMEventTag e)

deriving instance Eq (CMEventTag e)

instance MsgEncodingI e => StrEncoding (CMEventTag e) where
  strEncode = \case
    XMsgNew_ -> "x.msg.new"
    XMsgFileDescr_ -> "x.msg.file.descr"
    XMsgUpdate_ -> "x.msg.update"
    XMsgDel_ -> "x.msg.del"
    XMsgDeleted_ -> "x.msg.deleted"
    XMsgReact_ -> "x.msg.react"
    XFile_ -> "x.file"
    XFileAcpt_ -> "x.file.acpt"
    XFileAcptInv_ -> "x.file.acpt.inv"
    XFileCancel_ -> "x.file.cancel"
    XInfo_ -> "x.info"
    XContact_ -> "x.contact"
    XDirectDel_ -> "x.direct.del"
    XGrpInv_ -> "x.grp.inv"
    XGrpAcpt_ -> "x.grp.acpt"
    XGrpLinkInv_ -> "x.grp.link.inv"
    XGrpLinkMem_ -> "x.grp.link.mem"
    XGrpMemNew_ -> "x.grp.mem.new"
    XGrpMemIntro_ -> "x.grp.mem.intro"
    XGrpMemInv_ -> "x.grp.mem.inv"
    XGrpMemFwd_ -> "x.grp.mem.fwd"
    XGrpMemInfo_ -> "x.grp.mem.info"
    XGrpMemRole_ -> "x.grp.mem.role"
    XGrpMemCon_ -> "x.grp.mem.con"
    XGrpMemConAll_ -> "x.grp.mem.con.all"
    XGrpMemDel_ -> "x.grp.mem.del"
    XGrpLeave_ -> "x.grp.leave"
    XGrpDel_ -> "x.grp.del"
    XGrpInfo_ -> "x.grp.info"
    XGrpDirectInv_ -> "x.grp.direct.inv"
    XGrpMsgForward_ -> "x.grp.msg.forward"
    XInfoProbe_ -> "x.info.probe"
    XInfoProbeCheck_ -> "x.info.probe.check"
    XInfoProbeOk_ -> "x.info.probe.ok"
    XCallInv_ -> "x.call.inv"
    XCallOffer_ -> "x.call.offer"
    XCallAnswer_ -> "x.call.answer"
    XCallExtra_ -> "x.call.extra"
    XCallEnd_ -> "x.call.end"
    XOk_ -> "x.ok"
    XUnknown_ t -> encodeUtf8 t
    BFileChunk_ -> "F"
  strDecode = (\(ACMEventTag _ t) -> checkEncoding t) <=< strDecode
  strP = strDecode <$?> A.takeTill (== ' ')

instance StrEncoding ACMEventTag where
  strEncode (ACMEventTag _ t) = strEncode t
  strP =
    ((,) <$> A.peekChar' <*> A.takeTill (== ' ')) >>= \case
      ('x', t) -> pure . ACMEventTag SJson $ case t of
        "x.msg.new" -> XMsgNew_
        "x.msg.file.descr" -> XMsgFileDescr_
        "x.msg.update" -> XMsgUpdate_
        "x.msg.del" -> XMsgDel_
        "x.msg.deleted" -> XMsgDeleted_
        "x.msg.react" -> XMsgReact_
        "x.file" -> XFile_
        "x.file.acpt" -> XFileAcpt_
        "x.file.acpt.inv" -> XFileAcptInv_
        "x.file.cancel" -> XFileCancel_
        "x.info" -> XInfo_
        "x.contact" -> XContact_
        "x.direct.del" -> XDirectDel_
        "x.grp.inv" -> XGrpInv_
        "x.grp.acpt" -> XGrpAcpt_
        "x.grp.link.inv" -> XGrpLinkInv_
        "x.grp.link.mem" -> XGrpLinkMem_
        "x.grp.mem.new" -> XGrpMemNew_
        "x.grp.mem.intro" -> XGrpMemIntro_
        "x.grp.mem.inv" -> XGrpMemInv_
        "x.grp.mem.fwd" -> XGrpMemFwd_
        "x.grp.mem.info" -> XGrpMemInfo_
        "x.grp.mem.role" -> XGrpMemRole_
        "x.grp.mem.con" -> XGrpMemCon_
        "x.grp.mem.con.all" -> XGrpMemConAll_
        "x.grp.mem.del" -> XGrpMemDel_
        "x.grp.leave" -> XGrpLeave_
        "x.grp.del" -> XGrpDel_
        "x.grp.info" -> XGrpInfo_
        "x.grp.direct.inv" -> XGrpDirectInv_
        "x.grp.msg.forward" -> XGrpMsgForward_
        "x.info.probe" -> XInfoProbe_
        "x.info.probe.check" -> XInfoProbeCheck_
        "x.info.probe.ok" -> XInfoProbeOk_
        "x.call.inv" -> XCallInv_
        "x.call.offer" -> XCallOffer_
        "x.call.answer" -> XCallAnswer_
        "x.call.extra" -> XCallExtra_
        "x.call.end" -> XCallEnd_
        "x.ok" -> XOk_
        _ -> XUnknown_ $ safeDecodeUtf8 t
      (_, "F") -> pure $ ACMEventTag SBinary BFileChunk_
      _ -> fail "bad ACMEventTag"

toCMEventTag :: ChatMsgEvent e -> CMEventTag e
toCMEventTag msg = case msg of
  XMsgNew _ -> XMsgNew_
  XMsgFileDescr _ _ -> XMsgFileDescr_
  XMsgUpdate {} -> XMsgUpdate_
  XMsgDel {} -> XMsgDel_
  XMsgDeleted -> XMsgDeleted_
  XMsgReact {} -> XMsgReact_
  XFile _ -> XFile_
  XFileAcpt _ -> XFileAcpt_
  XFileAcptInv {} -> XFileAcptInv_
  XFileCancel _ -> XFileCancel_
  XInfo _ -> XInfo_
  XContact _ _ -> XContact_
  XDirectDel -> XDirectDel_
  XGrpInv _ -> XGrpInv_
  XGrpAcpt _ -> XGrpAcpt_
  XGrpLinkInv _ -> XGrpLinkInv_
  XGrpLinkMem _ -> XGrpLinkMem_
  XGrpMemNew _ -> XGrpMemNew_
  XGrpMemIntro _ -> XGrpMemIntro_
  XGrpMemInv _ _ -> XGrpMemInv_
  XGrpMemFwd _ _ -> XGrpMemFwd_
  XGrpMemInfo _ _ -> XGrpMemInfo_
  XGrpMemRole _ _ -> XGrpMemRole_
  XGrpMemCon _ -> XGrpMemCon_
  XGrpMemConAll _ -> XGrpMemConAll_
  XGrpMemDel _ -> XGrpMemDel_
  XGrpLeave -> XGrpLeave_
  XGrpDel -> XGrpDel_
  XGrpInfo _ -> XGrpInfo_
  XGrpDirectInv _ _ -> XGrpDirectInv_
  XGrpMsgForward {} -> XGrpMsgForward_
  XInfoProbe _ -> XInfoProbe_
  XInfoProbeCheck _ -> XInfoProbeCheck_
  XInfoProbeOk _ -> XInfoProbeOk_
  XCallInv _ _ -> XCallInv_
  XCallOffer _ _ -> XCallOffer_
  XCallAnswer _ _ -> XCallAnswer_
  XCallExtra _ _ -> XCallExtra_
  XCallEnd _ -> XCallEnd_
  XOk -> XOk_
  XUnknown t _ -> XUnknown_ t
  BFileChunk _ _ -> BFileChunk_

instance MsgEncodingI e => TextEncoding (CMEventTag e) where
  textEncode = decodeLatin1 . strEncode
  textDecode = eitherToMaybe . strDecode . encodeUtf8

instance TextEncoding ACMEventTag where
  textEncode (ACMEventTag _ t) = textEncode t
  textDecode = eitherToMaybe . strDecode . encodeUtf8

instance (MsgEncodingI e, Typeable e) => FromField (CMEventTag e) where fromField = fromTextField_ textDecode

instance MsgEncodingI e => ToField (CMEventTag e) where toField = toField . textEncode

instance FromField ACMEventTag where fromField = fromTextField_ textDecode

instance ToField ACMEventTag where toField = toField . textEncode

hasNotification :: CMEventTag e -> Bool
hasNotification = \case
  XMsgNew_ -> True
  XFile_ -> True
  XContact_ -> True
  XGrpInv_ -> True
  XGrpMemFwd_ -> True
  XGrpDel_ -> True
  XCallInv_ -> True
  _ -> False

hasDeliveryReceipt :: CMEventTag e -> Bool
hasDeliveryReceipt = \case
  XMsgNew_ -> True
  XGrpInv_ -> True
  XCallInv_ -> True
  _ -> False

appBinaryToCM :: AppMessageBinary -> Either String (ChatMessage 'Binary)
appBinaryToCM AppMessageBinary {msgId, tag, body} = do
  eventTag <- strDecode $ B.singleton tag
  chatMsgEvent <- parseAll (msg eventTag) body
  pure ChatMessage {chatVRange = chatInitialVRange, msgId, chatMsgEvent}
  where
    msg :: CMEventTag 'Binary -> A.Parser (ChatMsgEvent 'Binary)
    msg = \case
      BFileChunk_ -> BFileChunk <$> (SharedMsgId <$> smpP) <*> (unIFC <$> smpP)

appJsonToCM :: AppMessageJson -> Either String (ChatMessage 'Json)
appJsonToCM AppMessageJson {v, msgId, event, params} = do
  eventTag <- strDecode $ encodeUtf8 event
  chatMsgEvent <- msg eventTag
  pure ChatMessage {chatVRange = maybe chatInitialVRange fromChatVRange v, msgId, chatMsgEvent}
  where
    p :: FromJSON a => J.Key -> Either String a
    p key = JT.parseEither (.: key) params
    opt :: FromJSON a => J.Key -> Either String (Maybe a)
    opt key = JT.parseEither (.:? key) params
    msg :: CMEventTag 'Json -> Either String (ChatMsgEvent 'Json)
    msg = \case
      XMsgNew_ -> XMsgNew <$> JT.parseEither parseMsgContainer params
      XMsgFileDescr_ -> XMsgFileDescr <$> p "msgId" <*> p "fileDescr"
      XMsgUpdate_ -> XMsgUpdate <$> p "msgId" <*> p "content" <*> opt "ttl" <*> opt "live"
      XMsgDel_ -> XMsgDel <$> p "msgId" <*> opt "memberId"
      XMsgDeleted_ -> pure XMsgDeleted
      XMsgReact_ -> XMsgReact <$> p "msgId" <*> opt "memberId" <*> p "reaction" <*> p "add"
      XFile_ -> XFile <$> p "file"
      XFileAcpt_ -> XFileAcpt <$> p "fileName"
      XFileAcptInv_ -> XFileAcptInv <$> p "msgId" <*> opt "fileConnReq" <*> p "fileName"
      XFileCancel_ -> XFileCancel <$> p "msgId"
      XInfo_ -> XInfo <$> p "profile"
      XContact_ -> XContact <$> p "profile" <*> opt "contactReqId"
      XDirectDel_ -> pure XDirectDel
      XGrpInv_ -> XGrpInv <$> p "groupInvitation"
      XGrpAcpt_ -> XGrpAcpt <$> p "memberId"
      XGrpLinkInv_ -> XGrpLinkInv <$> p "groupLinkInvitation"
      XGrpLinkMem_ -> XGrpLinkMem <$> p "profile"
      XGrpMemNew_ -> XGrpMemNew <$> p "memberInfo"
      XGrpMemIntro_ -> XGrpMemIntro <$> p "memberInfo"
      XGrpMemInv_ -> XGrpMemInv <$> p "memberId" <*> p "memberIntro"
      XGrpMemFwd_ -> XGrpMemFwd <$> p "memberInfo" <*> p "memberIntro"
      XGrpMemInfo_ -> XGrpMemInfo <$> p "memberId" <*> p "profile"
      XGrpMemRole_ -> XGrpMemRole <$> p "memberId" <*> p "role"
      XGrpMemCon_ -> XGrpMemCon <$> p "memberId"
      XGrpMemConAll_ -> XGrpMemConAll <$> p "memberId"
      XGrpMemDel_ -> XGrpMemDel <$> p "memberId"
      XGrpLeave_ -> pure XGrpLeave
      XGrpDel_ -> pure XGrpDel
      XGrpInfo_ -> XGrpInfo <$> p "groupProfile"
      XGrpDirectInv_ -> XGrpDirectInv <$> p "connReq" <*> opt "content"
      XGrpMsgForward_ -> XGrpMsgForward <$> p "memberId" <*> p "msg" <*> p "msgTs"
      XInfoProbe_ -> XInfoProbe <$> p "probe"
      XInfoProbeCheck_ -> XInfoProbeCheck <$> p "probeHash"
      XInfoProbeOk_ -> XInfoProbeOk <$> p "probe"
      XCallInv_ -> XCallInv <$> p "callId" <*> p "invitation"
      XCallOffer_ -> XCallOffer <$> p "callId" <*> p "offer"
      XCallAnswer_ -> XCallAnswer <$> p "callId" <*> p "answer"
      XCallExtra_ -> XCallExtra <$> p "callId" <*> p "extra"
      XCallEnd_ -> XCallEnd <$> p "callId"
      XOk_ -> pure XOk
      XUnknown_ t -> pure $ XUnknown t params

(.=?) :: ToJSON v => JT.Key -> Maybe v -> [(J.Key, J.Value)] -> [(J.Key, J.Value)]
key .=? value = maybe id ((:) . (key .=)) value

chatToAppMessage :: forall e. MsgEncodingI e => ChatMessage e -> AppMessage e
chatToAppMessage ChatMessage {chatVRange, msgId, chatMsgEvent} = case encoding @e of
  SBinary ->
    let (binaryMsgId, body) = toBody chatMsgEvent
     in AMBinary AppMessageBinary {msgId = binaryMsgId, tag = B.head $ strEncode tag, body}
  SJson -> AMJson AppMessageJson {v = Just $ ChatVersionRange chatVRange, msgId, event = textEncode tag, params = params chatMsgEvent}
  where
    tag = toCMEventTag chatMsgEvent
    o :: [(J.Key, J.Value)] -> J.Object
    o = JM.fromList
    toBody :: ChatMsgEvent 'Binary -> (Maybe SharedMsgId, ByteString)
    toBody = \case
      BFileChunk (SharedMsgId msgId') chunk -> (Nothing, smpEncode (msgId', IFC chunk))
    params :: ChatMsgEvent 'Json -> J.Object
    params = \case
      XMsgNew container -> msgContainerJSON container
      XMsgFileDescr msgId' fileDescr -> o ["msgId" .= msgId', "fileDescr" .= fileDescr]
      XMsgUpdate msgId' content ttl live -> o $ ("ttl" .=? ttl) $ ("live" .=? live) ["msgId" .= msgId', "content" .= content]
      XMsgDel msgId' memberId -> o $ ("memberId" .=? memberId) ["msgId" .= msgId']
      XMsgDeleted -> JM.empty
      XMsgReact msgId' memberId reaction add -> o $ ("memberId" .=? memberId) ["msgId" .= msgId', "reaction" .= reaction, "add" .= add]
      XFile fileInv -> o ["file" .= fileInv]
      XFileAcpt fileName -> o ["fileName" .= fileName]
      XFileAcptInv sharedMsgId fileConnReq fileName -> o $ ("fileConnReq" .=? fileConnReq) ["msgId" .= sharedMsgId, "fileName" .= fileName]
      XFileCancel sharedMsgId -> o ["msgId" .= sharedMsgId]
      XInfo profile -> o ["profile" .= profile]
      XContact profile xContactId -> o $ ("contactReqId" .=? xContactId) ["profile" .= profile]
      XDirectDel -> JM.empty
      XGrpInv groupInv -> o ["groupInvitation" .= groupInv]
      XGrpAcpt memId -> o ["memberId" .= memId]
      XGrpLinkInv groupLinkInv -> o ["groupLinkInvitation" .= groupLinkInv]
      XGrpLinkMem profile -> o ["profile" .= profile]
      XGrpMemNew memInfo -> o ["memberInfo" .= memInfo]
      XGrpMemIntro memInfo -> o ["memberInfo" .= memInfo]
      XGrpMemInv memId memIntro -> o ["memberId" .= memId, "memberIntro" .= memIntro]
      XGrpMemFwd memInfo memIntro -> o ["memberInfo" .= memInfo, "memberIntro" .= memIntro]
      XGrpMemInfo memId profile -> o ["memberId" .= memId, "profile" .= profile]
      XGrpMemRole memId role -> o ["memberId" .= memId, "role" .= role]
      XGrpMemCon memId -> o ["memberId" .= memId]
      XGrpMemConAll memId -> o ["memberId" .= memId]
      XGrpMemDel memId -> o ["memberId" .= memId]
      XGrpLeave -> JM.empty
      XGrpDel -> JM.empty
      XGrpInfo p -> o ["groupProfile" .= p]
      XGrpDirectInv connReq content -> o $ ("content" .=? content) ["connReq" .= connReq]
      XGrpMsgForward memberId msg msgTs -> o ["memberId" .= memberId, "msg" .= msg, "msgTs" .= msgTs]
      XInfoProbe probe -> o ["probe" .= probe]
      XInfoProbeCheck probeHash -> o ["probeHash" .= probeHash]
      XInfoProbeOk probe -> o ["probe" .= probe]
      XCallInv callId inv -> o ["callId" .= callId, "invitation" .= inv]
      XCallOffer callId offer -> o ["callId" .= callId, "offer" .= offer]
      XCallAnswer callId answer -> o ["callId" .= callId, "answer" .= answer]
      XCallExtra callId extra -> o ["callId" .= callId, "extra" .= extra]
      XCallEnd callId -> o ["callId" .= callId]
      XOk -> JM.empty
      XUnknown _ ps -> ps

instance ToJSON (ChatMessage 'Json) where
  toJSON = (\(AMJson msg) -> toJSON msg) . chatToAppMessage

instance FromJSON (ChatMessage 'Json) where
  parseJSON v = appJsonToCM <$?> parseJSON v
