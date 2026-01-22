{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Either (fromRight)
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Type.Equality
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Simplex.Chat.Call
import Simplex.Chat.Options.DB (FromField (..), ToField (..))
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Messaging.Agent.Protocol (VersionSMPA, pqdrSMPAgentVersion)
import Simplex.Messaging.Agent.Store.DB (blobFieldDecoder, fromTextField_)
import Simplex.Messaging.Compression (Compressed, compress1, decompress1)
import Simplex.Messaging.Encoding
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, fstToLower, parseAll, sumTypeJSON, taggedObjectJSON)
import Simplex.Messaging.Protocol (MsgBody)
import Simplex.Messaging.Util (decodeJSON, eitherToMaybe, encodeJSON, safeDecodeUtf8, (<$?>))
import Simplex.Messaging.Version hiding (version)

-- Chat version history:
-- 1 - support chat versions in connections (9/1/2023)
-- 2 - create contacts for group members only via x.grp.direct.inv (9/16/2023)
-- 3 - faster joining via group links without creating contact (10/30/2023)
-- 4 - group message forwarding (11/18/2023)
-- 5 - batch sending messages (12/23/2023)
-- 6 - send group welcome message after history (12/29/2023)
-- 7 - update member profiles (1/15/2024)
-- 8 - compress messages and PQ e2e encryption (2024-03-08)
-- 9 - batch sending in direct connections (2024-07-24)
-- 10 - business chats (2024-11-29)
-- 11 - fix profile update in business chats (2024-12-05)
-- 12 - support sending and receiving content reports (2025-01-03)
-- 14 - support sending and receiving group join rejection (2025-02-24)
-- 15 - support specifying message scopes for group messages (2025-03-12)
-- 16 - support short link data (2025-06-10)

-- This should not be used directly in code, instead use `maxVersion chatVRange` from ChatConfig.
-- This indirection is needed for backward/forward compatibility testing.
-- Testing with real app versions is still needed, as tests use the current code with different version ranges, not the old code.
currentChatVersion :: VersionChat
currentChatVersion = VersionChat 16

-- This should not be used directly in code, instead use `chatVRange` from ChatConfig (see comment above)
supportedChatVRange :: VersionRangeChat
supportedChatVRange = mkVersionRange initialChatVersion currentChatVersion
{-# INLINE supportedChatVRange #-}

-- version range that supports skipping establishing direct connections in a group and establishing direct connection via x.grp.direct.inv
groupDirectInvVersion :: VersionChat
groupDirectInvVersion = VersionChat 2

-- version range that supports joining group via group link without creating direct contact
groupFastLinkJoinVersion :: VersionChat
groupFastLinkJoinVersion = VersionChat 3

-- version range that supports group forwarding
groupForwardVersion :: VersionChat
groupForwardVersion = VersionChat 4

-- version range that supports batch sending in groups
batchSendVersion :: VersionChat
batchSendVersion = VersionChat 5

-- version range that supports sending group welcome message in group history
groupHistoryIncludeWelcomeVersion :: VersionChat
groupHistoryIncludeWelcomeVersion = VersionChat 6

-- version range that supports sending member profile updates to groups
memberProfileUpdateVersion :: VersionChat
memberProfileUpdateVersion = VersionChat 7

-- version range that supports compressing messages and PQ e2e encryption
pqEncryptionCompressionVersion :: VersionChat
pqEncryptionCompressionVersion = VersionChat 8

-- version range that supports batch sending in direct connections, and forwarding batched messages in groups
batchSend2Version :: VersionChat
batchSend2Version = VersionChat 9

-- supports differentiating business chats when joining contact addresses
businessChatsVersion :: VersionChat
businessChatsVersion = VersionChat 10

-- support updating preferences in business chats (XGrpPrefs message)
businessChatPrefsVersion :: VersionChat
businessChatPrefsVersion = VersionChat 11

-- support sending and receiving content reports (MCReport message content)
contentReportsVersion :: VersionChat
contentReportsVersion = VersionChat 12

-- support sending and receiving group join rejection (XGrpLinkReject)
groupJoinRejectVersion :: VersionChat
groupJoinRejectVersion = VersionChat 14

-- support group knocking (MsgScope)
groupKnockingVersion :: VersionChat
groupKnockingVersion = VersionChat 15

-- support short link data in invitation, contact and group links
shortLinkDataVersion :: VersionChat
shortLinkDataVersion = VersionChat 16

agentToChatVersion :: VersionSMPA -> VersionChat
agentToChatVersion v
  | v < pqdrSMPAgentVersion = initialChatVersion
  | otherwise = pqEncryptionCompressionVersion

data ConnectionEntity
  = RcvDirectMsgConnection {entityConnection :: Connection, contact :: Maybe Contact}
  | RcvGroupMsgConnection {entityConnection :: Connection, groupInfo :: GroupInfo, groupMember :: GroupMember}
  | UserContactConnection {entityConnection :: Connection, userContact :: UserContact}
  deriving (Eq, Show)

$(JQ.deriveJSON (sumTypeJSON fstToLower) ''ConnectionEntity)

connEntityInfo :: ConnectionEntity -> String
connEntityInfo = \case
  RcvDirectMsgConnection c ct_ -> ctInfo ct_ <> ", status: " <> show (connStatus c)
  RcvGroupMsgConnection c g m -> mInfo g m <> ", status: " <> show (connStatus c)
  UserContactConnection c _uc -> "user address, status: " <> show (connStatus c)
  where
    ctInfo = maybe "connection" $ \Contact {contactId} -> "contact " <> show contactId
    mInfo GroupInfo {groupId} GroupMember {groupMemberId} = "group " <> show groupId <> ", member " <> show groupMemberId

updateEntityConnStatus :: ConnectionEntity -> ConnStatus -> ConnectionEntity
updateEntityConnStatus connEntity connStatus = case connEntity of
  RcvDirectMsgConnection c ct_ -> RcvDirectMsgConnection (st c) ((\ct -> (ct :: Contact) {activeConn = Just $ st c}) <$> ct_)
  RcvGroupMsgConnection c gInfo m@GroupMember {activeConn = c'} -> RcvGroupMsgConnection (st c) gInfo m {activeConn = st <$> c'}
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

data MsgScope = MSMember {memberId :: MemberId} -- Admins can use any member id; members can use only their own id
  deriving (Eq, Show)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "MS") ''MsgScope)

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

data ReportReason = RRSpam | RRContent | RRCommunity | RRProfile | RROther | RRUnknown Text
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

instance StrEncoding ReportReason where
  strEncode = \case
    RRSpam -> "spam"
    RRContent -> "content"
    RRCommunity -> "community"
    RRProfile -> "profile"
    RROther -> "other"
    RRUnknown t -> encodeUtf8 t
  strP =
    A.takeTill (== ' ') >>= \case
      "spam" -> pure RRSpam
      "content" -> pure RRContent
      "community" -> pure RRCommunity
      "profile" -> pure RRProfile
      "other" -> pure RROther
      t -> pure $ RRUnknown $ safeDecodeUtf8 t

instance FromJSON ReportReason where
  parseJSON = strParseJSON "ReportReason"

instance ToJSON ReportReason where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data ChatMessage e = ChatMessage
  { chatVRange :: VersionRangeChat,
    msgId :: Maybe SharedMsgId,
    chatMsgEvent :: ChatMsgEvent e
  }
  deriving (Eq, Show)

data AChatMessage = forall e. MsgEncodingI e => ACMsg (SMsgEncoding e) (ChatMessage e)

type MessageFromChannel = Bool

data ChatMsgEvent (e :: MsgEncoding) where
  XMsgNew :: MsgContainer -> ChatMsgEvent 'Json
  XMsgFileDescr :: {msgId :: SharedMsgId, fileDescr :: FileDescr} -> ChatMsgEvent 'Json
  XMsgUpdate :: {msgId :: SharedMsgId, content :: MsgContent, mentions :: Map MemberName MsgMention, ttl :: Maybe Int, live :: Maybe Bool, scope :: Maybe MsgScope} -> ChatMsgEvent 'Json
  XMsgDel :: {msgId :: SharedMsgId, memberId :: Maybe MemberId, scope :: Maybe MsgScope} -> ChatMsgEvent 'Json
  XMsgDeleted :: ChatMsgEvent 'Json
  XMsgReact :: {msgId :: SharedMsgId, memberId :: Maybe MemberId, scope :: Maybe MsgScope, reaction :: MsgReaction, add :: Bool} -> ChatMsgEvent 'Json
  XFile :: FileInvitation -> ChatMsgEvent 'Json -- TODO discontinue
  XFileAcpt :: String -> ChatMsgEvent 'Json -- direct file protocol
  XFileAcptInv :: SharedMsgId -> Maybe ConnReqInvitation -> String -> ChatMsgEvent 'Json
  XFileCancel :: SharedMsgId -> ChatMsgEvent 'Json
  XInfo :: Profile -> ChatMsgEvent 'Json
  XContact :: {profile :: Profile, contactReqId :: Maybe XContactId, welcomeMsgId :: Maybe SharedMsgId, requestMsg :: Maybe (SharedMsgId, MsgContent)} -> ChatMsgEvent 'Json
  XMember :: {profile :: Profile, newMemberId :: MemberId} -> ChatMsgEvent 'Json
  XDirectDel :: ChatMsgEvent 'Json
  XGrpInv :: GroupInvitation -> ChatMsgEvent 'Json
  XGrpAcpt :: MemberId -> ChatMsgEvent 'Json
  XGrpLinkInv :: GroupLinkInvitation -> ChatMsgEvent 'Json
  XGrpLinkReject :: GroupLinkRejection -> ChatMsgEvent 'Json
  XGrpLinkMem :: Profile -> ChatMsgEvent 'Json
  XGrpLinkAcpt :: GroupAcceptance -> GroupMemberRole -> MemberId -> ChatMsgEvent 'Json
  XGrpRelayInv :: GroupRelayInvitation -> ChatMsgEvent 'Json
  XGrpRelayAcpt :: ShortLinkContact -> ChatMsgEvent 'Json
  XGrpMemNew :: MemberInfo -> Maybe MsgScope -> ChatMsgEvent 'Json
  XGrpMemIntro :: MemberInfo -> Maybe MemberRestrictions -> ChatMsgEvent 'Json
  XGrpMemInv :: MemberId -> IntroInvitation -> ChatMsgEvent 'Json
  XGrpMemFwd :: MemberInfo -> IntroInvitation -> ChatMsgEvent 'Json
  XGrpMemInfo :: MemberId -> Profile -> ChatMsgEvent 'Json
  XGrpMemRole :: MemberId -> GroupMemberRole -> ChatMsgEvent 'Json
  XGrpMemRestrict :: MemberId -> MemberRestrictions -> ChatMsgEvent 'Json
  XGrpMemCon :: MemberId -> ChatMsgEvent 'Json
  XGrpMemConAll :: MemberId -> ChatMsgEvent 'Json -- TODO not implemented
  XGrpMemDel :: MemberId -> Bool -> ChatMsgEvent 'Json
  XGrpLeave :: ChatMsgEvent 'Json
  XGrpDel :: ChatMsgEvent 'Json
  XGrpInfo :: GroupProfile -> ChatMsgEvent 'Json
  XGrpPrefs :: GroupPreferences -> ChatMsgEvent 'Json
  XGrpDirectInv :: ConnReqInvitation -> Maybe MsgContent -> Maybe MsgScope -> ChatMsgEvent 'Json
  XGrpMsgForward :: Maybe MemberId -> Maybe ContactName -> ChatMessage 'Json -> UTCTime -> ChatMsgEvent 'Json
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

-- when sending, used for deciding whether message will be forwarded by host or not (memberSendAction);
-- actual filtering on forwarding is done in processEvent
isForwardedGroupMsg :: ChatMsgEvent e -> Bool
isForwardedGroupMsg ev = case ev of
  XMsgNew mc -> case mcExtMsgContent mc of
    ExtMsgContent {file = Just FileInvitation {fileInline = Just _}} -> False
    _ -> True
  XMsgFileDescr _ _ -> True
  XMsgUpdate {} -> True
  XMsgDel {} -> True
  XMsgReact {} -> True
  XFileCancel _ -> True
  XInfo _ -> True
  XGrpMemNew {} -> True
  XGrpMemRole {} -> True
  XGrpMemRestrict {} -> True
  XGrpMemDel {} -> True
  XGrpLeave -> True
  XGrpDel -> True
  XGrpInfo _ -> True
  XGrpPrefs _ -> True
  _ -> False

data MsgReaction = MREmoji {emoji :: MREmojiChar} | MRUnknown {tag :: Text, json :: J.Object}
  deriving (Eq, Show)

emojiTag :: IsString a => a
emojiTag = "emoji"

knownReaction :: MsgReaction -> Either String MsgReaction
knownReaction = \case
  r@MREmoji {} -> Right r
  MRUnknown {} -> Left "unknown MsgReaction"

-- parseJSON for MsgReaction parses unknown emoji reactions as MRUnknown with type "emoji",
-- allowing to add new emojis in a backwards compatible way - UI shows them as ?
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
  | c `elem` ("👍👎😀😂😢❤️🚀✅" :: String) = Right $ MREmojiChar c
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

data MsgContentTag
  = MCText_
  | MCLink_
  | MCImage_
  | MCVideo_
  | MCVoice_
  | MCFile_
  | MCReport_
  | MCChat_
  | MCUnknown_ Text
  deriving (Eq, Show)

instance StrEncoding MsgContentTag where
  strEncode = \case
    MCText_ -> "text"
    MCLink_ -> "link"
    MCImage_ -> "image"
    MCVideo_ -> "video"
    MCFile_ -> "file"
    MCVoice_ -> "voice"
    MCReport_ -> "report"
    MCChat_ -> "chat"
    MCUnknown_ t -> encodeUtf8 t
  strDecode = \case
    "text" -> Right MCText_
    "link" -> Right MCLink_
    "image" -> Right MCImage_
    "video" -> Right MCVideo_
    "voice" -> Right MCVoice_
    "file" -> Right MCFile_
    "report" -> Right MCReport_
    "chat" -> Right MCChat_
    t -> Right . MCUnknown_ $ safeDecodeUtf8 t
  strP = strDecode <$?> A.takeTill (== ' ')

instance FromJSON MsgContentTag where
  parseJSON = strParseJSON "MsgContentType"

instance ToJSON MsgContentTag where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromField MsgContentTag where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

instance ToField MsgContentTag where toField = toField . safeDecodeUtf8 . strEncode

data MsgContainer
  = MCSimple ExtMsgContent
  | MCQuote QuotedMsg ExtMsgContent
  | MCComment MsgRef ExtMsgContent
  | MCForward ExtMsgContent
  deriving (Eq, Show)

mcExtMsgContent :: MsgContainer -> ExtMsgContent
mcExtMsgContent = \case
  MCSimple c -> c
  MCQuote _ c -> c
  MCComment _ c -> c
  MCForward c -> c

isMCForward :: MsgContainer -> Bool
isMCForward = \case
  MCForward _ -> True
  _ -> False

data MsgContent
  = MCText {text :: Text}
  | MCLink {text :: Text, preview :: LinkPreview}
  | MCImage {text :: Text, image :: ImageData}
  | MCVideo {text :: Text, image :: ImageData, duration :: Int}
  | MCVoice {text :: Text, duration :: Int}
  | MCFile {text :: Text}
  | MCReport {text :: Text, reason :: ReportReason}
  | MCChat {text :: Text, chatLink :: MsgChatLink}
  | MCUnknown {tag :: Text, text :: Text, json :: J.Object}
  deriving (Eq, Show)

data MsgChatLink
  = MCLContact {connLink :: ShortLinkContact, profile :: Profile, business :: Bool}
  | MCLInvitation {invLink :: ShortLinkInvitation, profile :: Profile}
  | MCLGroup {connLink :: ShortLinkContact, groupProfile :: GroupProfile}
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
  MCReport {text, reason} ->
    if T.null text then msg else msg <> ": " <> text
    where
      msg = "report " <> safeDecodeUtf8 (strEncode reason)
  MCChat {text} -> text
  MCUnknown {text} -> text

durationText :: Int -> Text
durationText duration =
  let (mins, secs) = duration `divMod` 60 in T.pack $ "(" <> with0 mins <> ":" <> with0 secs <> ")"
  where
    with0 n
      | n <= 9 = '0' : show n
      | otherwise = show n

msgContentHasText :: MsgContent -> Bool
msgContentHasText =
  not . T.null . \case
    MCVoice {text} -> text
    mc -> msgContentText mc

isVoice :: MsgContent -> Bool
isVoice = \case
  MCVoice {} -> True
  _ -> False

isReport :: MsgContent -> Bool
isReport = \case
  MCReport {} -> True
  _ -> False

msgContentTag :: MsgContent -> MsgContentTag
msgContentTag = \case
  MCText _ -> MCText_
  MCLink {} -> MCLink_
  MCImage {} -> MCImage_
  MCVideo {} -> MCVideo_
  MCVoice {} -> MCVoice_
  MCFile {} -> MCFile_
  MCReport {} -> MCReport_
  MCChat {} -> MCChat_
  MCUnknown {tag} -> MCUnknown_ tag

data ExtMsgContent = ExtMsgContent
  { content :: MsgContent,
    -- the key used in mentions is a locally (per message) unique display name of member.
    -- Suffixes _1, _2 should be appended to make names locally unique.
    -- It should be done in the UI, as they will be part of the text, and validated in the API.
    mentions :: Map MemberName MsgMention,
    file :: Maybe FileInvitation,
    ttl :: Maybe Int,
    live :: Maybe Bool,
    scope :: Maybe MsgScope
  }
  deriving (Eq, Show)

data MsgMention = MsgMention {memberId :: MemberId}
  deriving (Eq, Show)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "MCL") ''MsgChatLink)

$(JQ.deriveJSON defaultJSON ''MsgMention)

$(JQ.deriveJSON defaultJSON ''QuotedMsg)

-- this limit reserves space for metadata in forwarded messages
-- 15780 (limit used for fileChunkSize) - 161 (x.grp.msg.forward overhead) = 15619, - 16 for block encryption ("rounded" to 15602)
maxEncodedMsgLength :: Int
maxEncodedMsgLength = 15602

-- maxEncodedMsgLength - 2222, see e2eEncUserMsgLength in agent
maxCompressedMsgLength :: Int
maxCompressedMsgLength = 13380

-- maxEncodedMsgLength - delta between MSG and INFO + 100 (returned for forward overhead)
-- delta between MSG and INFO = e2eEncUserMsgLength (no PQ) - e2eEncConnInfoLength (no PQ) = 1008
maxEncodedInfoLength :: Int
maxEncodedInfoLength = 14694

maxCompressedInfoLength :: Int
maxCompressedInfoLength = 10968 -- maxEncodedInfoLength - 3726, see e2eEncConnInfoLength in agent

data EncodedChatMessage = ECMEncoded ByteString | ECMLarge

encodeChatMessage :: MsgEncodingI e => Int -> ChatMessage e -> EncodedChatMessage
encodeChatMessage maxSize msg = do
  case chatToAppMessage msg of
    AMJson m -> do
      let body = LB.toStrict $ J.encode m
      if B.length body > maxSize
        then ECMLarge
        else ECMEncoded body
    AMBinary m -> ECMEncoded $ strEncode m

parseChatMessages :: ByteString -> [Either String AChatMessage]
parseChatMessages "" = [Left "empty string"]
parseChatMessages s = case B.head s of
  '{' -> [ACMsg SJson <$> J.eitherDecodeStrict' s]
  '[' -> case J.eitherDecodeStrict' s of
    Right v -> map parseItem v
    Left e -> [Left e]
  'X' -> decodeCompressed (B.drop 1 s)
  _ -> [ACMsg SBinary <$> (appBinaryToCM =<< strDecode s)]
  where
    parseItem :: J.Value -> Either String AChatMessage
    parseItem v = ACMsg SJson <$> JT.parseEither parseJSON v
    decodeCompressed :: ByteString -> [Either String AChatMessage]
    decodeCompressed s' = case smpDecode s' of
      Left e -> [Left e]
      Right (compressed :: L.NonEmpty Compressed) -> concatMap (either (pure . Left) parseChatMessages . decompress1) compressed

compressedBatchMsgBody_ :: MsgBody -> ByteString
compressedBatchMsgBody_ = markCompressedBatch . smpEncode . (L.:| []) . compress1

markCompressedBatch :: ByteString -> ByteString
markCompressedBatch = B.cons 'X'
{-# INLINE markCompressedBatch #-}

parseMsgContainer :: J.Object -> JT.Parser MsgContainer
parseMsgContainer v =
  MCQuote <$> v .: "quote" <*> mc
    <|> MCComment <$> v .: "parent" <*> mc
    <|> (v .: "forward" >>= \f -> (if f then MCForward else MCSimple) <$> mc)
    -- The support for arbitrary object in "forward" property is added to allow
    -- forward compatibility with forwards that include public group links.
    <|> (MCForward <$> ((v .: "forward" :: JT.Parser J.Object) *> mc))
    <|> MCSimple <$> mc
  where
    mc = do
      content <- v .: "content"
      file <- v .:? "file"
      ttl <- v .:? "ttl"
      live <- v .:? "live"
      mentions <- fromMaybe M.empty <$> (v .:? "mentions")
      scope <- v .:? "scope"
      pure ExtMsgContent {content, mentions, file, ttl, live, scope}

extMsgContent :: MsgContent -> Maybe FileInvitation -> ExtMsgContent
extMsgContent mc file = ExtMsgContent mc M.empty file Nothing Nothing Nothing

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
      MCReport_ -> do
        text <- v .: "text"
        reason <- v .: "reason"
        pure MCReport {text, reason}
      MCChat_ -> do
        text <- v .: "text"
        chatLink <- v .: "chatLink"
        pure MCChat {text, chatLink}
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
  MCComment ref mc -> o $ ("parent" .= ref) : msgContent mc
  MCForward mc -> o $ ("forward" .= True) : msgContent mc
  MCSimple mc -> o $ msgContent mc
  where
    o = JM.fromList
    msgContent ExtMsgContent {content, mentions, file, ttl, live, scope} =
      ("file" .=? file) $ ("ttl" .=? ttl) $ ("live" .=? live) $ ("mentions" .=? nonEmptyMap mentions) $ ("scope" .=? scope) ["content" .= content]

nonEmptyMap :: Map k v -> Maybe (Map k v)
nonEmptyMap m = if M.null m then Nothing else Just m
{-# INLINE nonEmptyMap #-}

instance ToJSON MsgContent where
  toJSON = \case
    MCUnknown {json} -> J.Object json
    MCText t -> J.object ["type" .= MCText_, "text" .= t]
    MCLink {text, preview} -> J.object ["type" .= MCLink_, "text" .= text, "preview" .= preview]
    MCImage {text, image} -> J.object ["type" .= MCImage_, "text" .= text, "image" .= image]
    MCVideo {text, image, duration} -> J.object ["type" .= MCVideo_, "text" .= text, "image" .= image, "duration" .= duration]
    MCVoice {text, duration} -> J.object ["type" .= MCVoice_, "text" .= text, "duration" .= duration]
    MCFile t -> J.object ["type" .= MCFile_, "text" .= t]
    MCReport {text, reason} -> J.object ["type" .= MCReport_, "text" .= text, "reason" .= reason]
    MCChat {text, chatLink} -> J.object ["type" .= MCChat_, "text" .= text, "chatLink" .= chatLink]
  toEncoding = \case
    MCUnknown {json} -> JE.value $ J.Object json
    MCText t -> J.pairs $ "type" .= MCText_ <> "text" .= t
    MCLink {text, preview} -> J.pairs $ "type" .= MCLink_ <> "text" .= text <> "preview" .= preview
    MCImage {text, image} -> J.pairs $ "type" .= MCImage_ <> "text" .= text <> "image" .= image
    MCVideo {text, image, duration} -> J.pairs $ "type" .= MCVideo_ <> "text" .= text <> "image" .= image <> "duration" .= duration
    MCVoice {text, duration} -> J.pairs $ "type" .= MCVoice_ <> "text" .= text <> "duration" .= duration
    MCFile t -> J.pairs $ "type" .= MCFile_ <> "text" .= t
    MCReport {text, reason} -> J.pairs $ "type" .= MCReport_ <> "text" .= text <> "reason" .= reason
    MCChat {text, chatLink} -> J.pairs $ "type" .= MCChat_ <> "text" .= text <> "chatLink" .= chatLink

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
  XMember_ :: CMEventTag 'Json
  XDirectDel_ :: CMEventTag 'Json
  XGrpInv_ :: CMEventTag 'Json
  XGrpAcpt_ :: CMEventTag 'Json
  XGrpLinkInv_ :: CMEventTag 'Json
  XGrpLinkReject_ :: CMEventTag 'Json
  XGrpLinkMem_ :: CMEventTag 'Json
  XGrpLinkAcpt_ :: CMEventTag 'Json
  XGrpRelayInv_ :: CMEventTag 'Json
  XGrpRelayAcpt_ :: CMEventTag 'Json
  XGrpMemNew_ :: CMEventTag 'Json
  XGrpMemIntro_ :: CMEventTag 'Json
  XGrpMemInv_ :: CMEventTag 'Json
  XGrpMemFwd_ :: CMEventTag 'Json
  XGrpMemInfo_ :: CMEventTag 'Json
  XGrpMemRole_ :: CMEventTag 'Json
  XGrpMemRestrict_ :: CMEventTag 'Json
  XGrpMemCon_ :: CMEventTag 'Json
  XGrpMemConAll_ :: CMEventTag 'Json
  XGrpMemDel_ :: CMEventTag 'Json
  XGrpLeave_ :: CMEventTag 'Json
  XGrpDel_ :: CMEventTag 'Json
  XGrpInfo_ :: CMEventTag 'Json
  XGrpPrefs_ :: CMEventTag 'Json
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
    XMember_ -> "x.member"
    XDirectDel_ -> "x.direct.del"
    XGrpInv_ -> "x.grp.inv"
    XGrpAcpt_ -> "x.grp.acpt"
    XGrpLinkInv_ -> "x.grp.link.inv"
    XGrpLinkReject_ -> "x.grp.link.reject"
    XGrpLinkMem_ -> "x.grp.link.mem"
    XGrpLinkAcpt_ -> "x.grp.link.acpt"
    XGrpRelayInv_ -> "x.grp.relay.inv"
    XGrpRelayAcpt_ -> "x.grp.relay.acpt"
    XGrpMemNew_ -> "x.grp.mem.new"
    XGrpMemIntro_ -> "x.grp.mem.intro"
    XGrpMemInv_ -> "x.grp.mem.inv"
    XGrpMemFwd_ -> "x.grp.mem.fwd"
    XGrpMemInfo_ -> "x.grp.mem.info"
    XGrpMemRole_ -> "x.grp.mem.role"
    XGrpMemRestrict_ -> "x.grp.mem.restrict"
    XGrpMemCon_ -> "x.grp.mem.con"
    XGrpMemConAll_ -> "x.grp.mem.con.all"
    XGrpMemDel_ -> "x.grp.mem.del"
    XGrpLeave_ -> "x.grp.leave"
    XGrpDel_ -> "x.grp.del"
    XGrpInfo_ -> "x.grp.info"
    XGrpPrefs_ -> "x.grp.prefs"
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
        "x.member" -> XMember_
        "x.direct.del" -> XDirectDel_
        "x.grp.inv" -> XGrpInv_
        "x.grp.acpt" -> XGrpAcpt_
        "x.grp.link.inv" -> XGrpLinkInv_
        "x.grp.link.reject" -> XGrpLinkReject_
        "x.grp.link.mem" -> XGrpLinkMem_
        "x.grp.link.acpt" -> XGrpLinkAcpt_
        "x.grp.relay.inv" -> XGrpRelayInv_
        "x.grp.relay.acpt" -> XGrpRelayAcpt_
        "x.grp.mem.new" -> XGrpMemNew_
        "x.grp.mem.intro" -> XGrpMemIntro_
        "x.grp.mem.inv" -> XGrpMemInv_
        "x.grp.mem.fwd" -> XGrpMemFwd_
        "x.grp.mem.info" -> XGrpMemInfo_
        "x.grp.mem.role" -> XGrpMemRole_
        "x.grp.mem.restrict" -> XGrpMemRestrict_
        "x.grp.mem.con" -> XGrpMemCon_
        "x.grp.mem.con.all" -> XGrpMemConAll_
        "x.grp.mem.del" -> XGrpMemDel_
        "x.grp.leave" -> XGrpLeave_
        "x.grp.del" -> XGrpDel_
        "x.grp.info" -> XGrpInfo_
        "x.grp.prefs" -> XGrpPrefs_
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
  XContact {} -> XContact_
  XMember {} -> XMember_
  XDirectDel -> XDirectDel_
  XGrpInv _ -> XGrpInv_
  XGrpAcpt _ -> XGrpAcpt_
  XGrpLinkInv _ -> XGrpLinkInv_
  XGrpLinkReject _ -> XGrpLinkReject_
  XGrpLinkMem _ -> XGrpLinkMem_
  XGrpLinkAcpt {} -> XGrpLinkAcpt_
  XGrpRelayInv _ -> XGrpRelayInv_
  XGrpRelayAcpt _ -> XGrpRelayAcpt_
  XGrpMemNew {} -> XGrpMemNew_
  XGrpMemIntro _ _ -> XGrpMemIntro_
  XGrpMemInv _ _ -> XGrpMemInv_
  XGrpMemFwd _ _ -> XGrpMemFwd_
  XGrpMemInfo _ _ -> XGrpMemInfo_
  XGrpMemRole _ _ -> XGrpMemRole_
  XGrpMemRestrict _ _ -> XGrpMemRestrict_
  XGrpMemCon _ -> XGrpMemCon_
  XGrpMemConAll _ -> XGrpMemConAll_
  XGrpMemDel {} -> XGrpMemDel_
  XGrpLeave -> XGrpLeave_
  XGrpDel -> XGrpDel_
  XGrpInfo _ -> XGrpInfo_
  XGrpPrefs _ -> XGrpPrefs_
  XGrpDirectInv {} -> XGrpDirectInv_
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
      XMsgUpdate_ -> do
        msgId' <- p "msgId"
        content <- p "content"
        mentions <- fromMaybe M.empty <$> opt "mentions"
        ttl <- opt "ttl"
        live <- opt "live"
        scope <- opt "scope"
        pure XMsgUpdate {msgId = msgId', content, mentions, ttl, live, scope}
      XMsgDel_ -> XMsgDel <$> p "msgId" <*> opt "memberId" <*> opt "scope"
      XMsgDeleted_ -> pure XMsgDeleted
      XMsgReact_ -> XMsgReact <$> p "msgId" <*> opt "memberId" <*> opt "scope" <*> p "reaction" <*> p "add"
      XFile_ -> XFile <$> p "file"
      XFileAcpt_ -> XFileAcpt <$> p "fileName"
      XFileAcptInv_ -> XFileAcptInv <$> p "msgId" <*> opt "fileConnReq" <*> p "fileName"
      XFileCancel_ -> XFileCancel <$> p "msgId"
      XInfo_ -> XInfo <$> p "profile"
      XContact_ -> do
        profile <- p "profile"
        contactReqId <- opt "contactReqId"
        welcomeMsgId <- opt "welcomeMsgId"
        reqMsgId <- opt "msgId"
        reqContent <- opt "content"
        let requestMsg = (,) <$> reqMsgId <*> reqContent
        pure XContact {profile, contactReqId, welcomeMsgId, requestMsg}
      XMember_ -> XMember <$> p "profile" <*> p "newMemberId"
      XDirectDel_ -> pure XDirectDel
      XGrpInv_ -> XGrpInv <$> p "groupInvitation"
      XGrpAcpt_ -> XGrpAcpt <$> p "memberId"
      XGrpLinkInv_ -> XGrpLinkInv <$> p "groupLinkInvitation"
      XGrpLinkReject_ -> XGrpLinkReject <$> p "groupLinkRejection"
      XGrpLinkMem_ -> XGrpLinkMem <$> p "profile"
      XGrpLinkAcpt_ -> XGrpLinkAcpt <$> p "acceptance" <*> p "role" <*> p "memberId"
      XGrpRelayInv_ -> XGrpRelayInv <$> p "groupRelayInvitation"
      XGrpRelayAcpt_ -> XGrpRelayAcpt <$> p "relayLink"
      XGrpMemNew_ -> XGrpMemNew <$> p "memberInfo" <*> opt "scope"
      XGrpMemIntro_ -> XGrpMemIntro <$> p "memberInfo" <*> opt "memberRestrictions"
      XGrpMemInv_ -> XGrpMemInv <$> p "memberId" <*> p "memberIntro"
      XGrpMemFwd_ -> XGrpMemFwd <$> p "memberInfo" <*> p "memberIntro"
      XGrpMemInfo_ -> XGrpMemInfo <$> p "memberId" <*> p "profile"
      XGrpMemRole_ -> XGrpMemRole <$> p "memberId" <*> p "role"
      XGrpMemRestrict_ -> XGrpMemRestrict <$> p "memberId" <*> p "memberRestrictions"
      XGrpMemCon_ -> XGrpMemCon <$> p "memberId"
      XGrpMemConAll_ -> XGrpMemConAll <$> p "memberId"
      XGrpMemDel_ -> XGrpMemDel <$> p "memberId" <*> Right (fromRight False $ p "messages")
      XGrpLeave_ -> pure XGrpLeave
      XGrpDel_ -> pure XGrpDel
      XGrpInfo_ -> XGrpInfo <$> p "groupProfile"
      XGrpPrefs_ -> XGrpPrefs <$> p "groupPreferences"
      XGrpDirectInv_ -> XGrpDirectInv <$> p "connReq" <*> opt "content" <*> opt "scope"
      XGrpMsgForward_ -> XGrpMsgForward <$> opt "memberId" <*> opt "memberName" <*> p "msg" <*> p "msgTs"
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
chatToAppMessage chatMsg@ChatMessage {chatVRange, msgId, chatMsgEvent} = case encoding @e of
  SBinary -> AMBinary AppMessageBinary {msgId = Nothing, tag = B.head $ strEncode tag, body = chatMsgBinaryToBody chatMsg}
  SJson -> AMJson AppMessageJson {v = Just $ ChatVersionRange chatVRange, msgId, event = textEncode tag, params = params chatMsgEvent}
  where
    tag = toCMEventTag chatMsgEvent
    o :: [(J.Key, J.Value)] -> J.Object
    o = JM.fromList
    params :: ChatMsgEvent 'Json -> J.Object
    params = \case
      XMsgNew container -> msgContainerJSON container
      XMsgFileDescr msgId' fileDescr -> o ["msgId" .= msgId', "fileDescr" .= fileDescr]
      XMsgUpdate {msgId = msgId', content, mentions, ttl, live, scope} -> o $ ("ttl" .=? ttl) $ ("live" .=? live) $ ("scope" .=? scope) $ ("mentions" .=? nonEmptyMap mentions) ["msgId" .= msgId', "content" .= content]
      XMsgDel msgId' memberId scope -> o $ ("memberId" .=? memberId) $ ("scope" .=? scope) ["msgId" .= msgId']
      XMsgDeleted -> JM.empty
      XMsgReact msgId' memberId scope reaction add -> o $ ("memberId" .=? memberId) $ ("scope" .=? scope) ["msgId" .= msgId', "reaction" .= reaction, "add" .= add]
      XFile fileInv -> o ["file" .= fileInv]
      XFileAcpt fileName -> o ["fileName" .= fileName]
      XFileAcptInv sharedMsgId fileConnReq fileName -> o $ ("fileConnReq" .=? fileConnReq) ["msgId" .= sharedMsgId, "fileName" .= fileName]
      XFileCancel sharedMsgId -> o ["msgId" .= sharedMsgId]
      XInfo profile -> o ["profile" .= profile]
      XContact {profile, contactReqId, welcomeMsgId, requestMsg} -> o $ ("contactReqId" .=? contactReqId) $ ("welcomeMsgId" .=? welcomeMsgId) $ ("msgId" .=? (fst <$> requestMsg)) $ ("content" .=? (snd <$> requestMsg)) $ ["profile" .= profile]
      XMember {profile, newMemberId} -> o ["profile" .= profile, "newMemberId" .= newMemberId]
      XDirectDel -> JM.empty
      XGrpInv groupInv -> o ["groupInvitation" .= groupInv]
      XGrpAcpt memId -> o ["memberId" .= memId]
      XGrpLinkInv groupLinkInv -> o ["groupLinkInvitation" .= groupLinkInv]
      XGrpLinkReject groupLinkRjct -> o ["groupLinkRejection" .= groupLinkRjct]
      XGrpLinkMem profile -> o ["profile" .= profile]
      XGrpLinkAcpt acceptance role memberId -> o ["acceptance" .= acceptance, "role" .= role, "memberId" .= memberId]
      XGrpRelayInv groupRelayInv -> o ["groupRelayInvitation" .= groupRelayInv]
      XGrpRelayAcpt relayLink -> o ["relayLink" .= relayLink]
      XGrpMemNew memInfo scope -> o $ ("scope" .=? scope) ["memberInfo" .= memInfo]
      XGrpMemIntro memInfo memRestrictions -> o $ ("memberRestrictions" .=? memRestrictions) ["memberInfo" .= memInfo]
      XGrpMemInv memId memIntro -> o ["memberId" .= memId, "memberIntro" .= memIntro]
      XGrpMemFwd memInfo memIntro -> o ["memberInfo" .= memInfo, "memberIntro" .= memIntro]
      XGrpMemInfo memId profile -> o ["memberId" .= memId, "profile" .= profile]
      XGrpMemRole memId role -> o ["memberId" .= memId, "role" .= role]
      XGrpMemRestrict memId memRestrictions -> o ["memberId" .= memId, "memberRestrictions" .= memRestrictions]
      XGrpMemCon memId -> o ["memberId" .= memId]
      XGrpMemConAll memId -> o ["memberId" .= memId]
      XGrpMemDel memId messages -> o $ ("messages" .=? if messages then Just True else Nothing) ["memberId" .= memId]
      XGrpLeave -> JM.empty
      XGrpDel -> JM.empty
      XGrpInfo p -> o ["groupProfile" .= p]
      XGrpPrefs p -> o ["groupPreferences" .= p]
      XGrpDirectInv connReq content scope -> o $ ("content" .=? content) $ ("scope" .=? scope) ["connReq" .= connReq]
      XGrpMsgForward memberId_ memberName_ msg msgTs -> o $ ("memberId" .=? memberId_) $ ("memberName" .=? memberName_) ["msg" .= msg, "msgTs" .= msgTs]
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

chatMsgBinaryToBody :: ChatMessage 'Binary -> ByteString
chatMsgBinaryToBody ChatMessage {chatMsgEvent} = case chatMsgEvent of
  BFileChunk (SharedMsgId msgId) chunk -> smpEncode (msgId, IFC chunk)

chatMsgToBody :: forall e. MsgEncodingI e => ChatMessage e -> ByteString
chatMsgToBody chatMsg = case encoding @e of
  SBinary -> chatMsgBinaryToBody chatMsg
  SJson -> LB.toStrict $ J.encode chatMsg

instance ToJSON (ChatMessage 'Json) where
  toJSON = (\(AMJson msg) -> toJSON msg) . chatToAppMessage

instance FromJSON (ChatMessage 'Json) where
  parseJSON v = appJsonToCM <$?> parseJSON v

instance FromField (ChatMessage 'Json) where
  fromField = blobFieldDecoder J.eitherDecodeStrict'

data ContactShortLinkData = ContactShortLinkData
  { profile :: Profile,
    message :: Maybe MsgContent,
    business :: Bool
  }
  deriving (Show)

data GroupShortLinkData = GroupShortLinkData
  { groupProfile :: GroupProfile
  }
  deriving (Show)

$(JQ.deriveJSON defaultJSON ''ContactShortLinkData)

$(JQ.deriveJSON defaultJSON ''GroupShortLinkData)
