{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Messages where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time.Clock (UTCTime, diffUTCTime, nominalDay)
import Data.Type.Equality
import Data.Typeable (Typeable)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import Simplex.Chat.Markdown
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Messaging.Agent.Protocol (AgentMsgId, MsgMeta (..), MsgReceiptStatus (..))
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, fromTextField_, parseAll, sumTypeJSON)
import Simplex.Messaging.Protocol (MsgBody)
import Simplex.Messaging.Util (eitherToMaybe, safeDecodeUtf8, (<$?>))

data ChatType = CTDirect | CTGroup | CTContactRequest | CTContactConnection
  deriving (Eq, Show, Ord, Generic)

data ChatName = ChatName ChatType Text
  deriving (Show)

chatTypeStr :: ChatType -> String
chatTypeStr = \case
  CTDirect -> "@"
  CTGroup -> "#"
  CTContactRequest -> "<@"
  CTContactConnection -> ":"

chatNameStr :: ChatName -> String
chatNameStr (ChatName cType name) = chatTypeStr cType <> T.unpack name

data ChatRef = ChatRef ChatType Int64
  deriving (Eq, Show, Ord)

instance ToJSON ChatType where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "CT"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "CT"

data ChatInfo (c :: ChatType) where
  DirectChat :: Contact -> ChatInfo 'CTDirect
  GroupChat :: GroupInfo -> ChatInfo 'CTGroup
  ContactRequest :: UserContactRequest -> ChatInfo 'CTContactRequest
  ContactConnection :: PendingContactConnection -> ChatInfo 'CTContactConnection

deriving instance Show (ChatInfo c)

chatInfoChatTs :: ChatInfo c -> Maybe UTCTime
chatInfoChatTs = \case
  DirectChat Contact {chatTs} -> chatTs
  GroupChat GroupInfo {chatTs} -> chatTs
  _ -> Nothing

chatInfoUpdatedAt :: ChatInfo c -> UTCTime
chatInfoUpdatedAt = \case
  DirectChat Contact {updatedAt} -> updatedAt
  GroupChat GroupInfo {updatedAt} -> updatedAt
  ContactRequest UserContactRequest {updatedAt} -> updatedAt
  ContactConnection PendingContactConnection {updatedAt} -> updatedAt

chatInfoToRef :: ChatInfo c -> ChatRef
chatInfoToRef = \case
  DirectChat Contact {contactId} -> ChatRef CTDirect contactId
  GroupChat GroupInfo {groupId} -> ChatRef CTGroup groupId
  ContactRequest UserContactRequest {contactRequestId} -> ChatRef CTContactRequest contactRequestId
  ContactConnection PendingContactConnection {pccConnId} -> ChatRef CTContactConnection pccConnId

chatInfoMembership :: ChatInfo c -> Maybe GroupMember
chatInfoMembership = \case
  GroupChat GroupInfo {membership} -> Just membership
  _ -> Nothing

data JSONChatInfo
  = JCInfoDirect {contact :: Contact}
  | JCInfoGroup {groupInfo :: GroupInfo}
  | JCInfoContactRequest {contactRequest :: UserContactRequest}
  | JCInfoContactConnection {contactConnection :: PendingContactConnection}
  deriving (Generic)

instance ToJSON JSONChatInfo where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "JCInfo"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "JCInfo"

instance ToJSON (ChatInfo c) where
  toJSON = J.toJSON . jsonChatInfo
  toEncoding = J.toEncoding . jsonChatInfo

jsonChatInfo :: ChatInfo c -> JSONChatInfo
jsonChatInfo = \case
  DirectChat c -> JCInfoDirect c
  GroupChat g -> JCInfoGroup g
  ContactRequest g -> JCInfoContactRequest g
  ContactConnection c -> JCInfoContactConnection c

data AChatInfo = forall c. AChatInfo (SChatType c) (ChatInfo c)

deriving instance Show AChatInfo

instance ToJSON AChatInfo where
  toJSON (AChatInfo _ c) = J.toJSON c
  toEncoding (AChatInfo _ c) = J.toEncoding c

data ChatItem (c :: ChatType) (d :: MsgDirection) = ChatItem
  { chatDir :: CIDirection c d,
    meta :: CIMeta c d,
    content :: CIContent d,
    formattedText :: Maybe MarkdownList,
    quotedItem :: Maybe (CIQuote c),
    reactions :: [CIReactionCount],
    file :: Maybe (CIFile d)
  }
  deriving (Show, Generic)

instance MsgDirectionI d => ToJSON (ChatItem c d) where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data CIDirection (c :: ChatType) (d :: MsgDirection) where
  CIDirectSnd :: CIDirection 'CTDirect 'MDSnd
  CIDirectRcv :: CIDirection 'CTDirect 'MDRcv
  CIGroupSnd :: CIDirection 'CTGroup 'MDSnd
  CIGroupRcv :: GroupMember -> CIDirection 'CTGroup 'MDRcv

deriving instance Show (CIDirection c d)

data JSONCIDirection
  = JCIDirectSnd
  | JCIDirectRcv
  | JCIGroupSnd
  | JCIGroupRcv {groupMember :: GroupMember}
  deriving (Generic, Show)

instance ToJSON JSONCIDirection where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "JCI"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "JCI"

instance ToJSON (CIDirection c d) where
  toJSON = J.toJSON . jsonCIDirection
  toEncoding = J.toEncoding . jsonCIDirection

jsonCIDirection :: CIDirection c d -> JSONCIDirection
jsonCIDirection = \case
  CIDirectSnd -> JCIDirectSnd
  CIDirectRcv -> JCIDirectRcv
  CIGroupSnd -> JCIGroupSnd
  CIGroupRcv m -> JCIGroupRcv m

data CIReactionCount = CIReactionCount {reaction :: MsgReaction, userReacted :: Bool, totalReacted :: Int}
  deriving (Show, Generic)

instance ToJSON CIReactionCount where toEncoding = J.genericToEncoding J.defaultOptions

data CChatItem c = forall d. MsgDirectionI d => CChatItem (SMsgDirection d) (ChatItem c d)

deriving instance Show (CChatItem c)

instance ToJSON (CChatItem c) where
  toJSON (CChatItem _ ci) = J.toJSON ci
  toEncoding (CChatItem _ ci) = J.toEncoding ci

cchatItemId :: CChatItem c -> ChatItemId
cchatItemId (CChatItem _ ci) = chatItemId' ci

chatItemId' :: ChatItem c d -> ChatItemId
chatItemId' ChatItem {meta = CIMeta {itemId}} = itemId

chatItemTs :: CChatItem c -> UTCTime
chatItemTs (CChatItem _ ci) = chatItemTs' ci

chatItemTs' :: ChatItem c d -> UTCTime
chatItemTs' ChatItem {meta = CIMeta {itemTs}} = itemTs

chatItemTimed :: ChatItem c d -> Maybe CITimed
chatItemTimed ChatItem {meta = CIMeta {itemTimed}} = itemTimed

timedDeleteAt' :: CITimed -> Maybe UTCTime
timedDeleteAt' CITimed {deleteAt} = deleteAt

chatItemMember :: GroupInfo -> ChatItem 'CTGroup d -> GroupMember
chatItemMember GroupInfo {membership} ChatItem {chatDir} = case chatDir of
  CIGroupSnd -> membership
  CIGroupRcv m -> m

ciReactionAllowed :: ChatItem c d -> Bool
ciReactionAllowed ChatItem {meta = CIMeta {itemDeleted = Just _}} = False
ciReactionAllowed ChatItem {content} = isJust $ ciMsgContent content

data CIDeletedState = CIDeletedState
  { markedDeleted :: Bool,
    deletedByMember :: Maybe GroupMember
  }
  deriving (Show, Eq)

chatItemDeletedState :: ChatItem c d -> Maybe CIDeletedState
chatItemDeletedState ChatItem {meta = CIMeta {itemDeleted}, content} =
  ciDeletedToDeletedState <$> itemDeleted
  where
    ciDeletedToDeletedState cid =
      case content of
        CISndModerated -> CIDeletedState {markedDeleted = False, deletedByMember = byMember cid}
        CIRcvModerated -> CIDeletedState {markedDeleted = False, deletedByMember = byMember cid}
        _ -> CIDeletedState {markedDeleted = True, deletedByMember = byMember cid}
    byMember :: CIDeleted c -> Maybe GroupMember
    byMember = \case
      CIModerated _ m -> Just m
      CIDeleted _ -> Nothing

data ChatDirection (c :: ChatType) (d :: MsgDirection) where
  CDDirectSnd :: Contact -> ChatDirection 'CTDirect 'MDSnd
  CDDirectRcv :: Contact -> ChatDirection 'CTDirect 'MDRcv
  CDGroupSnd :: GroupInfo -> ChatDirection 'CTGroup 'MDSnd
  CDGroupRcv :: GroupInfo -> GroupMember -> ChatDirection 'CTGroup 'MDRcv

toCIDirection :: ChatDirection c d -> CIDirection c d
toCIDirection = \case
  CDDirectSnd _ -> CIDirectSnd
  CDDirectRcv _ -> CIDirectRcv
  CDGroupSnd _ -> CIGroupSnd
  CDGroupRcv _ m -> CIGroupRcv m

toChatInfo :: ChatDirection c d -> ChatInfo c
toChatInfo = \case
  CDDirectSnd c -> DirectChat c
  CDDirectRcv c -> DirectChat c
  CDGroupSnd g -> GroupChat g
  CDGroupRcv g _ -> GroupChat g

data NewChatItem d = NewChatItem
  { createdByMsgId :: Maybe MessageId,
    itemSent :: SMsgDirection d,
    itemTs :: ChatItemTs,
    itemContent :: CIContent d,
    itemText :: Text,
    itemStatus :: CIStatus d,
    itemSharedMsgId :: Maybe SharedMsgId,
    itemQuotedMsg :: Maybe QuotedMsg,
    createdAt :: UTCTime
  }
  deriving (Show)

-- | type to show one chat with messages
data Chat c = Chat
  { chatInfo :: ChatInfo c,
    chatItems :: [CChatItem c],
    chatStats :: ChatStats
  }
  deriving (Show, Generic)

instance ToJSON (Chat c) where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

data AChat = forall c. AChat (SChatType c) (Chat c)

deriving instance Show AChat

instance ToJSON AChat where
  toJSON (AChat _ c) = J.toJSON c
  toEncoding (AChat _ c) = J.toEncoding c

data ChatStats = ChatStats
  { unreadCount :: Int,
    minUnreadItemId :: ChatItemId,
    unreadChat :: Bool
  }
  deriving (Show, Generic)

instance ToJSON ChatStats where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

-- | type to show a mix of messages from multiple chats
data AChatItem = forall c d. MsgDirectionI d => AChatItem (SChatType c) (SMsgDirection d) (ChatInfo c) (ChatItem c d)

deriving instance Show AChatItem

instance ToJSON AChatItem where
  toJSON (AChatItem _ _ chat item) = J.toJSON $ JSONAnyChatItem chat item
  toEncoding (AChatItem _ _ chat item) = J.toEncoding $ JSONAnyChatItem chat item

data JSONAnyChatItem c d = JSONAnyChatItem {chatInfo :: ChatInfo c, chatItem :: ChatItem c d}
  deriving (Generic)

aChatItems :: AChat -> [AChatItem]
aChatItems (AChat ct Chat {chatInfo, chatItems}) = map aChatItem chatItems
  where
    aChatItem (CChatItem md ci) = AChatItem ct md chatInfo ci

aChatItemId :: AChatItem -> Int64
aChatItemId (AChatItem _ _ _ ci) = chatItemId' ci

aChatItemTs :: AChatItem -> UTCTime
aChatItemTs (AChatItem _ _ _ ci) = chatItemTs' ci

updateFileStatus :: forall c d. ChatItem c d -> CIFileStatus d -> ChatItem c d
updateFileStatus ci@ChatItem {file} status = case file of
  Just f -> ci {file = Just (f :: CIFile d) {fileStatus = status}}
  Nothing -> ci

instance MsgDirectionI d => ToJSON (JSONAnyChatItem c d) where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

-- This type is not saved to DB, so all JSON encodings are platform-specific
data CIMeta (c :: ChatType) (d :: MsgDirection) = CIMeta
  { itemId :: ChatItemId,
    itemTs :: ChatItemTs,
    itemText :: Text,
    itemStatus :: CIStatus d,
    itemSharedMsgId :: Maybe SharedMsgId,
    itemDeleted :: Maybe (CIDeleted c),
    itemEdited :: Bool,
    itemTimed :: Maybe CITimed,
    itemLive :: Maybe Bool,
    editable :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic)

mkCIMeta :: ChatItemId -> CIContent d -> Text -> CIStatus d -> Maybe SharedMsgId -> Maybe (CIDeleted c) -> Bool -> Maybe CITimed -> Maybe Bool -> UTCTime -> ChatItemTs -> UTCTime -> UTCTime -> CIMeta c d
mkCIMeta itemId itemContent itemText itemStatus itemSharedMsgId itemDeleted itemEdited itemTimed itemLive currentTs itemTs createdAt updatedAt =
  let editable = case itemContent of
        CISndMsgContent _ -> diffUTCTime currentTs itemTs < nominalDay && isNothing itemDeleted
        _ -> False
   in CIMeta {itemId, itemTs, itemText, itemStatus, itemSharedMsgId, itemDeleted, itemEdited, itemTimed, itemLive, editable, createdAt, updatedAt}

instance ToJSON (CIMeta c d) where toEncoding = J.genericToEncoding J.defaultOptions

data CITimed = CITimed
  { ttl :: Int, -- seconds
    deleteAt :: Maybe UTCTime -- this is initially Nothing for received items, the timer starts when they are read
  }
  deriving (Show, Generic)

instance ToJSON CITimed where toEncoding = J.genericToEncoding J.defaultOptions

ttl' :: CITimed -> Int
ttl' CITimed {ttl} = ttl

contactTimedTTL :: Contact -> Maybe (Maybe Int)
contactTimedTTL Contact {mergedPreferences = ContactUserPreferences {timedMessages = ContactUserPreference {enabled, userPreference}}}
  | forUser enabled && forContact enabled = Just ttl
  | otherwise = Nothing
  where
    TimedMessagesPreference {ttl} =  userPreference.preference

groupTimedTTL :: GroupInfo -> Maybe (Maybe Int)
groupTimedTTL GroupInfo {fullGroupPreferences = FullGroupPreferences {timedMessages = TimedMessagesGroupPreference {enable, ttl}}}
  | enable == FEOn = Just ttl
  | otherwise = Nothing

rcvContactCITimed :: Contact -> Maybe Int -> Maybe CITimed
rcvContactCITimed = rcvCITimed_ . contactTimedTTL

rcvGroupCITimed :: GroupInfo -> Maybe Int -> Maybe CITimed
rcvGroupCITimed = rcvCITimed_ . groupTimedTTL

rcvCITimed_ :: Maybe (Maybe Int) -> Maybe Int -> Maybe CITimed
rcvCITimed_ chatTTL itemTTL = (`CITimed` Nothing) <$> (chatTTL >> itemTTL)

data CIQuote (c :: ChatType) = CIQuote
  { chatDir :: CIQDirection c,
    itemId :: Maybe ChatItemId, -- Nothing in case MsgRef references the item the user did not receive yet
    sharedMsgId :: Maybe SharedMsgId, -- Nothing for the messages from the old clients
    sentAt :: UTCTime,
    content :: MsgContent,
    formattedText :: Maybe MarkdownList
  }
  deriving (Show, Generic)

instance ToJSON (CIQuote c) where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data CIReaction (c :: ChatType) (d :: MsgDirection) = CIReaction
  { chatDir :: CIDirection c d,
    chatItem :: CChatItem c,
    sentAt :: UTCTime,
    reaction :: MsgReaction
  }
  deriving (Show, Generic)

instance ToJSON (CIReaction c d) where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data ACIReaction = forall c d. ACIReaction (SChatType c) (SMsgDirection d) (ChatInfo c) (CIReaction c d)

deriving instance Show ACIReaction

instance ToJSON ACIReaction where
  toJSON (ACIReaction _ _ chat reaction) = J.toJSON $ JSONCIReaction chat reaction
  toEncoding (ACIReaction _ _ chat reaction) = J.toEncoding $ JSONCIReaction chat reaction

data JSONCIReaction c d = JSONCIReaction {chatInfo :: ChatInfo c, chatReaction :: CIReaction c d}
  deriving (Generic)

instance ToJSON (JSONCIReaction c d) where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

data CIQDirection (c :: ChatType) where
  CIQDirectSnd :: CIQDirection 'CTDirect
  CIQDirectRcv :: CIQDirection 'CTDirect
  CIQGroupSnd :: CIQDirection 'CTGroup
  CIQGroupRcv :: Maybe GroupMember -> CIQDirection 'CTGroup -- member can be Nothing in case MsgRef has memberId that the user is not notified about yet

deriving instance Show (CIQDirection c)

instance ToJSON (CIQDirection c) where
  toJSON = J.toJSON . jsonCIQDirection
  toEncoding = J.toEncoding . jsonCIQDirection

jsonCIQDirection :: CIQDirection c -> Maybe JSONCIDirection
jsonCIQDirection = \case
  CIQDirectSnd -> Just JCIDirectSnd
  CIQDirectRcv -> Just JCIDirectRcv
  CIQGroupSnd -> Just JCIGroupSnd
  CIQGroupRcv (Just m) -> Just $ JCIGroupRcv m
  CIQGroupRcv Nothing -> Nothing

quoteMsgDirection :: CIQDirection c -> MsgDirection
quoteMsgDirection = \case
  CIQDirectSnd -> MDSnd
  CIQDirectRcv -> MDRcv
  CIQGroupSnd -> MDSnd
  CIQGroupRcv _ -> MDRcv

data CIFile (d :: MsgDirection) = CIFile
  { fileId :: Int64,
    fileName :: String,
    fileSize :: Integer,
    fileSource :: Maybe CryptoFile, -- local file path with optional key and nonce
    fileStatus :: CIFileStatus d,
    fileProtocol :: FileProtocol
  }
  deriving (Show, Generic)

instance MsgDirectionI d => ToJSON (CIFile d) where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data FileProtocol = FPSMP | FPXFTP
  deriving (Eq, Show, Ord)

instance FromField FileProtocol where fromField = fromTextField_ textDecode

instance ToField FileProtocol where toField = toField . textEncode

instance ToJSON FileProtocol where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

instance TextEncoding FileProtocol where
  textDecode = \case
    "smp" -> Just FPSMP
    "xftp" -> Just FPXFTP
    _ -> Nothing
  textEncode = \case
    FPSMP -> "smp"
    FPXFTP -> "xftp"

data CIFileStatus (d :: MsgDirection) where
  CIFSSndStored :: CIFileStatus 'MDSnd
  CIFSSndTransfer :: {sndProgress :: Int64, sndTotal :: Int64} -> CIFileStatus 'MDSnd
  CIFSSndCancelled :: CIFileStatus 'MDSnd
  CIFSSndComplete :: CIFileStatus 'MDSnd
  CIFSSndError :: CIFileStatus 'MDSnd
  CIFSRcvInvitation :: CIFileStatus 'MDRcv
  CIFSRcvAccepted :: CIFileStatus 'MDRcv
  CIFSRcvTransfer :: {rcvProgress :: Int64, rcvTotal :: Int64} -> CIFileStatus 'MDRcv
  CIFSRcvComplete :: CIFileStatus 'MDRcv
  CIFSRcvCancelled :: CIFileStatus 'MDRcv
  CIFSRcvError :: CIFileStatus 'MDRcv
  CIFSInvalid :: {text :: Text} -> CIFileStatus 'MDSnd

deriving instance Eq (CIFileStatus d)

deriving instance Show (CIFileStatus d)

ciFileEnded :: CIFileStatus d -> Bool
ciFileEnded = \case
  CIFSSndStored -> False
  CIFSSndTransfer {} -> False
  CIFSSndCancelled -> True
  CIFSSndComplete -> True
  CIFSSndError -> True
  CIFSRcvInvitation -> False
  CIFSRcvAccepted -> False
  CIFSRcvTransfer {} -> False
  CIFSRcvCancelled -> True
  CIFSRcvComplete -> True
  CIFSRcvError -> True
  CIFSInvalid {} -> True

instance ToJSON (CIFileStatus d) where
  toJSON = J.toJSON . jsonCIFileStatus
  toEncoding = J.toEncoding . jsonCIFileStatus

instance MsgDirectionI d => ToField (CIFileStatus d) where toField = toField . decodeLatin1 . strEncode

instance FromField ACIFileStatus where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

data ACIFileStatus = forall d. MsgDirectionI d => AFS (SMsgDirection d) (CIFileStatus d)

deriving instance Show ACIFileStatus

instance MsgDirectionI d => StrEncoding (CIFileStatus d) where
  strEncode = \case
    CIFSSndStored -> "snd_stored"
    CIFSSndTransfer sent total -> strEncode (Str "snd_transfer", sent, total)
    CIFSSndCancelled -> "snd_cancelled"
    CIFSSndComplete -> "snd_complete"
    CIFSSndError -> "snd_error"
    CIFSRcvInvitation -> "rcv_invitation"
    CIFSRcvAccepted -> "rcv_accepted"
    CIFSRcvTransfer rcvd total -> strEncode (Str "rcv_transfer", rcvd, total)
    CIFSRcvComplete -> "rcv_complete"
    CIFSRcvCancelled -> "rcv_cancelled"
    CIFSRcvError -> "rcv_error"
    CIFSInvalid {} -> "invalid"
  strP = (\(AFS _ st) -> checkDirection st) <$?> strP

instance StrEncoding ACIFileStatus where
  strEncode (AFS _ s) = strEncode s
  strP =
    (statusP <* A.endOfInput) -- endOfInput to make it fail on partial correct parse
      <|> (AFS SMDSnd . CIFSInvalid . safeDecodeUtf8 <$> A.takeByteString)
    where
      statusP =
        A.takeTill (== ' ') >>= \case
          "snd_stored" -> pure $ AFS SMDSnd CIFSSndStored
          "snd_transfer" -> AFS SMDSnd <$> progress CIFSSndTransfer
          "snd_cancelled" -> pure $ AFS SMDSnd CIFSSndCancelled
          "snd_complete" -> pure $ AFS SMDSnd CIFSSndComplete
          "snd_error" -> pure $ AFS SMDSnd CIFSSndError
          "rcv_invitation" -> pure $ AFS SMDRcv CIFSRcvInvitation
          "rcv_accepted" -> pure $ AFS SMDRcv CIFSRcvAccepted
          "rcv_transfer" -> AFS SMDRcv <$> progress CIFSRcvTransfer
          "rcv_complete" -> pure $ AFS SMDRcv CIFSRcvComplete
          "rcv_cancelled" -> pure $ AFS SMDRcv CIFSRcvCancelled
          "rcv_error" -> pure $ AFS SMDRcv CIFSRcvError
          _ -> fail "bad file status"
      progress :: (Int64 -> Int64 -> a) -> A.Parser a
      progress f = f <$> num <*> num <|> pure (f 0 1)
      num = A.space *> A.decimal

data JSONCIFileStatus
  = JCIFSSndStored
  | JCIFSSndTransfer {sndProgress :: Int64, sndTotal :: Int64}
  | JCIFSSndCancelled
  | JCIFSSndComplete
  | JCIFSSndError
  | JCIFSRcvInvitation
  | JCIFSRcvAccepted
  | JCIFSRcvTransfer {rcvProgress :: Int64, rcvTotal :: Int64}
  | JCIFSRcvComplete
  | JCIFSRcvCancelled
  | JCIFSRcvError
  | JCIFSInvalid {text :: Text}
  deriving (Generic)

instance ToJSON JSONCIFileStatus where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "JCIFS"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "JCIFS"

jsonCIFileStatus :: CIFileStatus d -> JSONCIFileStatus
jsonCIFileStatus = \case
  CIFSSndStored -> JCIFSSndStored
  CIFSSndTransfer sent total -> JCIFSSndTransfer sent total
  CIFSSndCancelled -> JCIFSSndCancelled
  CIFSSndComplete -> JCIFSSndComplete
  CIFSSndError -> JCIFSSndError
  CIFSRcvInvitation -> JCIFSRcvInvitation
  CIFSRcvAccepted -> JCIFSRcvAccepted
  CIFSRcvTransfer rcvd total -> JCIFSRcvTransfer rcvd total
  CIFSRcvComplete -> JCIFSRcvComplete
  CIFSRcvCancelled -> JCIFSRcvCancelled
  CIFSRcvError -> JCIFSRcvError
  CIFSInvalid text -> JCIFSInvalid text

aciFileStatusJSON :: JSONCIFileStatus -> ACIFileStatus
aciFileStatusJSON = \case
  JCIFSSndStored -> AFS SMDSnd CIFSSndStored
  JCIFSSndTransfer sent total -> AFS SMDSnd $ CIFSSndTransfer sent total
  JCIFSSndCancelled -> AFS SMDSnd CIFSSndCancelled
  JCIFSSndComplete -> AFS SMDSnd CIFSSndComplete
  JCIFSSndError -> AFS SMDSnd CIFSSndError
  JCIFSRcvInvitation -> AFS SMDRcv CIFSRcvInvitation
  JCIFSRcvAccepted -> AFS SMDRcv CIFSRcvAccepted
  JCIFSRcvTransfer rcvd total -> AFS SMDRcv $ CIFSRcvTransfer rcvd total
  JCIFSRcvComplete -> AFS SMDRcv CIFSRcvComplete
  JCIFSRcvCancelled -> AFS SMDRcv CIFSRcvCancelled
  JCIFSRcvError -> AFS SMDRcv CIFSRcvError
  JCIFSInvalid text -> AFS SMDSnd $ CIFSInvalid text

-- to conveniently read file data from db
data CIFileInfo = CIFileInfo
  { fileId :: Int64,
    fileStatus :: Maybe ACIFileStatus,
    filePath :: Maybe FilePath
  }
  deriving (Show)

mkCIFileInfo :: MsgDirectionI d => CIFile d -> CIFileInfo
mkCIFileInfo CIFile {fileId, fileStatus, fileSource} =
  CIFileInfo
    { fileId,
      fileStatus = Just $ AFS msgDirection fileStatus,
      filePath = CF.filePath <$> fileSource
    }

data CIStatus (d :: MsgDirection) where
  CISSndNew :: CIStatus 'MDSnd
  CISSndSent :: SndCIStatusProgress -> CIStatus 'MDSnd
  CISSndRcvd :: MsgReceiptStatus -> SndCIStatusProgress -> CIStatus 'MDSnd
  CISSndErrorAuth :: CIStatus 'MDSnd
  CISSndError :: String -> CIStatus 'MDSnd
  CISRcvNew :: CIStatus 'MDRcv
  CISRcvRead :: CIStatus 'MDRcv
  CISInvalid :: Text -> CIStatus 'MDSnd

deriving instance Eq (CIStatus d)

deriving instance Show (CIStatus d)

instance ToJSON (CIStatus d) where
  toJSON = J.toJSON . jsonCIStatus
  toEncoding = J.toEncoding . jsonCIStatus

instance MsgDirectionI d => ToField (CIStatus d) where toField = toField . decodeLatin1 . strEncode

instance (Typeable d, MsgDirectionI d) => FromField (CIStatus d) where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

instance FromField ACIStatus where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

data ACIStatus = forall d. MsgDirectionI d => ACIStatus (SMsgDirection d) (CIStatus d)

deriving instance Show ACIStatus

instance MsgDirectionI d => StrEncoding (CIStatus d) where
  strEncode = \case
    CISSndNew -> "snd_new"
    CISSndSent sndProgress -> "snd_sent " <> strEncode sndProgress
    CISSndRcvd msgRcptStatus sndProgress -> "snd_rcvd " <> strEncode msgRcptStatus <> " " <> strEncode sndProgress
    CISSndErrorAuth -> "snd_error_auth"
    CISSndError e -> "snd_error " <> encodeUtf8 (T.pack e)
    CISRcvNew -> "rcv_new"
    CISRcvRead -> "rcv_read"
    CISInvalid {} -> "invalid"
  strP = (\(ACIStatus _ st) -> checkDirection st) <$?> strP

instance StrEncoding ACIStatus where
  strEncode (ACIStatus _ s) = strEncode s
  strP =
    (statusP <* A.endOfInput) -- endOfInput to make it fail on partial correct parse, e.g. "snd_rcvd ok complete"
      <|> (ACIStatus SMDSnd . CISInvalid . safeDecodeUtf8 <$> A.takeByteString)
    where
      statusP =
        A.takeTill (== ' ') >>= \case
          "snd_new" -> pure $ ACIStatus SMDSnd CISSndNew
          "snd_sent" -> ACIStatus SMDSnd . CISSndSent <$> ((A.space *> strP) <|> pure SSPComplete)
          "snd_rcvd" -> ACIStatus SMDSnd <$> (CISSndRcvd <$> (A.space *> strP) <*> ((A.space *> strP) <|> pure SSPComplete))
          "snd_error_auth" -> pure $ ACIStatus SMDSnd CISSndErrorAuth
          "snd_error" -> ACIStatus SMDSnd . CISSndError . T.unpack . safeDecodeUtf8 <$> (A.space *> A.takeByteString)
          "rcv_new" -> pure $ ACIStatus SMDRcv CISRcvNew
          "rcv_read" -> pure $ ACIStatus SMDRcv CISRcvRead
          _ -> fail "bad status"

data JSONCIStatus
  = JCISSndNew
  | JCISSndSent {sndProgress :: SndCIStatusProgress}
  | JCISSndRcvd {msgRcptStatus :: MsgReceiptStatus, sndProgress :: SndCIStatusProgress}
  | JCISSndErrorAuth
  | JCISSndError {agentError :: String}
  | JCISRcvNew
  | JCISRcvRead
  | JCISInvalid {text :: Text}
  deriving (Show, Generic)

instance ToJSON JSONCIStatus where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "JCIS"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "JCIS"

jsonCIStatus :: CIStatus d -> JSONCIStatus
jsonCIStatus = \case
  CISSndNew -> JCISSndNew
  CISSndSent sndProgress -> JCISSndSent sndProgress
  CISSndRcvd msgRcptStatus sndProgress -> JCISSndRcvd msgRcptStatus sndProgress
  CISSndErrorAuth -> JCISSndErrorAuth
  CISSndError e -> JCISSndError e
  CISRcvNew -> JCISRcvNew
  CISRcvRead -> JCISRcvRead
  CISInvalid text -> JCISInvalid text

ciStatusNew :: forall d. MsgDirectionI d => CIStatus d
ciStatusNew = case msgDirection @d of
  SMDSnd -> CISSndNew
  SMDRcv -> CISRcvNew

ciCreateStatus :: forall d. MsgDirectionI d => CIContent d -> CIStatus d
ciCreateStatus content = case msgDirection @d of
  SMDSnd -> ciStatusNew
  SMDRcv -> if ciRequiresAttention content then ciStatusNew else CISRcvRead

membersGroupItemStatus :: [(CIStatus 'MDSnd, Int)] -> CIStatus 'MDSnd
membersGroupItemStatus memStatusCounts
  | rcvdOk == total = CISSndRcvd MROk SSPComplete
  | rcvdOk + rcvdBad == total = CISSndRcvd MRBadMsgHash SSPComplete
  | rcvdBad > 0 = CISSndRcvd MRBadMsgHash SSPPartial
  | rcvdOk > 0 = CISSndRcvd MROk SSPPartial
  | sent == total = CISSndSent SSPComplete
  | sent > 0 = CISSndSent SSPPartial
  | otherwise = CISSndNew
  where
    total = sum $ map snd memStatusCounts
    rcvdOk = fromMaybe 0 $ lookup (CISSndRcvd MROk SSPComplete) memStatusCounts
    rcvdBad = fromMaybe 0 $ lookup (CISSndRcvd MRBadMsgHash SSPComplete) memStatusCounts
    sent = fromMaybe 0 $ lookup (CISSndSent SSPComplete) memStatusCounts

data SndCIStatusProgress
  = SSPPartial
  | SSPComplete
  deriving (Eq, Show, Generic)

instance ToJSON SndCIStatusProgress where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "SSP"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "SSP"

instance StrEncoding SndCIStatusProgress where
  strEncode = \case
    SSPPartial -> "partial"
    SSPComplete -> "complete"
  strP =
    A.takeWhile1 (/= ' ') >>= \case
      "partial" -> pure SSPPartial
      "complete" -> pure SSPComplete
      _ -> fail "bad SndCIStatusProgress"

type ChatItemId = Int64

type ChatItemTs = UTCTime

data ChatPagination
  = CPLast Int
  | CPAfter ChatItemId Int
  | CPBefore ChatItemId Int
  deriving (Show)

data SChatType (c :: ChatType) where
  SCTDirect :: SChatType 'CTDirect
  SCTGroup :: SChatType 'CTGroup
  SCTContactRequest :: SChatType 'CTContactRequest
  SCTContactConnection :: SChatType 'CTContactConnection

deriving instance Show (SChatType c)

instance TestEquality SChatType where
  testEquality SCTDirect SCTDirect = Just Refl
  testEquality SCTGroup SCTGroup = Just Refl
  testEquality SCTContactRequest SCTContactRequest = Just Refl
  testEquality SCTContactConnection SCTContactConnection = Just Refl
  testEquality _ _ = Nothing

class ChatTypeI (c :: ChatType) where
  chatTypeI :: SChatType c

instance ChatTypeI 'CTDirect where chatTypeI = SCTDirect

instance ChatTypeI 'CTGroup where chatTypeI = SCTGroup

data NewMessage e = NewMessage
  { chatMsgEvent :: ChatMsgEvent e,
    msgBody :: MsgBody
  }
  deriving (Show)

data SndMessage = SndMessage
  { msgId :: MessageId,
    sharedMsgId :: SharedMsgId,
    msgBody :: MsgBody
  }

data RcvMessage = RcvMessage
  { msgId :: MessageId,
    chatMsgEvent :: AChatMsgEvent,
    sharedMsgId_ :: Maybe SharedMsgId,
    msgBody :: MsgBody
  }

data PendingGroupMessage = PendingGroupMessage
  { msgId :: MessageId,
    cmEventTag :: ACMEventTag,
    msgBody :: MsgBody,
    introId_ :: Maybe Int64
  }

type MessageId = Int64

data ConnOrGroupId = ConnectionId Int64 | GroupId Int64

data SndMsgDelivery = SndMsgDelivery
  { connId :: Int64,
    agentMsgId :: AgentMsgId
  }
  deriving (Show)

data RcvMsgDelivery = RcvMsgDelivery
  { connId :: Int64,
    agentMsgId :: AgentMsgId,
    agentMsgMeta :: MsgMeta,
    agentAckCmdId :: CommandId
  }
  deriving (Show)

data MsgMetaJSON = MsgMetaJSON
  { integrity :: Text,
    rcvId :: Int64,
    rcvTs :: UTCTime,
    serverId :: Text,
    serverTs :: UTCTime,
    sndId :: Int64
  }
  deriving (Eq, Show, FromJSON, Generic)

instance ToJSON MsgMetaJSON where toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

msgMetaToJson :: MsgMeta -> MsgMetaJSON
msgMetaToJson MsgMeta {integrity, recipient = (rcvId, rcvTs), broker = (serverId, serverTs), sndMsgId = sndId} =
  MsgMetaJSON
    { integrity = (decodeLatin1 . strEncode) integrity,
      rcvId,
      rcvTs,
      serverId = (decodeLatin1 . B64.encode) serverId,
      serverTs,
      sndId
    }

msgMetaJson :: MsgMeta -> Text
msgMetaJson = decodeLatin1 . LB.toStrict . J.encode . msgMetaToJson

data MsgDeliveryStatus (d :: MsgDirection) where
  MDSRcvAgent :: MsgDeliveryStatus 'MDRcv
  MDSRcvAcknowledged :: MsgDeliveryStatus 'MDRcv
  MDSSndPending :: MsgDeliveryStatus 'MDSnd
  MDSSndAgent :: MsgDeliveryStatus 'MDSnd
  MDSSndSent :: MsgDeliveryStatus 'MDSnd
  MDSSndRcvd :: MsgReceiptStatus -> MsgDeliveryStatus 'MDSnd
  MDSSndRead :: MsgDeliveryStatus 'MDSnd

data AMsgDeliveryStatus = forall d. AMDS (SMsgDirection d) (MsgDeliveryStatus d)

instance (Typeable d, MsgDirectionI d) => FromField (MsgDeliveryStatus d) where
  fromField = fromTextField_ msgDeliveryStatusT'

instance ToField (MsgDeliveryStatus d) where toField = toField . serializeMsgDeliveryStatus

serializeMsgDeliveryStatus :: MsgDeliveryStatus d -> Text
serializeMsgDeliveryStatus = \case
  MDSRcvAgent -> "rcv_agent"
  MDSRcvAcknowledged -> "rcv_acknowledged"
  MDSSndPending -> "snd_pending"
  MDSSndAgent -> "snd_agent"
  MDSSndSent -> "snd_sent"
  MDSSndRcvd status -> "snd_rcvd " <> safeDecodeUtf8 (strEncode status)
  MDSSndRead -> "snd_read"

msgDeliveryStatusT :: Text -> Maybe AMsgDeliveryStatus
msgDeliveryStatusT = eitherToMaybe . parseAll statusP . encodeUtf8
  where
    statusP =
      A.takeTill (== ' ') >>= \case
        "rcv_agent" -> pure $ AMDS SMDRcv MDSRcvAgent
        "rcv_acknowledged" -> pure $ AMDS SMDRcv MDSRcvAcknowledged
        "snd_pending" -> pure $ AMDS SMDSnd MDSSndPending
        "snd_agent" -> pure $ AMDS SMDSnd MDSSndAgent
        "snd_sent" -> pure $ AMDS SMDSnd MDSSndSent
        "snd_rcvd" -> AMDS SMDSnd . MDSSndRcvd <$> (A.space *> strP)
        "snd_read" -> pure $ AMDS SMDSnd MDSSndRead
        _ -> fail "bad AMsgDeliveryStatus"

msgDeliveryStatusT' :: forall d. MsgDirectionI d => Text -> Maybe (MsgDeliveryStatus d)
msgDeliveryStatusT' s =
  msgDeliveryStatusT s >>= \(AMDS d st) ->
    case testEquality d (msgDirection @d) of
      Just Refl -> Just st
      _ -> Nothing

checkDirection :: forall t d d'. (MsgDirectionI d, MsgDirectionI d') => t d' -> Either String (t d)
checkDirection x = case testEquality (msgDirection @d) (msgDirection @d') of
  Just Refl -> Right x
  Nothing -> Left "bad direction"

data CIDeleted (c :: ChatType) where
  CIDeleted :: Maybe UTCTime -> CIDeleted c
  CIModerated :: Maybe UTCTime -> GroupMember -> CIDeleted 'CTGroup

deriving instance Show (CIDeleted c)

instance ToJSON (CIDeleted d) where
  toJSON = J.toJSON . jsonCIDeleted
  toEncoding = J.toEncoding . jsonCIDeleted

data JSONCIDeleted
  = JCIDDeleted {deletedTs :: Maybe UTCTime}
  | JCIDModerated {deletedTs :: Maybe UTCTime, byGroupMember :: GroupMember}
  deriving (Show, Generic)

instance ToJSON JSONCIDeleted where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "JCID"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "JCID"

jsonCIDeleted :: CIDeleted d -> JSONCIDeleted
jsonCIDeleted = \case
  CIDeleted ts -> JCIDDeleted ts
  CIModerated ts m -> JCIDModerated ts m

itemDeletedTs :: CIDeleted d -> Maybe UTCTime
itemDeletedTs = \case
  CIDeleted ts -> ts
  CIModerated ts _ -> ts

data ChatItemInfo = ChatItemInfo
  { itemVersions :: [ChatItemVersion],
    memberDeliveryStatuses :: Maybe [MemberDeliveryStatus]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ChatItemInfo where toEncoding = J.genericToEncoding J.defaultOptions

data ChatItemVersion = ChatItemVersion
  { chatItemVersionId :: Int64,
    msgContent :: MsgContent,
    formattedText :: Maybe MarkdownList,
    itemVersionTs :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON ChatItemVersion where toEncoding = J.genericToEncoding J.defaultOptions

mkItemVersion :: ChatItem c d -> Maybe ChatItemVersion
mkItemVersion ChatItem {content, meta} = version <$> ciMsgContent content
  where
    CIMeta {itemId, itemTs, createdAt} = meta
    version mc =
      ChatItemVersion
        { chatItemVersionId = itemId,
          msgContent = mc,
          formattedText = parseMaybeMarkdownList $ msgContentText mc,
          itemVersionTs = itemTs,
          createdAt = createdAt
        }

data MemberDeliveryStatus = MemberDeliveryStatus
  { groupMemberId :: GroupMemberId,
    memberDeliveryStatus :: CIStatus 'MDSnd
  }
  deriving (Eq, Show, Generic)

instance ToJSON MemberDeliveryStatus where toEncoding = J.genericToEncoding J.defaultOptions

data CIModeration = CIModeration
  { moderationId :: Int64,
    moderatorMember :: GroupMember,
    createdByMsgId :: MessageId,
    moderatedAt :: UTCTime
  }
  deriving (Show)
