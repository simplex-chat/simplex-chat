{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Messages where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON, (.:))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.TH as JQ
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (isSpace)
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
import Simplex.Chat.Markdown
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Messaging.Agent.Protocol (AgentMsgId, MsgMeta (..), MsgReceiptStatus (..))
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, fromTextField_, parseAll, sumTypeJSON)
import Simplex.Messaging.Protocol (MsgBody)
import Simplex.Messaging.Util (eitherToMaybe, safeDecodeUtf8, (<$?>))

data ChatType = CTDirect | CTGroup | CTContactRequest | CTContactConnection
  deriving (Eq, Show, Ord)

data ChatName = ChatName {chatType :: ChatType, chatName :: Text}
  deriving (Show)

chatTypeStr :: ChatType -> Text
chatTypeStr = \case
  CTDirect -> "@"
  CTGroup -> "#"
  CTContactRequest -> "<@"
  CTContactConnection -> ":"

chatNameStr :: ChatName -> String
chatNameStr (ChatName cType name) = T.unpack $ chatTypeStr cType <> if T.any isSpace name then "'" <> name <> "'" else name

data ChatRef = ChatRef ChatType Int64
  deriving (Eq, Show, Ord)

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

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "JCInfo") ''JSONChatInfo)

instance ChatTypeI c => FromJSON (ChatInfo c) where
  parseJSON v = (\(AChatInfo _ c) -> checkChatType c) <$?> J.parseJSON v

instance ToJSON (ChatInfo c) where
  toJSON = J.toJSON . jsonChatInfo
  toEncoding = J.toEncoding . jsonChatInfo

jsonChatInfo :: ChatInfo c -> JSONChatInfo
jsonChatInfo = \case
  DirectChat c -> JCInfoDirect c
  GroupChat g -> JCInfoGroup g
  ContactRequest g -> JCInfoContactRequest g
  ContactConnection c -> JCInfoContactConnection c

data AChatInfo = forall c. ChatTypeI c => AChatInfo (SChatType c) (ChatInfo c)

deriving instance Show AChatInfo

jsonAChatInfo :: JSONChatInfo -> AChatInfo
jsonAChatInfo = \case
  JCInfoDirect c -> AChatInfo SCTDirect $ DirectChat c
  JCInfoGroup g -> AChatInfo SCTGroup $ GroupChat g
  JCInfoContactRequest g -> AChatInfo SCTContactRequest $ ContactRequest g
  JCInfoContactConnection c -> AChatInfo SCTContactConnection $ ContactConnection c

instance FromJSON AChatInfo where
  parseJSON v = jsonAChatInfo <$> J.parseJSON v

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
  deriving (Show)

isMention :: ChatItem c d -> Bool
isMention ChatItem {chatDir, quotedItem} = case chatDir of
  CIDirectRcv -> userItem quotedItem
  CIGroupRcv _ -> userItem quotedItem
  _ -> False
  where
    userItem = \case
      Nothing -> False
      Just CIQuote {chatDir = cd} -> case cd of
        CIQDirectSnd -> True
        CIQGroupSnd -> True
        _ -> False

data CIDirection (c :: ChatType) (d :: MsgDirection) where
  CIDirectSnd :: CIDirection 'CTDirect 'MDSnd
  CIDirectRcv :: CIDirection 'CTDirect 'MDRcv
  CIGroupSnd :: CIDirection 'CTGroup 'MDSnd
  CIGroupRcv :: GroupMember -> CIDirection 'CTGroup 'MDRcv

deriving instance Show (CIDirection c d)

data CCIDirection c = forall d. MsgDirectionI d => CCID (SMsgDirection d) (CIDirection c d)

data ACIDirection = forall c d. (ChatTypeI c, MsgDirectionI d) => ACID (SChatType c) (SMsgDirection d) (CIDirection c d)

data JSONCIDirection
  = JCIDirectSnd
  | JCIDirectRcv
  | JCIGroupSnd
  | JCIGroupRcv {groupMember :: GroupMember}
  deriving (Show)

jsonCIDirection :: CIDirection c d -> JSONCIDirection
jsonCIDirection = \case
  CIDirectSnd -> JCIDirectSnd
  CIDirectRcv -> JCIDirectRcv
  CIGroupSnd -> JCIGroupSnd
  CIGroupRcv m -> JCIGroupRcv m

jsonACIDirection :: JSONCIDirection -> ACIDirection
jsonACIDirection = \case
  JCIDirectSnd -> ACID SCTDirect SMDSnd CIDirectSnd
  JCIDirectRcv -> ACID SCTDirect SMDRcv CIDirectRcv
  JCIGroupSnd -> ACID SCTGroup SMDSnd CIGroupSnd
  JCIGroupRcv m -> ACID SCTGroup SMDRcv $ CIGroupRcv m

data CIReactionCount = CIReactionCount {reaction :: MsgReaction, userReacted :: Bool, totalReacted :: Int}
  deriving (Show)

data CChatItem c = forall d. MsgDirectionI d => CChatItem (SMsgDirection d) (ChatItem c d)

deriving instance Show (CChatItem c)

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
  deriving (Show)

data AChat = forall c. ChatTypeI c => AChat (SChatType c) (Chat c)

deriving instance Show AChat

data ChatStats = ChatStats
  { unreadCount :: Int,
    minUnreadItemId :: ChatItemId,
    unreadChat :: Bool
  }
  deriving (Show)

-- | type to show a mix of messages from multiple chats
data AChatItem = forall c d. (ChatTypeI c, MsgDirectionI d) => AChatItem (SChatType c) (SMsgDirection d) (ChatInfo c) (ChatItem c d)

deriving instance Show AChatItem

data JSONAnyChatItem c d = JSONAnyChatItem {chatInfo :: ChatInfo c, chatItem :: ChatItem c d}

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
    forwardedByMember :: Maybe GroupMemberId,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show)

mkCIMeta :: ChatItemId -> CIContent d -> Text -> CIStatus d -> Maybe SharedMsgId -> Maybe (CIDeleted c) -> Bool -> Maybe CITimed -> Maybe Bool -> UTCTime -> ChatItemTs -> Maybe GroupMemberId -> UTCTime -> UTCTime -> CIMeta c d
mkCIMeta itemId itemContent itemText itemStatus itemSharedMsgId itemDeleted itemEdited itemTimed itemLive currentTs itemTs forwardedByMember createdAt updatedAt =
  let editable = case itemContent of
        CISndMsgContent _ -> diffUTCTime currentTs itemTs < nominalDay && isNothing itemDeleted
        _ -> False
   in CIMeta {itemId, itemTs, itemText, itemStatus, itemSharedMsgId, itemDeleted, itemEdited, itemTimed, itemLive, editable, forwardedByMember, createdAt, updatedAt}

data CITimed = CITimed
  { ttl :: Int, -- seconds
    deleteAt :: Maybe UTCTime -- this is initially Nothing for received items, the timer starts when they are read
  }
  deriving (Show)

ttl' :: CITimed -> Int
ttl' CITimed {ttl} = ttl

contactTimedTTL :: Contact -> Maybe (Maybe Int)
contactTimedTTL Contact {mergedPreferences = ContactUserPreferences {timedMessages = ContactUserPreference {enabled, userPreference}}}
  | forUser enabled && forContact enabled = Just ttl
  | otherwise = Nothing
  where
    TimedMessagesPreference {ttl} = userPreference.preference

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
  deriving (Show)

quoteItemId :: CIQuote c -> Maybe ChatItemId
quoteItemId CIQuote {itemId} = itemId

data CIReaction (c :: ChatType) (d :: MsgDirection) = CIReaction
  { chatDir :: CIDirection c d,
    chatItem :: CChatItem c,
    sentAt :: UTCTime,
    reaction :: MsgReaction
  }
  deriving (Show)

data AnyCIReaction = forall c d. ChatTypeI c => ACIR (SChatType c) (SMsgDirection d) (CIReaction c d)

data ACIReaction = forall c d. ChatTypeI c => ACIReaction (SChatType c) (SMsgDirection d) (ChatInfo c) (CIReaction c d)

deriving instance Show ACIReaction

data JSONCIReaction c d = JSONCIReaction {chatInfo :: ChatInfo c, chatReaction :: CIReaction c d}

data CIQDirection (c :: ChatType) where
  CIQDirectSnd :: CIQDirection 'CTDirect
  CIQDirectRcv :: CIQDirection 'CTDirect
  CIQGroupSnd :: CIQDirection 'CTGroup
  CIQGroupRcv :: Maybe GroupMember -> CIQDirection 'CTGroup -- member can be Nothing in case MsgRef has memberId that the user is not notified about yet

deriving instance Show (CIQDirection c)

data ACIQDirection = forall c. ChatTypeI c => ACIQDirection (SChatType c) (CIQDirection c)

jsonCIQDirection :: CIQDirection c -> Maybe JSONCIDirection
jsonCIQDirection = \case
  CIQDirectSnd -> Just JCIDirectSnd
  CIQDirectRcv -> Just JCIDirectRcv
  CIQGroupSnd -> Just JCIGroupSnd
  CIQGroupRcv (Just m) -> Just $ JCIGroupRcv m
  CIQGroupRcv Nothing -> Nothing

jsonACIQDirection :: Maybe JSONCIDirection -> ACIQDirection
jsonACIQDirection = \case
  Just JCIDirectSnd -> ACIQDirection SCTDirect CIQDirectSnd
  Just JCIDirectRcv -> ACIQDirection SCTDirect CIQDirectRcv
  Just JCIGroupSnd -> ACIQDirection SCTGroup CIQGroupSnd
  Just (JCIGroupRcv m) -> ACIQDirection SCTGroup $ CIQGroupRcv (Just m)
  Nothing -> ACIQDirection SCTGroup $ CIQGroupRcv Nothing

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
  deriving (Show)

data FileProtocol = FPSMP | FPXFTP
  deriving (Eq, Show, Ord)

instance FromField FileProtocol where fromField = fromTextField_ textDecode

instance ToField FileProtocol where toField = toField . textEncode

instance FromJSON FileProtocol where
  parseJSON = textParseJSON "FileProtocol"

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
  deriving (Show)

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

jsonACIStatus :: JSONCIStatus -> ACIStatus
jsonACIStatus = \case
  JCISSndNew -> ACIStatus SMDSnd CISSndNew
  JCISSndSent sndProgress -> ACIStatus SMDSnd $ CISSndSent sndProgress
  JCISSndRcvd msgRcptStatus sndProgress -> ACIStatus SMDSnd $ CISSndRcvd msgRcptStatus sndProgress
  JCISSndErrorAuth -> ACIStatus SMDSnd CISSndErrorAuth
  JCISSndError e -> ACIStatus SMDSnd $ CISSndError e
  JCISRcvNew -> ACIStatus SMDRcv CISRcvNew
  JCISRcvRead -> ACIStatus SMDRcv CISRcvRead
  JCISInvalid text -> ACIStatus SMDSnd $ CISInvalid text

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
  deriving (Eq, Show)

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

data AChatType = forall c. ChatTypeI c => ACT (SChatType c)

class ChatTypeI (c :: ChatType) where
  chatTypeI :: SChatType c

instance ChatTypeI 'CTDirect where chatTypeI = SCTDirect

instance ChatTypeI 'CTGroup where chatTypeI = SCTGroup

instance ChatTypeI 'CTContactRequest where chatTypeI = SCTContactRequest

instance ChatTypeI 'CTContactConnection where chatTypeI = SCTContactConnection

toChatType :: SChatType c -> ChatType
toChatType = \case
  SCTDirect -> CTDirect
  SCTGroup -> CTGroup
  SCTContactRequest -> CTContactRequest
  SCTContactConnection -> CTContactConnection

aChatType :: ChatType -> AChatType
aChatType = \case
  CTDirect -> ACT SCTDirect
  CTGroup -> ACT SCTGroup
  CTContactRequest -> ACT SCTContactRequest
  CTContactConnection -> ACT SCTContactConnection

checkChatType :: forall t c c'. (ChatTypeI c, ChatTypeI c') => t c' -> Either String (t c)
checkChatType x = case testEquality (chatTypeI @c) (chatTypeI @c') of
  Just Refl -> Right x
  Nothing -> Left "bad chat type"

type LazyMsgBody = L.ByteString

data SndMessage = SndMessage
  { msgId :: MessageId,
    sharedMsgId :: SharedMsgId,
    msgBody :: LazyMsgBody
  }
  deriving (Show)

data NewRcvMessage e = NewRcvMessage
  { chatMsgEvent :: ChatMsgEvent e,
    msgBody :: MsgBody
  }
  deriving (Show)

data RcvMessage = RcvMessage
  { msgId :: MessageId,
    chatMsgEvent :: AChatMsgEvent,
    sharedMsgId_ :: Maybe SharedMsgId,
    msgBody :: MsgBody,
    authorMember :: Maybe GroupMemberId,
    forwardedByMember :: Maybe GroupMemberId
  }

data PendingGroupMessage = PendingGroupMessage
  { msgId :: MessageId,
    cmEventTag :: ACMEventTag,
    msgBody :: LazyMsgBody,
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
  deriving (Eq, Show)

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

data CIDeleted (c :: ChatType) where
  CIDeleted :: Maybe UTCTime -> CIDeleted c
  CIBlocked :: Maybe UTCTime -> CIDeleted 'CTGroup
  CIModerated :: Maybe UTCTime -> GroupMember -> CIDeleted 'CTGroup

deriving instance Show (CIDeleted c)

data ACIDeleted = forall c. ChatTypeI c => ACIDeleted (SChatType c) (CIDeleted c)

data JSONCIDeleted
  = JCIDDeleted {deletedTs :: Maybe UTCTime, chatType :: ChatType}
  | JCIDBlocked {deletedTs :: Maybe UTCTime}
  | JCIDModerated {deletedTs :: Maybe UTCTime, byGroupMember :: GroupMember}
  deriving (Show)

jsonCIDeleted :: forall d. ChatTypeI d => CIDeleted d -> JSONCIDeleted
jsonCIDeleted = \case
  CIDeleted ts -> JCIDDeleted ts (toChatType $ chatTypeI @d)
  CIBlocked ts -> JCIDBlocked ts
  CIModerated ts m -> JCIDModerated ts m

jsonACIDeleted :: JSONCIDeleted -> ACIDeleted
jsonACIDeleted = \case
  JCIDDeleted ts cType -> case aChatType cType of ACT c -> ACIDeleted c $ CIDeleted ts
  JCIDBlocked ts -> ACIDeleted SCTGroup $ CIBlocked ts
  JCIDModerated ts m -> ACIDeleted SCTGroup (CIModerated ts m)

itemDeletedTs :: CIDeleted d -> Maybe UTCTime
itemDeletedTs = \case
  CIDeleted ts -> ts
  CIBlocked ts -> ts
  CIModerated ts _ -> ts

data ChatItemInfo = ChatItemInfo
  { itemVersions :: [ChatItemVersion],
    memberDeliveryStatuses :: Maybe [MemberDeliveryStatus]
  }
  deriving (Eq, Show)

data ChatItemVersion = ChatItemVersion
  { chatItemVersionId :: Int64,
    msgContent :: MsgContent,
    formattedText :: Maybe MarkdownList,
    itemVersionTs :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data CIModeration = CIModeration
  { moderationId :: Int64,
    moderatorMember :: GroupMember,
    createdByMsgId :: MessageId,
    moderatedAt :: UTCTime
  }
  deriving (Show)

$(JQ.deriveJSON (enumJSON $ dropPrefix "CT") ''ChatType)

instance ChatTypeI c => FromJSON (SChatType c) where
  parseJSON v = (\(ACT t) -> checkChatType t) . aChatType <$?> J.parseJSON v

instance ToJSON (SChatType c) where
  toJSON = J.toJSON . toChatType
  toEncoding = J.toEncoding . toChatType

$(JQ.deriveJSON defaultJSON ''ChatName)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "JCID") ''JSONCIDeleted)

instance ChatTypeI c => FromJSON (CIDeleted c) where
  parseJSON v = (\(ACIDeleted _ x) -> checkChatType x) . jsonACIDeleted <$?> J.parseJSON v

instance ChatTypeI c => ToJSON (CIDeleted c) where
  toJSON = J.toJSON . jsonCIDeleted
  toEncoding = J.toEncoding . jsonCIDeleted

$(JQ.deriveJSON defaultJSON ''CITimed)

$(JQ.deriveJSON (enumJSON $ dropPrefix "SSP") ''SndCIStatusProgress)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "JCIS") ''JSONCIStatus)

instance MsgDirectionI d => FromJSON (CIStatus d) where
  parseJSON v = (\(ACIStatus _ s) -> checkDirection s) . jsonACIStatus <$?> J.parseJSON v

instance ToJSON (CIStatus d) where
  toJSON = J.toJSON . jsonCIStatus
  toEncoding = J.toEncoding . jsonCIStatus

instance MsgDirectionI d => ToField (CIStatus d) where toField = toField . decodeLatin1 . strEncode

instance (Typeable d, MsgDirectionI d) => FromField (CIStatus d) where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

instance FromField ACIStatus where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

$(JQ.deriveJSON defaultJSON ''MemberDeliveryStatus)

$(JQ.deriveJSON defaultJSON ''ChatItemVersion)

$(JQ.deriveJSON defaultJSON ''ChatItemInfo)

instance (ChatTypeI c, MsgDirectionI d) => FromJSON (CIMeta c d) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''CIMeta)

instance ChatTypeI c => ToJSON (CIMeta c d) where
  toJSON = $(JQ.mkToJSON defaultJSON ''CIMeta)
  toEncoding = $(JQ.mkToEncoding defaultJSON ''CIMeta)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "JCIFS") ''JSONCIFileStatus)

instance MsgDirectionI d => FromJSON (CIFileStatus d) where
  parseJSON v = (\(AFS _ s) -> checkDirection s) . aciFileStatusJSON <$?> J.parseJSON v

instance ToJSON (CIFileStatus d) where
  toJSON = J.toJSON . jsonCIFileStatus
  toEncoding = J.toEncoding . jsonCIFileStatus

instance MsgDirectionI d => ToField (CIFileStatus d) where toField = toField . decodeLatin1 . strEncode

instance FromField ACIFileStatus where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

instance MsgDirectionI d => FromJSON (CIFile d) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''CIFile)

instance MsgDirectionI d => ToJSON (CIFile d) where
  toJSON = $(JQ.mkToJSON defaultJSON ''CIFile)
  toEncoding = $(JQ.mkToEncoding defaultJSON ''CIFile)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "JCI") ''JSONCIDirection)

instance (ChatTypeI c, MsgDirectionI d) => FromJSON (CIDirection c d) where
  parseJSON v = (\(CCID _ x') -> checkDirection x') <$?> J.parseJSON v

instance ToJSON (CIDirection c d) where
  toJSON = J.toJSON . jsonCIDirection
  toEncoding = J.toEncoding . jsonCIDirection

instance ChatTypeI c => FromJSON (CCIDirection c) where
  parseJSON v = (\(ACID _ d x) -> checkChatType (CCID d x)) <$?> J.parseJSON v

instance FromJSON ACIDirection where
  parseJSON v = jsonACIDirection <$> J.parseJSON v

instance ChatTypeI c => FromJSON (CIQDirection c) where
  parseJSON v = (\(ACIQDirection _ x) -> checkChatType x) . jsonACIQDirection <$?> J.parseJSON v

instance ToJSON (CIQDirection c) where
  toJSON = J.toJSON . jsonCIQDirection
  toEncoding = J.toEncoding . jsonCIQDirection

instance ChatTypeI c => FromJSON (CIQuote c) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''CIQuote)

$(JQ.deriveToJSON defaultJSON ''CIQuote)

$(JQ.deriveJSON defaultJSON ''CIReactionCount)

instance (ChatTypeI c, MsgDirectionI d) => FromJSON (ChatItem c d) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''ChatItem)

instance (ChatTypeI c, MsgDirectionI d) => ToJSON (ChatItem c d) where
  toJSON = $(JQ.mkToJSON defaultJSON ''ChatItem)
  toEncoding = $(JQ.mkToEncoding defaultJSON ''ChatItem)

instance (ChatTypeI c, MsgDirectionI d) => ToJSON (JSONAnyChatItem c d) where
  toJSON = $(JQ.mkToJSON defaultJSON ''JSONAnyChatItem)
  toEncoding = $(JQ.mkToEncoding defaultJSON ''JSONAnyChatItem)

instance FromJSON AChatItem where
  parseJSON = J.withObject "AChatItem" $ \o -> do
    AChatInfo c chatInfo <- o .: "chatInfo"
    CChatItem d chatItem <- o .: "chatItem"
    pure $ AChatItem c d chatInfo chatItem

instance ToJSON AChatItem where
  toJSON (AChatItem _ _ chat item) = J.toJSON $ JSONAnyChatItem chat item
  toEncoding (AChatItem _ _ chat item) = J.toEncoding $ JSONAnyChatItem chat item

instance forall c. ChatTypeI c => FromJSON (CChatItem c) where
  parseJSON v = J.withObject "CChatItem" parse v
    where
      parse o = do
        CCID d (_ :: CIDirection c d) <- o .: "chatDir"
        ci <- J.parseJSON @(ChatItem c d) v
        pure $ CChatItem d ci

instance ChatTypeI c => ToJSON (CChatItem c) where
  toJSON (CChatItem _ ci) = J.toJSON ci
  toEncoding (CChatItem _ ci) = J.toEncoding ci

$(JQ.deriveJSON defaultJSON ''ChatStats)

instance ChatTypeI c => ToJSON (Chat c) where
  toJSON = $(JQ.mkToJSON defaultJSON ''Chat)
  toEncoding = $(JQ.mkToEncoding defaultJSON ''Chat)

instance FromJSON AChat where
  parseJSON = J.withObject "AChat" $ \o -> do
    AChatInfo c chatInfo <- o .: "chatInfo"
    chatItems <- o .: "chatItems"
    chatStats <- o .: "chatStats"
    pure $ AChat c Chat {chatInfo, chatItems, chatStats}

instance ToJSON AChat where
  toJSON (AChat _ c) = J.toJSON c
  toEncoding (AChat _ c) = J.toEncoding c

instance (ChatTypeI c, MsgDirectionI d) => FromJSON (CIReaction c d) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''CIReaction)

instance ChatTypeI c => ToJSON (CIReaction c d) where
  toJSON = $(JQ.mkToJSON defaultJSON ''CIReaction)
  toEncoding = $(JQ.mkToEncoding defaultJSON ''CIReaction)

instance FromJSON AnyCIReaction where
  parseJSON v = J.withObject "AnyCIReaction" parse v
    where
      parse o = do
        ACID c d (_ :: CIDirection c d) <- o .: "chatDir"
        ACIR c d <$> J.parseJSON @(CIReaction c d) v

instance ChatTypeI c => ToJSON (JSONCIReaction c d) where
  toJSON = $(JQ.mkToJSON defaultJSON ''JSONCIReaction)
  toEncoding = $(JQ.mkToEncoding defaultJSON ''JSONCIReaction)

instance FromJSON ACIReaction where
  parseJSON = J.withObject "ACIReaction" $ \o -> do
    ACIR c d reaction <- o .: "chatReaction"
    cInfo <- o .: "chatInfo"
    pure $ ACIReaction c d cInfo reaction

instance ToJSON ACIReaction where
  toJSON (ACIReaction _ _ cInfo reaction) = J.toJSON $ JSONCIReaction cInfo reaction
  toEncoding (ACIReaction _ _ cInfo reaction) = J.toEncoding $ JSONCIReaction cInfo reaction

$(JQ.deriveJSON defaultJSON ''MsgMetaJSON)

msgMetaJson :: MsgMeta -> Text
msgMetaJson = decodeLatin1 . LB.toStrict . J.encode . msgMetaToJson
