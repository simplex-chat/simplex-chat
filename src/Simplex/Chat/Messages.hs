{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fno-warn-operator-whitespace #-}

module Simplex.Chat.Messages where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON, (.:))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.TH as JQ
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, nominalDay)
import Data.Type.Equality
import Data.Typeable (Typeable)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.TypeLits (ErrorMessage (ShowType, type (:<>:)), TypeError)
import qualified GHC.TypeLits as Type
import Simplex.Chat.Markdown
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Util (textParseJSON)
import Simplex.Messaging.Agent.Protocol (AgentMsgId, MsgMeta (..), MsgReceiptStatus (..))
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, fromTextField_, parseAll, sumTypeJSON)
import Simplex.Messaging.Protocol (MsgBody)
import Simplex.Messaging.Util (eitherToMaybe, safeDecodeUtf8, (<$?>))

data ChatType = CTDirect | CTGroup | CTLocal | CTContactRequest | CTContactConnection
  deriving (Eq, Show, Ord)

data ChatName = ChatName {chatType :: ChatType, chatName :: Text}
  deriving (Show)

chatTypeStr :: ChatType -> Text
chatTypeStr = \case
  CTDirect -> "@"
  CTGroup -> "#"
  CTLocal -> "*"
  CTContactRequest -> "<@"
  CTContactConnection -> ":"

chatNameStr :: ChatName -> String
chatNameStr (ChatName cType name) = T.unpack $ chatTypeStr cType <> if T.any isSpace name then "'" <> name <> "'" else name

data ChatRef = ChatRef ChatType Int64
  deriving (Eq, Show, Ord)

data ChatInfo (c :: ChatType) where
  DirectChat :: Contact -> ChatInfo 'CTDirect
  GroupChat :: GroupInfo -> ChatInfo 'CTGroup
  LocalChat :: NoteFolder -> ChatInfo 'CTLocal
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
  LocalChat NoteFolder {updatedAt} -> updatedAt
  ContactRequest UserContactRequest {updatedAt} -> updatedAt
  ContactConnection PendingContactConnection {updatedAt} -> updatedAt

chatInfoToRef :: ChatInfo c -> ChatRef
chatInfoToRef = \case
  DirectChat Contact {contactId} -> ChatRef CTDirect contactId
  GroupChat GroupInfo {groupId} -> ChatRef CTGroup groupId
  LocalChat NoteFolder {noteFolderId} -> ChatRef CTLocal noteFolderId
  ContactRequest UserContactRequest {contactRequestId} -> ChatRef CTContactRequest contactRequestId
  ContactConnection PendingContactConnection {pccConnId} -> ChatRef CTContactConnection pccConnId

chatInfoMembership :: ChatInfo c -> Maybe GroupMember
chatInfoMembership = \case
  GroupChat GroupInfo {membership} -> Just membership
  _ -> Nothing

data JSONChatInfo
  = JCInfoDirect {contact :: Contact}
  | JCInfoGroup {groupInfo :: GroupInfo}
  | JCInfoLocal {noteFolder :: NoteFolder}
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
  LocalChat l -> JCInfoLocal l
  ContactRequest g -> JCInfoContactRequest g
  ContactConnection c -> JCInfoContactConnection c

data AChatInfo = forall c. ChatTypeI c => AChatInfo (SChatType c) (ChatInfo c)

deriving instance Show AChatInfo

jsonAChatInfo :: JSONChatInfo -> AChatInfo
jsonAChatInfo = \case
  JCInfoDirect c -> AChatInfo SCTDirect $ DirectChat c
  JCInfoGroup g -> AChatInfo SCTGroup $ GroupChat g
  JCInfoLocal l -> AChatInfo SCTLocal $ LocalChat l
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
  CILocalSnd :: CIDirection 'CTLocal 'MDSnd
  CILocalRcv :: CIDirection 'CTLocal 'MDRcv

deriving instance Show (CIDirection c d)

data CCIDirection c = forall d. MsgDirectionI d => CCID (SMsgDirection d) (CIDirection c d)

data ACIDirection = forall c d. (ChatTypeI c, MsgDirectionI d) => ACID (SChatType c) (SMsgDirection d) (CIDirection c d)

data JSONCIDirection
  = JCIDirectSnd
  | JCIDirectRcv
  | JCIGroupSnd
  | JCIGroupRcv {groupMember :: GroupMember}
  | JCILocalSnd
  | JCILocalRcv
  deriving (Show)

jsonCIDirection :: CIDirection c d -> JSONCIDirection
jsonCIDirection = \case
  CIDirectSnd -> JCIDirectSnd
  CIDirectRcv -> JCIDirectRcv
  CIGroupSnd -> JCIGroupSnd
  CIGroupRcv m -> JCIGroupRcv m
  CILocalSnd -> JCILocalSnd
  CILocalRcv -> JCILocalRcv

jsonACIDirection :: JSONCIDirection -> ACIDirection
jsonACIDirection = \case
  JCIDirectSnd -> ACID SCTDirect SMDSnd CIDirectSnd
  JCIDirectRcv -> ACID SCTDirect SMDRcv CIDirectRcv
  JCIGroupSnd -> ACID SCTGroup SMDSnd CIGroupSnd
  JCIGroupRcv m -> ACID SCTGroup SMDRcv $ CIGroupRcv m
  JCILocalSnd -> ACID SCTLocal SMDSnd CILocalSnd
  JCILocalRcv -> ACID SCTLocal SMDRcv CILocalRcv

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
  CDLocalSnd :: NoteFolder -> ChatDirection 'CTLocal 'MDSnd
  CDLocalRcv :: NoteFolder -> ChatDirection 'CTLocal 'MDRcv

toCIDirection :: ChatDirection c d -> CIDirection c d
toCIDirection = \case
  CDDirectSnd _ -> CIDirectSnd
  CDDirectRcv _ -> CIDirectRcv
  CDGroupSnd _ -> CIGroupSnd
  CDGroupRcv _ m -> CIGroupRcv m
  CDLocalSnd _ -> CILocalSnd
  CDLocalRcv _ -> CILocalRcv

toChatInfo :: ChatDirection c d -> ChatInfo c
toChatInfo = \case
  CDDirectSnd c -> DirectChat c
  CDDirectRcv c -> DirectChat c
  CDGroupSnd g -> GroupChat g
  CDGroupRcv g _ -> GroupChat g
  CDLocalSnd l -> LocalChat l
  CDLocalRcv l -> LocalChat l

contactChatDeleted :: ChatDirection c d -> Bool
contactChatDeleted = \case
  CDDirectSnd Contact {chatDeleted} -> chatDeleted
  CDDirectRcv Contact {chatDeleted} -> chatDeleted
  _ -> False

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

aChatItemDir :: AChatItem -> MsgDirection
aChatItemDir (AChatItem _ sMsgDir _ _) = toMsgDirection sMsgDir

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
    sentViaProxy :: Maybe Bool,
    itemSharedMsgId :: Maybe SharedMsgId,
    itemForwarded :: Maybe CIForwardedFrom,
    itemDeleted :: Maybe (CIDeleted c),
    itemEdited :: Bool,
    itemTimed :: Maybe CITimed,
    itemLive :: Maybe Bool,
    deletable :: Bool,
    editable :: Bool,
    forwardedByMember :: Maybe GroupMemberId,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show)

mkCIMeta :: forall c d. ChatTypeI c => ChatItemId -> CIContent d -> Text -> CIStatus d -> Maybe Bool -> Maybe SharedMsgId -> Maybe CIForwardedFrom -> Maybe (CIDeleted c) -> Bool -> Maybe CITimed -> Maybe Bool -> UTCTime -> ChatItemTs -> Maybe GroupMemberId -> UTCTime -> UTCTime -> CIMeta c d
mkCIMeta itemId itemContent itemText itemStatus sentViaProxy itemSharedMsgId itemForwarded itemDeleted itemEdited itemTimed itemLive currentTs itemTs forwardedByMember createdAt updatedAt =
  let deletable = deletable' itemContent itemDeleted itemTs nominalDay currentTs
      editable = deletable && isNothing itemForwarded
   in CIMeta {itemId, itemTs, itemText, itemStatus, sentViaProxy, itemSharedMsgId, itemForwarded, itemDeleted, itemEdited, itemTimed, itemLive, deletable, editable, forwardedByMember, createdAt, updatedAt}

deletable' :: forall c d. ChatTypeI c => CIContent d -> Maybe (CIDeleted c) -> UTCTime -> NominalDiffTime -> UTCTime -> Bool
deletable' itemContent itemDeleted itemTs allowedInterval currentTs =
  case itemContent of
    CISndMsgContent _ ->
      case chatTypeI @c of
        SCTLocal -> isNothing itemDeleted
        _ -> diffUTCTime currentTs itemTs < allowedInterval && isNothing itemDeleted
    _ -> False

dummyMeta :: ChatItemId -> UTCTime -> Text -> CIMeta c 'MDSnd
dummyMeta itemId ts itemText =
  CIMeta
    { itemId,
      itemTs = ts,
      itemText,
      itemStatus = CISSndNew,
      sentViaProxy = Nothing,
      itemSharedMsgId = Nothing,
      itemForwarded = Nothing,
      itemDeleted = Nothing,
      itemEdited = False,
      itemTimed = Nothing,
      itemLive = Nothing,
      deletable = False,
      editable = False,
      forwardedByMember = Nothing,
      createdAt = ts,
      updatedAt = ts
    }

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
    TimedMessagesPreference {ttl} = case userPreference of
      CUPContact {preference} -> preference
      CUPUser {preference} -> preference

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

type family ChatTypeQuotable (a :: ChatType) :: Constraint where
  ChatTypeQuotable 'CTDirect = ()
  ChatTypeQuotable 'CTGroup = ()
  ChatTypeQuotable a =
    (Int ~ Bool, TypeError ('Type.Text "ChatType " ':<>: 'ShowType a ':<>: 'Type.Text " cannot be quoted"))

data CIQDirection (c :: ChatType) where
  CIQDirectSnd :: CIQDirection 'CTDirect
  CIQDirectRcv :: CIQDirection 'CTDirect
  CIQGroupSnd :: CIQDirection 'CTGroup
  CIQGroupRcv :: Maybe GroupMember -> CIQDirection 'CTGroup -- member can be Nothing in case MsgRef has memberId that the user is not notified about yet

deriving instance Show (CIQDirection c)

data ACIQDirection = forall c. (ChatTypeI c, ChatTypeQuotable c) => ACIQDirection (SChatType c) (CIQDirection c)

jsonCIQDirection :: CIQDirection c -> Maybe JSONCIDirection
jsonCIQDirection = \case
  CIQDirectSnd -> Just JCIDirectSnd
  CIQDirectRcv -> Just JCIDirectRcv
  CIQGroupSnd -> Just JCIGroupSnd
  CIQGroupRcv (Just m) -> Just $ JCIGroupRcv m
  CIQGroupRcv Nothing -> Nothing

jsonACIQDirection :: Maybe JSONCIDirection -> Either String ACIQDirection
jsonACIQDirection = \case
  Just JCIDirectSnd -> Right $ ACIQDirection SCTDirect CIQDirectSnd
  Just JCIDirectRcv -> Right $ ACIQDirection SCTDirect CIQDirectRcv
  Just JCIGroupSnd -> Right $ ACIQDirection SCTGroup CIQGroupSnd
  Just (JCIGroupRcv m) -> Right $ ACIQDirection SCTGroup $ CIQGroupRcv (Just m)
  Nothing -> Right $ ACIQDirection SCTGroup $ CIQGroupRcv Nothing
  Just JCILocalSnd -> Left "unquotable"
  Just JCILocalRcv -> Left "unquotable"

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

data FileProtocol = FPSMP | FPXFTP | FPLocal
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
    "local" -> Just FPLocal
    _ -> Nothing
  textEncode = \case
    FPSMP -> "smp"
    FPXFTP -> "xftp"
    FPLocal -> "local"

data CIFileStatus (d :: MsgDirection) where
  CIFSSndStored :: CIFileStatus 'MDSnd
  CIFSSndTransfer :: {sndProgress :: Int64, sndTotal :: Int64} -> CIFileStatus 'MDSnd
  CIFSSndCancelled :: CIFileStatus 'MDSnd
  CIFSSndComplete :: CIFileStatus 'MDSnd
  CIFSSndError :: {sndFileError :: FileError} -> CIFileStatus 'MDSnd
  CIFSSndWarning :: {sndFileError :: FileError} -> CIFileStatus 'MDSnd
  CIFSRcvInvitation :: CIFileStatus 'MDRcv
  CIFSRcvAccepted :: CIFileStatus 'MDRcv
  CIFSRcvTransfer :: {rcvProgress :: Int64, rcvTotal :: Int64} -> CIFileStatus 'MDRcv
  CIFSRcvAborted :: CIFileStatus 'MDRcv
  CIFSRcvComplete :: CIFileStatus 'MDRcv
  CIFSRcvCancelled :: CIFileStatus 'MDRcv
  CIFSRcvError :: {rcvFileError :: FileError} -> CIFileStatus 'MDRcv
  CIFSRcvWarning :: {rcvFileError :: FileError} -> CIFileStatus 'MDRcv
  CIFSInvalid :: {text :: Text} -> CIFileStatus 'MDSnd

deriving instance Eq (CIFileStatus d)

deriving instance Show (CIFileStatus d)

ciFileEnded :: CIFileStatus d -> Bool
ciFileEnded = \case
  CIFSSndStored -> False
  CIFSSndTransfer {} -> False
  CIFSSndCancelled -> True
  CIFSSndComplete -> True
  CIFSSndError {} -> True
  CIFSSndWarning {} -> False
  CIFSRcvInvitation -> False
  CIFSRcvAccepted -> False
  CIFSRcvTransfer {} -> False
  CIFSRcvAborted -> True
  CIFSRcvCancelled -> True
  CIFSRcvComplete -> True
  CIFSRcvError {} -> True
  CIFSRcvWarning {} -> False
  CIFSInvalid {} -> True

ciFileLoaded :: CIFileStatus d -> Bool
ciFileLoaded = \case
  CIFSSndStored -> True
  CIFSSndTransfer {} -> True
  CIFSSndComplete -> True
  CIFSSndCancelled -> True
  CIFSSndError {} -> True
  CIFSSndWarning {} -> True
  CIFSRcvInvitation -> False
  CIFSRcvAccepted -> False
  CIFSRcvTransfer {} -> False
  CIFSRcvAborted -> False
  CIFSRcvCancelled -> False
  CIFSRcvComplete -> True
  CIFSRcvError {} -> False
  CIFSRcvWarning {} -> False
  CIFSInvalid {} -> False

data ForwardFileError = FFENotAccepted FileTransferId | FFEInProgress | FFEFailed | FFEMissing
  deriving (Eq, Ord)

ciFileForwardError :: FileTransferId -> CIFileStatus d -> Maybe ForwardFileError
ciFileForwardError fId = \case
  CIFSSndStored -> Nothing
  CIFSSndTransfer {} -> Nothing
  CIFSSndComplete -> Nothing
  CIFSSndCancelled -> Nothing
  CIFSSndError {} -> Nothing
  CIFSSndWarning {} -> Nothing
  CIFSRcvInvitation -> Just $ FFENotAccepted fId
  CIFSRcvAccepted -> Just FFEInProgress
  CIFSRcvTransfer {} -> Just FFEInProgress
  CIFSRcvAborted -> Just $ FFENotAccepted fId
  CIFSRcvCancelled -> Just FFEFailed
  CIFSRcvComplete -> Nothing
  CIFSRcvError {} -> Just FFEFailed
  CIFSRcvWarning {} -> Just FFEFailed
  CIFSInvalid {} -> Just FFEFailed

data ACIFileStatus = forall d. MsgDirectionI d => AFS (SMsgDirection d) (CIFileStatus d)

deriving instance Show ACIFileStatus

instance MsgDirectionI d => StrEncoding (CIFileStatus d) where
  strEncode = \case
    CIFSSndStored -> "snd_stored"
    CIFSSndTransfer sent total -> strEncode (Str "snd_transfer", sent, total)
    CIFSSndCancelled -> "snd_cancelled"
    CIFSSndComplete -> "snd_complete"
    CIFSSndError sndFileErr -> "snd_error " <> strEncode sndFileErr
    CIFSSndWarning sndFileErr -> "snd_warning " <> strEncode sndFileErr
    CIFSRcvInvitation -> "rcv_invitation"
    CIFSRcvAccepted -> "rcv_accepted"
    CIFSRcvTransfer rcvd total -> strEncode (Str "rcv_transfer", rcvd, total)
    CIFSRcvAborted -> "rcv_aborted"
    CIFSRcvComplete -> "rcv_complete"
    CIFSRcvCancelled -> "rcv_cancelled"
    CIFSRcvError rcvFileErr -> "rcv_error " <> strEncode rcvFileErr
    CIFSRcvWarning rcvFileErr -> "rcv_warning " <> strEncode rcvFileErr
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
          "snd_error" -> AFS SMDSnd . CIFSSndError <$> ((A.space *> strP) <|> pure (FileErrOther "")) -- alternative for backwards compatibility
          "snd_warning" -> AFS SMDSnd . CIFSSndWarning <$> (A.space *> strP)
          "rcv_invitation" -> pure $ AFS SMDRcv CIFSRcvInvitation
          "rcv_accepted" -> pure $ AFS SMDRcv CIFSRcvAccepted
          "rcv_transfer" -> AFS SMDRcv <$> progress CIFSRcvTransfer
          "rcv_aborted" -> pure $ AFS SMDRcv CIFSRcvAborted
          "rcv_complete" -> pure $ AFS SMDRcv CIFSRcvComplete
          "rcv_cancelled" -> pure $ AFS SMDRcv CIFSRcvCancelled
          "rcv_error" -> AFS SMDRcv . CIFSRcvError <$> ((A.space *> strP) <|> pure (FileErrOther "")) -- alternative for backwards compatibility
          "rcv_warning" -> AFS SMDRcv . CIFSRcvWarning <$> (A.space *> strP)
          _ -> fail "bad file status"
      progress :: (Int64 -> Int64 -> a) -> A.Parser a
      progress f = f <$> num <*> num <|> pure (f 0 1)
      num = A.space *> A.decimal

data JSONCIFileStatus
  = JCIFSSndStored
  | JCIFSSndTransfer {sndProgress :: Int64, sndTotal :: Int64}
  | JCIFSSndCancelled
  | JCIFSSndComplete
  | JCIFSSndError {sndFileError :: FileError}
  | JCIFSSndWarning {sndFileError :: FileError}
  | JCIFSRcvInvitation
  | JCIFSRcvAccepted
  | JCIFSRcvTransfer {rcvProgress :: Int64, rcvTotal :: Int64}
  | JCIFSRcvAborted
  | JCIFSRcvComplete
  | JCIFSRcvCancelled
  | JCIFSRcvError {rcvFileError :: FileError}
  | JCIFSRcvWarning {rcvFileError :: FileError}
  | JCIFSInvalid {text :: Text}

jsonCIFileStatus :: CIFileStatus d -> JSONCIFileStatus
jsonCIFileStatus = \case
  CIFSSndStored -> JCIFSSndStored
  CIFSSndTransfer sent total -> JCIFSSndTransfer sent total
  CIFSSndCancelled -> JCIFSSndCancelled
  CIFSSndComplete -> JCIFSSndComplete
  CIFSSndError sndFileErr -> JCIFSSndError sndFileErr
  CIFSSndWarning sndFileErr -> JCIFSSndWarning sndFileErr
  CIFSRcvInvitation -> JCIFSRcvInvitation
  CIFSRcvAccepted -> JCIFSRcvAccepted
  CIFSRcvTransfer rcvd total -> JCIFSRcvTransfer rcvd total
  CIFSRcvAborted -> JCIFSRcvAborted
  CIFSRcvComplete -> JCIFSRcvComplete
  CIFSRcvCancelled -> JCIFSRcvCancelled
  CIFSRcvError rcvFileErr -> JCIFSRcvError rcvFileErr
  CIFSRcvWarning rcvFileErr -> JCIFSRcvWarning rcvFileErr
  CIFSInvalid text -> JCIFSInvalid text

aciFileStatusJSON :: JSONCIFileStatus -> ACIFileStatus
aciFileStatusJSON = \case
  JCIFSSndStored -> AFS SMDSnd CIFSSndStored
  JCIFSSndTransfer sent total -> AFS SMDSnd $ CIFSSndTransfer sent total
  JCIFSSndCancelled -> AFS SMDSnd CIFSSndCancelled
  JCIFSSndComplete -> AFS SMDSnd CIFSSndComplete
  JCIFSSndError sndFileErr -> AFS SMDSnd (CIFSSndError sndFileErr)
  JCIFSSndWarning sndFileErr -> AFS SMDSnd (CIFSSndWarning sndFileErr)
  JCIFSRcvInvitation -> AFS SMDRcv CIFSRcvInvitation
  JCIFSRcvAccepted -> AFS SMDRcv CIFSRcvAccepted
  JCIFSRcvTransfer rcvd total -> AFS SMDRcv $ CIFSRcvTransfer rcvd total
  JCIFSRcvAborted -> AFS SMDRcv CIFSRcvAborted
  JCIFSRcvComplete -> AFS SMDRcv CIFSRcvComplete
  JCIFSRcvCancelled -> AFS SMDRcv CIFSRcvCancelled
  JCIFSRcvError rcvFileErr -> AFS SMDRcv (CIFSRcvError rcvFileErr)
  JCIFSRcvWarning rcvFileErr -> AFS SMDRcv (CIFSRcvWarning rcvFileErr)
  JCIFSInvalid text -> AFS SMDSnd $ CIFSInvalid text

data FileError
  = FileErrAuth
  | FileErrNoFile
  | FileErrRelay {srvError :: SrvError}
  | FileErrOther {fileError :: Text}
  deriving (Eq, Show)

instance StrEncoding FileError where
  strEncode = \case
    FileErrAuth -> "auth"
    FileErrNoFile -> "no_file"
    FileErrRelay srvErr -> "relay " <> strEncode srvErr
    FileErrOther e -> "other " <> encodeUtf8 e
  strP =
    A.takeWhile1 (/= ' ') >>= \case
      "auth" -> pure FileErrAuth
      "no_file" -> pure FileErrNoFile
      "relay" -> FileErrRelay <$> (A.space *> strP)
      "other" -> FileErrOther . safeDecodeUtf8 <$> (A.space *> A.takeByteString)
      s -> FileErrOther . safeDecodeUtf8 . (s <>) <$> A.takeByteString

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
  CISSndErrorAuth :: CIStatus 'MDSnd -- deprecated
  CISSndError :: SndError -> CIStatus 'MDSnd
  CISSndWarning :: SndError -> CIStatus 'MDSnd
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
    CISSndError sndErr -> "snd_error " <> strEncode sndErr
    CISSndWarning sndErr -> "snd_warning " <> strEncode sndErr
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
          "snd_error" -> ACIStatus SMDSnd . CISSndError <$> (A.space *> strP)
          "snd_warning" -> ACIStatus SMDSnd . CISSndWarning <$> (A.space *> strP)
          "rcv_new" -> pure $ ACIStatus SMDRcv CISRcvNew
          "rcv_read" -> pure $ ACIStatus SMDRcv CISRcvRead
          _ -> fail "bad status"

-- see serverHostError in agent
data SndError
  = SndErrAuth
  | SndErrQuota
  | SndErrExpired -- TIMEOUT/NETWORK errors
  | SndErrRelay {srvError :: SrvError} -- BROKER errors (other than TIMEOUT/NETWORK)
  | SndErrProxy {proxyServer :: String, srvError :: SrvError} -- SMP PROXY errors
  | SndErrProxyRelay {proxyServer :: String, srvError :: SrvError} -- PROXY BROKER errors
  | SndErrOther {sndError :: Text} -- other errors
  deriving (Eq, Show)

data SrvError
  = SrvErrHost
  | SrvErrVersion
  | SrvErrOther {srvError :: Text}
  deriving (Eq, Show)

instance StrEncoding SndError where
  strEncode = \case
    SndErrAuth -> "auth"
    SndErrQuota -> "quota"
    SndErrExpired -> "expired"
    SndErrRelay srvErr -> "relay " <> strEncode srvErr
    SndErrProxy proxy srvErr -> "proxy " <> encodeUtf8 (T.pack proxy) <> " " <> strEncode srvErr
    SndErrProxyRelay proxy srvErr -> "proxy_relay " <> encodeUtf8 (T.pack proxy) <> " " <> strEncode srvErr
    SndErrOther e -> "other " <> encodeUtf8 e
  strP =
    A.takeWhile1 (/= ' ') >>= \case
      "auth" -> pure SndErrAuth
      "quota" -> pure SndErrQuota
      "expired" -> pure SndErrExpired
      "relay" -> SndErrRelay <$> (A.space *> strP)
      "proxy" -> SndErrProxy . T.unpack . safeDecodeUtf8 <$> (A.space *> A.takeWhile1 (/= ' ') <* A.space) <*> strP
      "proxy_relay" -> SndErrProxyRelay . T.unpack . safeDecodeUtf8 <$> (A.space *> A.takeWhile1 (/= ' ') <* A.space) <*> strP
      "other" -> SndErrOther . safeDecodeUtf8 <$> (A.space *> A.takeByteString)
      s -> SndErrOther . safeDecodeUtf8 . (s <>) <$> A.takeByteString -- for backward compatibility with `CISSndError String`

instance StrEncoding SrvError where
  strEncode = \case
    SrvErrHost -> "host"
    SrvErrVersion -> "version"
    SrvErrOther e -> "other " <> encodeUtf8 e
  strP =
    A.takeWhile1 (/= ' ') >>= \case
      "host" -> pure SrvErrHost
      "version" -> pure SrvErrVersion
      "other" -> SrvErrOther . safeDecodeUtf8 <$> (A.space *> A.takeByteString)
      _ -> fail "bad SrvError"

data JSONCIStatus
  = JCISSndNew
  | JCISSndSent {sndProgress :: SndCIStatusProgress}
  | JCISSndRcvd {msgRcptStatus :: MsgReceiptStatus, sndProgress :: SndCIStatusProgress}
  | JCISSndErrorAuth -- deprecated
  | JCISSndError {agentError :: SndError}
  | JCISSndWarning {agentError :: SndError}
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
  CISSndError sndErr -> JCISSndError sndErr
  CISSndWarning sndErr -> JCISSndWarning sndErr
  CISRcvNew -> JCISRcvNew
  CISRcvRead -> JCISRcvRead
  CISInvalid text -> JCISInvalid text

jsonACIStatus :: JSONCIStatus -> ACIStatus
jsonACIStatus = \case
  JCISSndNew -> ACIStatus SMDSnd CISSndNew
  JCISSndSent sndProgress -> ACIStatus SMDSnd $ CISSndSent sndProgress
  JCISSndRcvd msgRcptStatus sndProgress -> ACIStatus SMDSnd $ CISSndRcvd msgRcptStatus sndProgress
  JCISSndErrorAuth -> ACIStatus SMDSnd CISSndErrorAuth
  JCISSndError sndErr -> ACIStatus SMDSnd $ CISSndError sndErr
  JCISSndWarning sndErr -> ACIStatus SMDSnd $ CISSndWarning sndErr
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

membersGroupItemStatus :: [(GroupSndStatus, Int)] -> CIStatus 'MDSnd
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
    rcvdOk = fromMaybe 0 $ lookup (GSSRcvd MROk) memStatusCounts
    rcvdBad = fromMaybe 0 $ lookup (GSSRcvd MRBadMsgHash) memStatusCounts
    sent = fromMaybe 0 $ lookup GSSSent memStatusCounts

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

data GroupSndStatus
  = GSSNew
  | GSSForwarded
  | GSSInactive
  | GSSSent
  | GSSRcvd {msgRcptStatus :: MsgReceiptStatus}
  | GSSError {agentError :: SndError}
  | GSSWarning {agentError :: SndError}
  | GSSInvalid {text :: Text}

deriving instance Eq GroupSndStatus

deriving instance Show GroupSndStatus

-- Preserve CIStatus encoding for backwards compatibility
instance StrEncoding GroupSndStatus where
  strEncode = \case
    GSSNew -> "snd_new"
    GSSForwarded -> "snd_forwarded"
    GSSInactive -> "snd_inactive"
    GSSSent -> "snd_sent complete"
    GSSRcvd msgRcptStatus -> "snd_rcvd " <> strEncode msgRcptStatus <> " complete"
    GSSError sndErr -> "snd_error " <> strEncode sndErr
    GSSWarning sndErr -> "snd_warning " <> strEncode sndErr
    GSSInvalid {} -> "invalid"
  strP =
    (statusP <* A.endOfInput) -- see ACIStatus decoding
      <|> (GSSInvalid . safeDecodeUtf8 <$> A.takeByteString)
    where
      statusP =
        A.takeTill (== ' ') >>= \case
          "snd_new" -> pure GSSNew
          "snd_forwarded" -> pure GSSForwarded
          "snd_inactive" -> pure GSSInactive
          "snd_sent" -> GSSSent <$ " complete"
          "snd_rcvd" -> GSSRcvd <$> (_strP <* " complete")
          "snd_error_auth" -> pure $ GSSError SndErrAuth
          "snd_error" -> GSSError <$> (A.space *> strP)
          "snd_warning" -> GSSWarning <$> (A.space *> strP)
          _ -> fail "bad status"

type ChatItemId = Int64

type ChatItemTs = UTCTime

data SChatType (c :: ChatType) where
  SCTDirect :: SChatType 'CTDirect
  SCTGroup :: SChatType 'CTGroup
  SCTLocal :: SChatType 'CTLocal
  SCTContactRequest :: SChatType 'CTContactRequest
  SCTContactConnection :: SChatType 'CTContactConnection

deriving instance Show (SChatType c)

instance TestEquality SChatType where
  testEquality SCTDirect SCTDirect = Just Refl
  testEquality SCTGroup SCTGroup = Just Refl
  testEquality SCTLocal SCTLocal = Just Refl
  testEquality SCTContactRequest SCTContactRequest = Just Refl
  testEquality SCTContactConnection SCTContactConnection = Just Refl
  testEquality _ _ = Nothing

data AChatType = forall c. ChatTypeI c => ACT (SChatType c)

class ChatTypeI (c :: ChatType) where
  chatTypeI :: SChatType c

instance ChatTypeI 'CTDirect where chatTypeI = SCTDirect

instance ChatTypeI 'CTGroup where chatTypeI = SCTGroup

instance ChatTypeI 'CTLocal where chatTypeI = SCTLocal

instance ChatTypeI 'CTContactRequest where chatTypeI = SCTContactRequest

instance ChatTypeI 'CTContactConnection where chatTypeI = SCTContactConnection

toChatType :: SChatType c -> ChatType
toChatType = \case
  SCTDirect -> CTDirect
  SCTGroup -> CTGroup
  SCTLocal -> CTLocal
  SCTContactRequest -> CTContactRequest
  SCTContactConnection -> CTContactConnection

aChatType :: ChatType -> AChatType
aChatType = \case
  CTDirect -> ACT SCTDirect
  CTGroup -> ACT SCTGroup
  CTLocal -> ACT SCTLocal
  CTContactRequest -> ACT SCTContactRequest
  CTContactConnection -> ACT SCTContactConnection

checkChatType :: forall t c c'. (ChatTypeI c, ChatTypeI c') => t c' -> Either String (t c)
checkChatType x = case testEquality (chatTypeI @c) (chatTypeI @c') of
  Just Refl -> Right x
  Nothing -> Left "bad chat type"

data SndMessage = SndMessage
  { msgId :: MessageId,
    sharedMsgId :: SharedMsgId,
    msgBody :: MsgBody
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
    agentMsgMeta :: MsgMeta
  }
  deriving (Show)

data RcvMsgInfo = RcvMsgInfo
  { msgId :: Int64,
    msgDeliveryId :: Int64,
    msgDeliveryStatus :: Text,
    agentMsgId :: AgentMsgId,
    agentMsgMeta :: Text
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
  MDSRcvAcknowledged :: MsgDeliveryStatus 'MDRcv -- not used
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
  CIBlockedByAdmin :: Maybe UTCTime -> CIDeleted 'CTGroup
  CIModerated :: Maybe UTCTime -> GroupMember -> CIDeleted 'CTGroup

deriving instance Show (CIDeleted c)

data ACIDeleted = forall c. ChatTypeI c => ACIDeleted (SChatType c) (CIDeleted c)

data JSONCIDeleted
  = JCIDDeleted {deletedTs :: Maybe UTCTime, chatType :: ChatType}
  | JCIDBlocked {deletedTs :: Maybe UTCTime}
  | JCIDBlockedByAdmin {deletedTs :: Maybe UTCTime}
  | JCIDModerated {deletedTs :: Maybe UTCTime, byGroupMember :: GroupMember}
  deriving (Show)

jsonCIDeleted :: forall d. ChatTypeI d => CIDeleted d -> JSONCIDeleted
jsonCIDeleted = \case
  CIDeleted ts -> JCIDDeleted ts (toChatType $ chatTypeI @d)
  CIBlocked ts -> JCIDBlocked ts
  CIBlockedByAdmin ts -> JCIDBlockedByAdmin ts
  CIModerated ts m -> JCIDModerated ts m

jsonACIDeleted :: JSONCIDeleted -> ACIDeleted
jsonACIDeleted = \case
  JCIDDeleted ts cType -> case aChatType cType of ACT c -> ACIDeleted c $ CIDeleted ts
  JCIDBlocked ts -> ACIDeleted SCTGroup $ CIBlocked ts
  JCIDBlockedByAdmin ts -> ACIDeleted SCTGroup $ CIBlockedByAdmin ts
  JCIDModerated ts m -> ACIDeleted SCTGroup (CIModerated ts m)

itemDeletedTs :: CIDeleted d -> Maybe UTCTime
itemDeletedTs = \case
  CIDeleted ts -> ts
  CIBlocked ts -> ts
  CIBlockedByAdmin ts -> ts
  CIModerated ts _ -> ts

data CIForwardedFrom
  = CIFFUnknown
  | CIFFContact {chatName :: Text, msgDir :: MsgDirection, contactId :: Maybe ContactId, chatItemId :: Maybe ChatItemId}
  | CIFFGroup {chatName :: Text, msgDir :: MsgDirection, groupId :: Maybe GroupId, chatItemId :: Maybe ChatItemId}
  deriving (Show)

cmForwardedFrom :: AChatMsgEvent -> Maybe CIForwardedFrom
cmForwardedFrom = \case
  ACME _ (XMsgNew (MCForward _)) -> Just CIFFUnknown
  _ -> Nothing

data CIForwardedFromTag
  = CIFFUnknown_
  | CIFFContact_
  | CIFFGroup_

instance FromField CIForwardedFromTag where fromField = fromTextField_ textDecode

instance ToField CIForwardedFromTag where toField = toField . textEncode

instance TextEncoding CIForwardedFromTag where
  textDecode = \case
    "unknown" -> Just CIFFUnknown_
    "contact" -> Just CIFFContact_
    "group" -> Just CIFFGroup_
    _ -> Nothing
  textEncode = \case
    CIFFUnknown_ -> "unknown"
    CIFFContact_ -> "contact"
    CIFFGroup_ -> "group"

data ChatItemInfo = ChatItemInfo
  { itemVersions :: [ChatItemVersion],
    memberDeliveryStatuses :: Maybe (NonEmpty MemberDeliveryStatus),
    forwardedFromChatItem :: Maybe AChatItem
  }
  deriving (Show)

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
    memberDeliveryStatus :: GroupSndStatus,
    sentViaProxy :: Maybe Bool
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

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CIFF") ''CIForwardedFrom)

$(JQ.deriveJSON defaultJSON ''CITimed)

$(JQ.deriveJSON (enumJSON $ dropPrefix "SSP") ''SndCIStatusProgress)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "SrvErr") ''SrvError)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "SndErr") ''SndError)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "JCIS") ''JSONCIStatus)

instance MsgDirectionI d => FromJSON (CIStatus d) where
  parseJSON v = (\(ACIStatus _ s) -> checkDirection s) . jsonACIStatus <$?> J.parseJSON v

instance ToJSON (CIStatus d) where
  toJSON = J.toJSON . jsonCIStatus
  toEncoding = J.toEncoding . jsonCIStatus

instance MsgDirectionI d => ToField (CIStatus d) where toField = toField . decodeLatin1 . strEncode

instance (Typeable d, MsgDirectionI d) => FromField (CIStatus d) where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

instance FromField ACIStatus where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "GSS") ''GroupSndStatus)

instance ToField GroupSndStatus where toField = toField . decodeLatin1 . strEncode

instance FromField GroupSndStatus where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

$(JQ.deriveJSON defaultJSON ''MemberDeliveryStatus)

$(JQ.deriveJSON defaultJSON ''ChatItemVersion)

instance (ChatTypeI c, MsgDirectionI d) => FromJSON (CIMeta c d) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''CIMeta)

instance ChatTypeI c => ToJSON (CIMeta c d) where
  toJSON = $(JQ.mkToJSON defaultJSON ''CIMeta)
  toEncoding = $(JQ.mkToEncoding defaultJSON ''CIMeta)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "FileErr") ''FileError)

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
  parseJSON v = (jsonACIQDirection >=> \(ACIQDirection _ x) -> checkChatType x) <$?> J.parseJSON v

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

$(JQ.deriveJSON defaultJSON ''ChatItemInfo)

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

$(JQ.deriveJSON defaultJSON ''RcvMsgInfo)
