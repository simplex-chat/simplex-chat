{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Simplex.Chat.Types where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable
import Database.SQLite.Simple (ResultError (..), SQLData (..))
import Database.SQLite.Simple.FromField (FieldParser, FromField (..), returnError)
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import Simplex.Messaging.Agent.Protocol (ConnId, ConnectionMode (..), ConnectionRequestUri, InvitationId)
import Simplex.Messaging.Agent.Store.SQLite (fromTextField_)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, sumTypeJSON)
import Simplex.Messaging.Util ((<$?>))

class IsContact a where
  contactId' :: a -> Int64
  profile' :: a -> Profile
  localDisplayName' :: a -> ContactName

instance IsContact User where
  contactId' = userContactId
  profile' = profile
  localDisplayName' = localDisplayName

instance IsContact Contact where
  contactId' = contactId
  profile' = profile
  localDisplayName' = localDisplayName

data User = User
  { userId :: UserId,
    userContactId :: Int64,
    localDisplayName :: ContactName,
    profile :: Profile,
    activeUser :: Bool
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON User where toEncoding = J.genericToEncoding J.defaultOptions

type UserId = Int64

data Contact = Contact
  { contactId :: Int64,
    localDisplayName :: ContactName,
    profile :: Profile,
    activeConn :: Connection,
    viaGroup :: Maybe Int64,
    createdAt :: UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON Contact where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

contactConn :: Contact -> Connection
contactConn = activeConn

contactConnId :: Contact -> ConnId
contactConnId Contact {activeConn} = aConnId activeConn

data UserContact = UserContact
  { userContactLinkId :: Int64,
    connReqContact :: ConnReqContact
  }
  deriving (Eq, Show)

data UserContactRequest = UserContactRequest
  { contactRequestId :: Int64,
    agentInvitationId :: AgentInvId,
    userContactLinkId :: Int64,
    agentContactConnId :: AgentConnId, -- connection id of user contact
    localDisplayName :: ContactName,
    profileId :: Int64,
    profile :: Profile,
    createdAt :: UTCTime,
    xContactId :: Maybe XContactId
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON UserContactRequest where
  toEncoding = J.genericToEncoding J.defaultOptions

newtype XContactId = XContactId ByteString
  deriving (Eq, Show)

instance FromField XContactId where fromField f = XContactId <$> fromField f

instance ToField XContactId where toField (XContactId m) = toField m

instance StrEncoding XContactId where
  strEncode (XContactId m) = strEncode m
  strDecode s = XContactId <$> strDecode s
  strP = XContactId <$> strP

instance FromJSON XContactId where
  parseJSON = strParseJSON "XContactId"

instance ToJSON XContactId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

newtype ConnReqUriHash = ConnReqUriHash {unConnReqUriHash :: ByteString}
  deriving (Eq, Show)

instance FromField ConnReqUriHash where fromField f = ConnReqUriHash <$> fromField f

instance ToField ConnReqUriHash where toField (ConnReqUriHash m) = toField m

instance StrEncoding ConnReqUriHash where
  strEncode (ConnReqUriHash m) = strEncode m
  strDecode s = ConnReqUriHash <$> strDecode s
  strP = ConnReqUriHash <$> strP

instance FromJSON ConnReqUriHash where
  parseJSON = strParseJSON "ConnReqUriHash"

instance ToJSON ConnReqUriHash where
  toJSON = strToJSON
  toEncoding = strToJEncoding

type ContactName = Text

type GroupName = Text

data Group = Group {groupInfo :: GroupInfo, members :: [GroupMember]}
  deriving (Eq, Show, Generic)

instance ToJSON Group where toEncoding = J.genericToEncoding J.defaultOptions

data GroupInfo = GroupInfo
  { groupId :: Int64,
    localDisplayName :: GroupName,
    groupProfile :: GroupProfile,
    membership :: GroupMember,
    createdAt :: UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON GroupInfo where toEncoding = J.genericToEncoding J.defaultOptions

groupName' :: GroupInfo -> GroupName
groupName' GroupInfo {localDisplayName = g} = g

data Profile = Profile
  { displayName :: ContactName,
    fullName :: Text,
    image :: Maybe ProfileImage
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON Profile where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data GroupProfile = GroupProfile
  { displayName :: GroupName,
    fullName :: Text,
    image :: Maybe ProfileImage
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON GroupProfile where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

newtype ProfileImage = ProfileImage Text
  deriving (Eq, Show)

instance FromJSON ProfileImage where
  parseJSON = fmap ProfileImage . J.parseJSON

instance ToJSON ProfileImage where
  toJSON (ProfileImage t) = J.toJSON t
  toEncoding (ProfileImage t) = J.toEncoding t

instance ToField ProfileImage where toField (ProfileImage t) = toField t

instance FromField ProfileImage where fromField = fmap ProfileImage . fromField

data GroupInvitation = GroupInvitation
  { fromMember :: MemberIdRole,
    invitedMember :: MemberIdRole,
    connRequest :: ConnReqInvitation,
    groupProfile :: GroupProfile
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON GroupInvitation where toEncoding = J.genericToEncoding J.defaultOptions

data MemberIdRole = MemberIdRole
  { memberId :: MemberId,
    memberRole :: GroupMemberRole
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON MemberIdRole where toEncoding = J.genericToEncoding J.defaultOptions

data IntroInvitation = IntroInvitation
  { groupConnReq :: ConnReqInvitation,
    directConnReq :: ConnReqInvitation
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON IntroInvitation where toEncoding = J.genericToEncoding J.defaultOptions

data MemberInfo = MemberInfo
  { memberId :: MemberId,
    memberRole :: GroupMemberRole,
    profile :: Profile
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON MemberInfo where toEncoding = J.genericToEncoding J.defaultOptions

memberInfo :: GroupMember -> MemberInfo
memberInfo GroupMember {memberId, memberRole, memberProfile} =
  MemberInfo memberId memberRole memberProfile

data ReceivedGroupInvitation = ReceivedGroupInvitation
  { fromMember :: GroupMember,
    connRequest :: ConnReqInvitation,
    groupInfo :: GroupInfo
  }
  deriving (Eq, Show)

data GroupMember = GroupMember
  { groupMemberId :: Int64,
    groupId :: Int64,
    memberId :: MemberId,
    memberRole :: GroupMemberRole,
    memberCategory :: GroupMemberCategory,
    memberStatus :: GroupMemberStatus,
    invitedBy :: InvitedBy,
    localDisplayName :: ContactName,
    memberProfile :: Profile,
    memberContactId :: Maybe Int64,
    activeConn :: Maybe Connection
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON GroupMember where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

memberConn :: GroupMember -> Maybe Connection
memberConn = activeConn

memberConnId :: GroupMember -> Maybe ConnId
memberConnId GroupMember {activeConn} = aConnId <$> activeConn

data NewGroupMember = NewGroupMember
  { memInfo :: MemberInfo,
    memCategory :: GroupMemberCategory,
    memStatus :: GroupMemberStatus,
    memInvitedBy :: InvitedBy,
    localDisplayName :: ContactName,
    memProfileId :: Int64,
    memContactId :: Maybe Int64
  }

newtype MemberId = MemberId {unMemberId :: ByteString}
  deriving (Eq, Show)

instance FromField MemberId where fromField f = MemberId <$> fromField f

instance ToField MemberId where toField (MemberId m) = toField m

instance StrEncoding MemberId where
  strEncode (MemberId m) = strEncode m
  strDecode s = MemberId <$> strDecode s
  strP = MemberId <$> strP

instance FromJSON MemberId where
  parseJSON = strParseJSON "MemberId"

instance ToJSON MemberId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data InvitedBy = IBContact {byContactId :: Int64} | IBUser | IBUnknown
  deriving (Eq, Show, Generic)

instance FromJSON InvitedBy where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "IB"

instance ToJSON InvitedBy where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "IB"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "IB"

toInvitedBy :: Int64 -> Maybe Int64 -> InvitedBy
toInvitedBy userCtId (Just ctId)
  | userCtId == ctId = IBUser
  | otherwise = IBContact ctId
toInvitedBy _ Nothing = IBUnknown

fromInvitedBy :: Int64 -> InvitedBy -> Maybe Int64
fromInvitedBy userCtId = \case
  IBUnknown -> Nothing
  IBContact ctId -> Just ctId
  IBUser -> Just userCtId

data GroupMemberRole = GRMember | GRAdmin | GROwner
  deriving (Eq, Show, Ord)

instance FromField GroupMemberRole where fromField = fromBlobField_ strDecode

instance ToField GroupMemberRole where toField = toField . strEncode

instance StrEncoding GroupMemberRole where
  strEncode = \case
    GROwner -> "owner"
    GRAdmin -> "admin"
    GRMember -> "member"
  strDecode = \case
    "owner" -> Right GROwner
    "admin" -> Right GRAdmin
    "member" -> Right GRMember
    r -> Left $ "bad GroupMemberRole " <> B.unpack r
  strP = strDecode <$?> A.takeByteString

instance FromJSON GroupMemberRole where
  parseJSON = strParseJSON "GroupMemberRole"

instance ToJSON GroupMemberRole where
  toJSON = strToJSON
  toEncoding = strToJEncoding

fromBlobField_ :: Typeable k => (ByteString -> Either String k) -> FieldParser k
fromBlobField_ p = \case
  f@(Field (SQLBlob b) _) ->
    case p b of
      Right k -> Ok k
      Left e -> returnError ConversionFailed f ("could not parse field: " ++ e)
  f -> returnError ConversionFailed f "expecting SQLBlob column type"

newtype Probe = Probe {unProbe :: ByteString}
  deriving (Eq, Show)

instance StrEncoding Probe where
  strEncode (Probe p) = strEncode p
  strDecode s = Probe <$> strDecode s
  strP = Probe <$> strP

instance FromJSON Probe where
  parseJSON = strParseJSON "Probe"

instance ToJSON Probe where
  toJSON = strToJSON
  toEncoding = strToJEncoding

newtype ProbeHash = ProbeHash {unProbeHash :: ByteString}
  deriving (Eq, Show)

instance StrEncoding ProbeHash where
  strEncode (ProbeHash p) = strEncode p
  strDecode s = ProbeHash <$> strDecode s
  strP = ProbeHash <$> strP

instance FromJSON ProbeHash where
  parseJSON = strParseJSON "ProbeHash"

instance ToJSON ProbeHash where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data GroupMemberCategory
  = GCUserMember
  | GCInviteeMember -- member invited by the user
  | GCHostMember -- member who invited the user
  | GCPreMember -- member who joined before the user and was introduced to the user (user receives x.grp.mem.intro about such members)
  | GCPostMember -- member who joined after the user to whom the user was introduced (user receives x.grp.mem.new announcing these members and then x.grp.mem.fwd with invitation from these members)
  deriving (Eq, Show)

instance FromField GroupMemberCategory where fromField = fromTextField_ decodeText

instance ToField GroupMemberCategory where toField = toField . encodeText

instance FromJSON GroupMemberCategory where parseJSON = textParseJSON "GroupMemberCategory"

instance ToJSON GroupMemberCategory where
  toJSON = J.String . encodeText
  toEncoding = JE.text . encodeText

instance TextEncoding GroupMemberCategory where
  decodeText = \case
    "user" -> Just GCUserMember
    "invitee" -> Just GCInviteeMember
    "host" -> Just GCHostMember
    "pre" -> Just GCPreMember
    "post" -> Just GCPostMember
    _ -> Nothing
  encodeText = \case
    GCUserMember -> "user"
    GCInviteeMember -> "invitee"
    GCHostMember -> "host"
    GCPreMember -> "pre"
    GCPostMember -> "post"

data GroupMemberStatus
  = GSMemRemoved -- member who was removed from the group
  | GSMemLeft -- member who left the group
  | GSMemGroupDeleted -- user member of the deleted group
  | GSMemInvited -- member is sent to or received invitation to join the group
  | GSMemIntroduced -- user received x.grp.mem.intro for this member (only with GCPreMember)
  | GSMemIntroInvited -- member is sent to or received from intro invitation
  | GSMemAccepted -- member accepted invitation (only User and Invitee)
  | GSMemAnnounced -- host announced (x.grp.mem.new) a member (Invitee and PostMember) to the group - at this point this member can send messages and invite other members (if they have sufficient permissions)
  | GSMemConnected -- member created the group connection with the inviting member
  | GSMemComplete -- host confirmed (x.grp.mem.all) that a member (User, Invitee and PostMember) created group connections with all previous members
  | GSMemCreator -- user member that created the group (only GCUserMember)
  deriving (Eq, Show, Ord)

instance FromField GroupMemberStatus where fromField = fromTextField_ decodeText

instance ToField GroupMemberStatus where toField = toField . encodeText

instance FromJSON GroupMemberStatus where parseJSON = textParseJSON "GroupMemberStatus"

instance ToJSON GroupMemberStatus where
  toJSON = J.String . encodeText
  toEncoding = JE.text . encodeText

memberActive :: GroupMember -> Bool
memberActive m = case memberStatus m of
  GSMemRemoved -> False
  GSMemLeft -> False
  GSMemGroupDeleted -> False
  GSMemInvited -> False
  GSMemIntroduced -> False
  GSMemIntroInvited -> False
  GSMemAccepted -> False
  GSMemAnnounced -> False
  GSMemConnected -> True
  GSMemComplete -> True
  GSMemCreator -> True

memberCurrent :: GroupMember -> Bool
memberCurrent m = case memberStatus m of
  GSMemRemoved -> False
  GSMemLeft -> False
  GSMemGroupDeleted -> False
  GSMemInvited -> False
  GSMemIntroduced -> True
  GSMemIntroInvited -> True
  GSMemAccepted -> True
  GSMemAnnounced -> True
  GSMemConnected -> True
  GSMemComplete -> True
  GSMemCreator -> True

instance TextEncoding GroupMemberStatus where
  decodeText = \case
    "removed" -> Just GSMemRemoved
    "left" -> Just GSMemLeft
    "deleted" -> Just GSMemGroupDeleted
    "invited" -> Just GSMemInvited
    "introduced" -> Just GSMemIntroduced
    "intro-inv" -> Just GSMemIntroInvited
    "accepted" -> Just GSMemAccepted
    "announced" -> Just GSMemAnnounced
    "connected" -> Just GSMemConnected
    "complete" -> Just GSMemComplete
    "creator" -> Just GSMemCreator
    _ -> Nothing
  encodeText = \case
    GSMemRemoved -> "removed"
    GSMemLeft -> "left"
    GSMemGroupDeleted -> "deleted"
    GSMemInvited -> "invited"
    GSMemIntroduced -> "introduced"
    GSMemIntroInvited -> "intro-inv"
    GSMemAccepted -> "accepted"
    GSMemAnnounced -> "announced"
    GSMemConnected -> "connected"
    GSMemComplete -> "complete"
    GSMemCreator -> "creator"

data SndFileTransfer = SndFileTransfer
  { fileId :: FileTransferId,
    fileName :: String,
    filePath :: String,
    fileSize :: Integer,
    chunkSize :: Integer,
    recipientDisplayName :: ContactName,
    connId :: Int64,
    agentConnId :: AgentConnId,
    fileStatus :: FileStatus
  }
  deriving (Eq, Show, Generic)

instance ToJSON SndFileTransfer where toEncoding = J.genericToEncoding J.defaultOptions

-- new file protocol - snd file transfer that wasn't accepted
data SndPendingFileTransfer = SndPendingFileTransfer
  { fileId :: FileTransferId,
    fileName :: String,
    filePath :: String,
    fileSize :: Integer,
    chunkSize :: Integer
  }
  deriving (Eq, Show, Generic)

instance ToJSON SndPendingFileTransfer where toEncoding = J.genericToEncoding J.defaultOptions

type FileTransferId = Int64

data FileInvitation = FileInvitation
  { fileName :: String,
    fileSize :: Integer,
    fileConnReq :: Maybe ConnReqInvitation
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON FileInvitation where toEncoding = J.genericToEncoding J.defaultOptions

data RcvFileTransfer = RcvFileTransfer
  { fileId :: FileTransferId,
    fileInvitation :: FileInvitation,
    fileStatus :: RcvFileStatus,
    senderDisplayName :: ContactName,
    chunkSize :: Integer,
    grpMemberId :: Maybe Int64
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON RcvFileTransfer where toEncoding = J.genericToEncoding J.defaultOptions

data RcvFileStatus
  = RFSNew
  | RFSAccepted RcvFileInfo
  | RFSConnected RcvFileInfo
  | RFSComplete RcvFileInfo
  | RFSCancelled RcvFileInfo
  deriving (Eq, Show, Generic)

instance FromJSON RcvFileStatus where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "RFS"

instance ToJSON RcvFileStatus where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "RFS"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "RFS"

data RcvFileInfo = RcvFileInfo
  { filePath :: FilePath,
    connId :: Int64,
    agentConnId :: AgentConnId
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON RcvFileInfo where toEncoding = J.genericToEncoding J.defaultOptions

newtype AgentConnId = AgentConnId ConnId
  deriving (Eq, Show)

instance StrEncoding AgentConnId where
  strEncode (AgentConnId connId) = strEncode connId
  strDecode s = AgentConnId <$> strDecode s
  strP = AgentConnId <$> strP

instance FromJSON AgentConnId where
  parseJSON = strParseJSON "AgentConnId"

instance ToJSON AgentConnId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromField AgentConnId where fromField f = AgentConnId <$> fromField f

instance ToField AgentConnId where toField (AgentConnId m) = toField m

newtype AgentInvId = AgentInvId InvitationId
  deriving (Eq, Show)

instance StrEncoding AgentInvId where
  strEncode (AgentInvId connId) = strEncode connId
  strDecode s = AgentInvId <$> strDecode s
  strP = AgentInvId <$> strP

instance FromJSON AgentInvId where
  parseJSON = strParseJSON "AgentInvId"

instance ToJSON AgentInvId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromField AgentInvId where fromField f = AgentInvId <$> fromField f

instance ToField AgentInvId where toField (AgentInvId m) = toField m

data FileTransfer
  = FTSnd
      { fileTransferMeta :: FileTransferMeta,
        sndFileTransfers :: [SndFileTransfer]
      }
  | FTRcv {rcvFileTransfer :: RcvFileTransfer}
  deriving (Show, Generic)

instance ToJSON FileTransfer where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "FT"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "FT"

data FileTransferMeta = FileTransferMeta
  { fileId :: FileTransferId,
    fileName :: String,
    filePath :: String,
    fileSize :: Integer,
    chunkSize :: Integer
  }
  deriving (Eq, Show, Generic)

instance ToJSON FileTransferMeta where toEncoding = J.genericToEncoding J.defaultOptions

data FileStatus = FSNew | FSAccepted | FSConnected | FSComplete | FSCancelled deriving (Eq, Ord, Show)

instance FromField FileStatus where fromField = fromTextField_ decodeText

instance ToField FileStatus where toField = toField . encodeText

instance FromJSON FileStatus where parseJSON = textParseJSON "FileStatus"

instance ToJSON FileStatus where
  toJSON = J.String . encodeText
  toEncoding = JE.text . encodeText

instance TextEncoding FileStatus where
  decodeText = \case
    "new" -> Just FSNew
    "accepted" -> Just FSAccepted
    "connected" -> Just FSConnected
    "complete" -> Just FSComplete
    "cancelled" -> Just FSCancelled
    _ -> Nothing
  encodeText = \case
    FSNew -> "new"
    FSAccepted -> "accepted"
    FSConnected -> "connected"
    FSComplete -> "complete"
    FSCancelled -> "cancelled"

data RcvChunkStatus = RcvChunkOk | RcvChunkFinal | RcvChunkDuplicate | RcvChunkError
  deriving (Eq, Show)

type ConnReqInvitation = ConnectionRequestUri 'CMInvitation

type ConnReqContact = ConnectionRequestUri 'CMContact

data Connection = Connection
  { connId :: Int64,
    agentConnId :: AgentConnId,
    connLevel :: Int,
    viaContact :: Maybe Int64,
    connType :: ConnType,
    connStatus :: ConnStatus,
    entityId :: Maybe Int64, -- contact, group member, file ID or user contact ID
    createdAt :: UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON)

aConnId :: Connection -> ConnId
aConnId Connection {agentConnId = AgentConnId cId} = cId

instance ToJSON Connection where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data ConnStatus
  = -- | connection is created by initiating party with agent NEW command (createConnection)
    ConnNew
  | -- | connection is joined by joining party with agent JOIN command (joinConnection)
    ConnJoined
  | -- | initiating party received CONF notification (to be renamed to REQ)
    ConnRequested
  | -- | initiating party accepted connection with agent LET command (to be renamed to ACPT) (allowConnection)
    ConnAccepted
  | -- | connection can be sent messages to (after joining party received INFO notification)
    ConnSndReady
  | -- | connection is ready for both parties to send and receive messages
    ConnReady
  | -- | connection deleted
    ConnDeleted
  deriving (Eq, Show)

instance FromField ConnStatus where fromField = fromTextField_ decodeText

instance ToField ConnStatus where toField = toField . encodeText

instance FromJSON ConnStatus where parseJSON = textParseJSON "ConnStatus"

instance ToJSON ConnStatus where
  toJSON = J.String . encodeText
  toEncoding = JE.text . encodeText

instance TextEncoding ConnStatus where
  decodeText = \case
    "new" -> Just ConnNew
    "joined" -> Just ConnJoined
    "requested" -> Just ConnRequested
    "accepted" -> Just ConnAccepted
    "snd-ready" -> Just ConnSndReady
    "ready" -> Just ConnReady
    "deleted" -> Just ConnDeleted
    _ -> Nothing
  encodeText = \case
    ConnNew -> "new"
    ConnJoined -> "joined"
    ConnRequested -> "requested"
    ConnAccepted -> "accepted"
    ConnSndReady -> "snd-ready"
    ConnReady -> "ready"
    ConnDeleted -> "deleted"

data ConnType = ConnContact | ConnMember | ConnSndFile | ConnRcvFile | ConnUserContact
  deriving (Eq, Show)

instance FromField ConnType where fromField = fromTextField_ decodeText

instance ToField ConnType where toField = toField . encodeText

instance FromJSON ConnType where parseJSON = textParseJSON "ConnType"

instance ToJSON ConnType where
  toJSON = J.String . encodeText
  toEncoding = JE.text . encodeText

instance TextEncoding ConnType where
  decodeText = \case
    "contact" -> Just ConnContact
    "member" -> Just ConnMember
    "snd_file" -> Just ConnSndFile
    "rcv_file" -> Just ConnRcvFile
    "user_contact" -> Just ConnUserContact
    _ -> Nothing
  encodeText = \case
    ConnContact -> "contact"
    ConnMember -> "member"
    ConnSndFile -> "snd_file"
    ConnRcvFile -> "rcv_file"
    ConnUserContact -> "user_contact"

data NewConnection = NewConnection
  { agentConnId :: ByteString,
    connLevel :: Int,
    viaConn :: Maybe Int64
  }

data GroupMemberIntro = GroupMemberIntro
  { introId :: Int64,
    reMember :: GroupMember,
    toMember :: GroupMember,
    introStatus :: GroupMemberIntroStatus,
    introInvitation :: Maybe IntroInvitation
  }
  deriving (Show)

data GroupMemberIntroStatus
  = GMIntroPending
  | GMIntroSent
  | GMIntroInvReceived
  | GMIntroInvForwarded
  | GMIntroReConnected
  | GMIntroToConnected
  | GMIntroConnected
  deriving (Show)

instance FromField GroupMemberIntroStatus where fromField = fromTextField_ introStatusT

instance ToField GroupMemberIntroStatus where toField = toField . serializeIntroStatus

introStatusT :: Text -> Maybe GroupMemberIntroStatus
introStatusT = \case
  "new" -> Just GMIntroPending
  "sent" -> Just GMIntroSent
  "rcv" -> Just GMIntroInvReceived
  "fwd" -> Just GMIntroInvForwarded
  "re-con" -> Just GMIntroReConnected
  "to-con" -> Just GMIntroToConnected
  "con" -> Just GMIntroConnected
  _ -> Nothing

serializeIntroStatus :: GroupMemberIntroStatus -> Text
serializeIntroStatus = \case
  GMIntroPending -> "new"
  GMIntroSent -> "sent"
  GMIntroInvReceived -> "rcv"
  GMIntroInvForwarded -> "fwd"
  GMIntroReConnected -> "re-con"
  GMIntroToConnected -> "to-con"
  GMIntroConnected -> "con"

data Notification = Notification {title :: Text, text :: Text}

type JSONString = String

class TextEncoding a where
  encodeText :: a -> Text
  decodeText :: Text -> Maybe a

textParseJSON :: TextEncoding a => String -> J.Value -> JT.Parser a
textParseJSON name = J.withText name $ maybe (fail $ "bad " <> name) pure . decodeText
