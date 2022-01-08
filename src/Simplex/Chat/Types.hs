{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat.Types where

import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Time.Clock (UTCTime)
import Data.Type.Equality
import Data.Typeable (Typeable)
import Database.SQLite.Simple (ResultError (..), SQLData (..))
import Database.SQLite.Simple.FromField (FieldParser, FromField (..), returnError)
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics
import Simplex.Chat.SimpleXMQ
import "simplexmq-legacy" Simplex.Messaging.Agent.Protocol (AgentMsgId, ConnId, ConnectionMode (..), ConnectionRequest, InvitationId, MsgMeta (..), serializeMsgIntegrity)
import "simplexmq-legacy" Simplex.Messaging.Agent.Store.SQLite (fromTextField_)
import Simplex.Messaging.Encoding.String
import "simplexmq" Simplex.Messaging.Protocol (MsgBody)
import "simplexmq" Simplex.Messaging.Util ((<$?>))

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

type UserId = Int64

data Contact = Contact
  { contactId :: Int64,
    localDisplayName :: ContactName,
    profile :: Profile,
    activeConn :: Connection,
    viaGroup :: Maybe Int64
  }
  deriving (Eq, Show)

contactConn :: Contact -> Connection
contactConn = activeConn

contactConnId :: Contact -> ConnId
contactConnId Contact {activeConn = Connection {agentConnId}} = agentConnId

data UserContact = UserContact
  { userContactLinkId :: Int64,
    connReqContact :: ConnReqContact
  }
  deriving (Eq, Show)

data UserContactRequest = UserContactRequest
  { contactRequestId :: Int64,
    agentInvitationId :: InvitationId,
    userContactLinkId :: Int64,
    agentContactConnId :: ConnId,
    localDisplayName :: ContactName,
    profileId :: Int64
  }
  deriving (Eq, Show)

type ContactName = Text

type GroupName = Text

data Group = Group
  { groupId :: Int64,
    localDisplayName :: GroupName,
    groupProfile :: GroupProfile,
    members :: [GroupMember],
    membership :: GroupMember
  }
  deriving (Eq, Show)

data Profile = Profile
  { displayName :: ContactName,
    fullName :: Text
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON Profile where toEncoding = J.genericToEncoding J.defaultOptions

data GroupProfile = GroupProfile
  { displayName :: GroupName,
    fullName :: Text
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON GroupProfile where toEncoding = J.genericToEncoding J.defaultOptions

data GroupInvitation = GroupInvitation
  { fromMember :: MemberIdRole,
    invitedMember :: MemberIdRole,
    connRequest :: ConnReqInv 'AgentV0,
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
  { groupConnReq :: ConnReqInv 'AgentV0,
    directConnReq :: ConnReqInv 'AgentV0
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
    userMember :: GroupMember,
    connRequest :: ConnReqInv 'AgentV0,
    groupProfile :: GroupProfile
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
  deriving (Eq, Show)

memberConn :: GroupMember -> Maybe Connection
memberConn = activeConn

memberConnId :: GroupMember -> Maybe ConnId
memberConnId GroupMember {activeConn} = case activeConn of
  Just Connection {agentConnId} -> Just agentConnId
  Nothing -> Nothing

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

data InvitedBy = IBContact Int64 | IBUser | IBUnknown
  deriving (Eq, Show)

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

instance FromField GroupMemberCategory where fromField = fromTextField_ memberCategoryT

instance ToField GroupMemberCategory where toField = toField . serializeMemberCategory

memberCategoryT :: Text -> Maybe GroupMemberCategory
memberCategoryT = \case
  "user" -> Just GCUserMember
  "invitee" -> Just GCInviteeMember
  "host" -> Just GCHostMember
  "pre" -> Just GCPreMember
  "post" -> Just GCPostMember
  _ -> Nothing

serializeMemberCategory :: GroupMemberCategory -> Text
serializeMemberCategory = \case
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

instance FromField GroupMemberStatus where fromField = fromTextField_ memberStatusT

instance ToField GroupMemberStatus where toField = toField . serializeMemberStatus

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

memberStatusT :: Text -> Maybe GroupMemberStatus
memberStatusT = \case
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

serializeMemberStatus :: GroupMemberStatus -> Text
serializeMemberStatus = \case
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
  { fileId :: Int64,
    fileName :: String,
    filePath :: String,
    fileSize :: Integer,
    chunkSize :: Integer,
    recipientDisplayName :: ContactName,
    connId :: Int64,
    agentConnId :: ConnId,
    fileStatus :: FileStatus
  }
  deriving (Eq, Show)

data FileInvitation = FileInvitation
  { fileName :: String,
    fileSize :: Integer,
    fileConnReq :: ConnReqInv 'AgentV0
  }
  deriving (Eq, Show, Generic)

instance FromJSON FileInvitation where
  parseJSON (J.Object v) = FileInvitation <$> v .: "fileName" <*> v .: "fileSize" <*> v .: "fileConnReq"
  parseJSON invalid = JT.prependFailure "bad FileInvitation, " (JT.typeMismatch "Object" invalid)

instance ToJSON FileInvitation where
  toJSON (FileInvitation fileName fileSize fileConnReq) =
    J.object
      [ "fileName" .= fileName,
        "fileSize" .= fileSize,
        "fileConnReq" .= fileConnReq
      ]
  toEncoding (FileInvitation fileName fileSize fileConnReq) =
    J.pairs $
      "fileName" .= fileName
        <> "fileSize" .= fileSize
        <> "fileConnReq" .= fileConnReq

data RcvFileTransfer = RcvFileTransfer
  { fileId :: Int64,
    fileInvitation :: FileInvitation,
    fileStatus :: RcvFileStatus,
    senderDisplayName :: ContactName,
    chunkSize :: Integer
  }
  deriving (Eq, Show)

data RcvFileStatus
  = RFSNew
  | RFSAccepted RcvFileInfo
  | RFSConnected RcvFileInfo
  | RFSComplete RcvFileInfo
  | RFSCancelled RcvFileInfo
  deriving (Eq, Show)

data RcvFileInfo = RcvFileInfo
  { filePath :: FilePath,
    connId :: Int64,
    agentConnId :: ConnId
  }
  deriving (Eq, Show)

data FileTransfer = FTSnd [SndFileTransfer] | FTRcv RcvFileTransfer

data FileStatus = FSNew | FSAccepted | FSConnected | FSComplete | FSCancelled deriving (Eq, Ord, Show)

instance FromField FileStatus where fromField = fromTextField_ fileStatusT

instance ToField FileStatus where toField = toField . serializeFileStatus

fileStatusT :: Text -> Maybe FileStatus
fileStatusT = \case
  "new" -> Just FSNew
  "accepted" -> Just FSAccepted
  "connected" -> Just FSConnected
  "complete" -> Just FSComplete
  "cancelled" -> Just FSCancelled
  _ -> Nothing

serializeFileStatus :: FileStatus -> Text
serializeFileStatus = \case
  FSNew -> "new"
  FSAccepted -> "accepted"
  FSConnected -> "connected"
  FSComplete -> "complete"
  FSCancelled -> "cancelled"

data RcvChunkStatus = RcvChunkOk | RcvChunkFinal | RcvChunkDuplicate | RcvChunkError
  deriving (Eq, Show)

type ConnReqContact = ConnectionRequest 'CMContact

data Connection = Connection
  { connId :: Int64,
    agentConnId :: ConnId,
    connLevel :: Int,
    viaContact :: Maybe Int64,
    connType :: ConnType,
    connStatus :: ConnStatus,
    entityId :: Maybe Int64, -- contact, group member, file ID or user contact ID
    createdAt :: UTCTime
  }
  deriving (Eq, Show)

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

instance FromField ConnStatus where fromField = fromTextField_ connStatusT

instance ToField ConnStatus where toField = toField . serializeConnStatus

connStatusT :: Text -> Maybe ConnStatus
connStatusT = \case
  "new" -> Just ConnNew
  "joined" -> Just ConnJoined
  "requested" -> Just ConnRequested
  "accepted" -> Just ConnAccepted
  "snd-ready" -> Just ConnSndReady
  "ready" -> Just ConnReady
  "deleted" -> Just ConnDeleted
  _ -> Nothing

serializeConnStatus :: ConnStatus -> Text
serializeConnStatus = \case
  ConnNew -> "new"
  ConnJoined -> "joined"
  ConnRequested -> "requested"
  ConnAccepted -> "accepted"
  ConnSndReady -> "snd-ready"
  ConnReady -> "ready"
  ConnDeleted -> "deleted"

data ConnType = ConnContact | ConnMember | ConnSndFile | ConnRcvFile | ConnUserContact
  deriving (Eq, Show)

instance FromField ConnType where fromField = fromTextField_ connTypeT

instance ToField ConnType where toField = toField . serializeConnType

connTypeT :: Text -> Maybe ConnType
connTypeT = \case
  "contact" -> Just ConnContact
  "member" -> Just ConnMember
  "snd_file" -> Just ConnSndFile
  "rcv_file" -> Just ConnRcvFile
  "user_contact" -> Just ConnUserContact
  _ -> Nothing

serializeConnType :: ConnType -> Text
serializeConnType = \case
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

data GroupMemberIntroStatus
  = GMIntroPending
  | GMIntroSent
  | GMIntroInvReceived
  | GMIntroInvForwarded
  | GMIntroReConnected
  | GMIntroToConnected
  | GMIntroConnected

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

data Onboarding = Onboarding
  { contactsCount :: Int,
    createdGroups :: Int,
    membersCount :: Int,
    filesSentCount :: Int,
    addressCount :: Int
  }

data NewMessage = NewMessage
  { direction :: MsgDirection,
    chatMsgEventType :: Text,
    msgBody :: MsgBody
  }

type MessageId = Int64

data MsgDirection = MDRcv | MDSnd

data SMsgDirection (d :: MsgDirection) where
  SMDRcv :: SMsgDirection 'MDRcv
  SMDSnd :: SMsgDirection 'MDSnd

instance TestEquality SMsgDirection where
  testEquality SMDRcv SMDRcv = Just Refl
  testEquality SMDSnd SMDSnd = Just Refl
  testEquality _ _ = Nothing

class MsgDirectionI (d :: MsgDirection) where
  msgDirection :: SMsgDirection d

instance MsgDirectionI 'MDRcv where msgDirection = SMDRcv

instance MsgDirectionI 'MDSnd where msgDirection = SMDSnd

instance ToField MsgDirection where toField = toField . msgDirectionInt

msgDirectionInt :: MsgDirection -> Int
msgDirectionInt = \case
  MDRcv -> 0
  MDSnd -> 1

msgDirectionIntP :: Int -> Maybe MsgDirection
msgDirectionIntP = \case
  0 -> Just MDRcv
  1 -> Just MDSnd
  _ -> Nothing

data SndMsgDelivery = SndMsgDelivery
  { connId :: Int64,
    agentMsgId :: AgentMsgId
  }

data RcvMsgDelivery = RcvMsgDelivery
  { connId :: Int64,
    agentMsgId :: AgentMsgId,
    agentMsgMeta :: MsgMeta
  }

data MsgMetaJSON = MsgMetaJSON
  { integrity :: Text,
    rcvId :: Int64,
    rcvTs :: UTCTime,
    serverId :: Text,
    serverTs :: UTCTime,
    sndId :: Int64
  }
  deriving (Eq, Show, FromJSON, Generic)

instance ToJSON MsgMetaJSON where toEncoding = J.genericToEncoding J.defaultOptions

msgMetaToJson :: MsgMeta -> MsgMetaJSON
msgMetaToJson MsgMeta {integrity, recipient = (rcvId, rcvTs), broker = (serverId, serverTs), sender = (sndId, _)} =
  MsgMetaJSON
    { integrity = (decodeLatin1 . serializeMsgIntegrity) integrity,
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
  MDSSndReceived :: MsgDeliveryStatus 'MDSnd
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
  MDSSndReceived -> "snd_received"
  MDSSndRead -> "snd_read"

msgDeliveryStatusT :: Text -> Maybe AMsgDeliveryStatus
msgDeliveryStatusT = \case
  "rcv_agent" -> Just $ AMDS SMDRcv MDSRcvAgent
  "rcv_acknowledged" -> Just $ AMDS SMDRcv MDSRcvAcknowledged
  "snd_pending" -> Just $ AMDS SMDSnd MDSSndPending
  "snd_agent" -> Just $ AMDS SMDSnd MDSSndAgent
  "snd_sent" -> Just $ AMDS SMDSnd MDSSndSent
  "snd_received" -> Just $ AMDS SMDSnd MDSSndReceived
  "snd_read" -> Just $ AMDS SMDSnd MDSSndRead
  _ -> Nothing

msgDeliveryStatusT' :: forall d. MsgDirectionI d => Text -> Maybe (MsgDeliveryStatus d)
msgDeliveryStatusT' s =
  msgDeliveryStatusT s >>= \(AMDS d st) ->
    case testEquality d (msgDirection @d) of
      Just Refl -> Just st
      _ -> Nothing
