{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Types where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import Database.SQLite.Simple (ResultError (..), SQLData (..))
import Database.SQLite.Simple.FromField (FieldParser, FromField (..), returnError)
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics
import Simplex.Messaging.Agent.Protocol (ConnId, SMPQueueInfo)
import Simplex.Messaging.Agent.Store.SQLite (fromTextField_)

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
    activeConn :: Connection
  }
  deriving (Eq, Show)

contactConnId :: Contact -> ConnId
contactConnId Contact {activeConn = Connection {agentConnId}} = agentConnId

type ContactName = Text

type GroupName = Text

data Group = Group
  { groupId :: Int64,
    localDisplayName :: GroupName,
    groupProfile :: GroupProfile,
    members :: [(GroupMember, Maybe Connection)],
    membership :: GroupMember
  }
  deriving (Eq, Show)

data Profile = Profile
  { displayName :: ContactName,
    fullName :: Text
  }
  deriving (Generic, Eq, Show)

instance ToJSON Profile where toEncoding = J.genericToEncoding J.defaultOptions

instance FromJSON Profile

data GroupProfile = GroupProfile
  { displayName :: GroupName,
    fullName :: Text
  }
  deriving (Generic, Eq, Show)

instance ToJSON GroupProfile where toEncoding = J.genericToEncoding J.defaultOptions

instance FromJSON GroupProfile

data GroupInvitation = GroupInvitation
  { fromMember :: (MemberId, GroupMemberRole),
    invitedMember :: (MemberId, GroupMemberRole),
    queueInfo :: SMPQueueInfo,
    groupProfile :: GroupProfile
  }
  deriving (Eq, Show)

data IntroInvitation = IntroInvitation
  { groupQueue :: SMPQueueInfo,
    directQueue :: SMPQueueInfo
  }
  deriving (Eq, Show)

data MemberInfo = MemberInfo MemberId GroupMemberRole Profile
  deriving (Eq, Show)

memberInfo :: GroupMember -> MemberInfo
memberInfo m = MemberInfo (memberId m) (memberRole m) (memberProfile m)

data ReceivedGroupInvitation = ReceivedGroupInvitation
  { fromMember :: GroupMember,
    invitedMember :: GroupMember,
    queueInfo :: SMPQueueInfo,
    groupProfile :: GroupProfile
  }
  deriving (Eq, Show)

data GroupMember = GroupMember
  { groupMemberId :: Int64,
    memberId :: MemberId,
    memberRole :: GroupMemberRole,
    memberCategory :: GroupMemberCategory,
    memberStatus :: GroupMemberStatus,
    invitedBy :: InvitedBy,
    localDisplayName :: ContactName,
    memberProfile :: Profile,
    memberContactId :: Maybe Int64
  }
  deriving (Eq, Show)

type MemberId = ByteString

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

instance FromField GroupMemberRole where fromField = fromBlobField_ toMemberRole

instance ToField GroupMemberRole where toField = toField . serializeMemberRole

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

fromBlobField_ :: Typeable k => (ByteString -> Either String k) -> FieldParser k
fromBlobField_ p = \case
  f@(Field (SQLBlob b) _) ->
    case p b of
      Right k -> Ok k
      Left e -> returnError ConversionFailed f ("couldn't parse field: " ++ e)
  f -> returnError ConversionFailed f "expecting SQLBlob column type"

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
  | GSMemIntroduced -- user received x.grp.mem.intro for this member (only with GCPreMember)
  | GSMemInvited -- member sent or received invitation
  | GSMemAccepted -- member accepted invitation (only User and Invitee)
  | GSMemConnected -- member created the group connection with the inviting member
  | GSMemAnnounced -- host announced (x.grp.mem.new) a member (User, Invitee and PostMember) to the group - at this point this member can send messages and invite other members (if they have sufficient permissions)
  | GSMemComplete -- host confirmed (x.grp.mem.all) that a member (User, Invitee and PostMember) created group connections with all previous members
  | GSMemCreator -- user member that created the group (only GCUserMember)
  deriving (Eq, Show, Ord)

instance FromField GroupMemberStatus where fromField = fromTextField_ memberStatusT

instance ToField GroupMemberStatus where toField = toField . serializeMemberStatus

memberActive :: GroupMember -> Bool
memberActive m = case memberStatus m of
  GSMemRemoved -> False
  GSMemLeft -> False
  GSMemIntroduced -> False
  GSMemInvited -> False
  GSMemAccepted -> False
  GSMemConnected -> True
  GSMemAnnounced -> True
  GSMemComplete -> True
  GSMemCreator -> True

memberStatusT :: Text -> Maybe GroupMemberStatus
memberStatusT = \case
  "creator" -> Just GSMemCreator
  "removed" -> Just GSMemRemoved
  "left" -> Just GSMemLeft
  "introduced" -> Just GSMemIntroduced
  "invited" -> Just GSMemInvited
  "accepted" -> Just GSMemAccepted
  "connected" -> Just GSMemConnected
  "announced" -> Just GSMemAnnounced
  "complete" -> Just GSMemComplete
  _ -> Nothing

serializeMemberStatus :: GroupMemberStatus -> Text
serializeMemberStatus = \case
  GSMemCreator -> "creator"
  GSMemRemoved -> "removed"
  GSMemLeft -> "left"
  GSMemIntroduced -> "introduced"
  GSMemInvited -> "invited"
  GSMemAccepted -> "accepted"
  GSMemConnected -> "connected"
  GSMemAnnounced -> "announced"
  GSMemComplete -> "complete"

data Connection = Connection
  { connId :: Int64,
    agentConnId :: ConnId,
    connLevel :: Int,
    viaContact :: Maybe Int64,
    connType :: ConnType,
    connStatus :: ConnStatus,
    entityId :: Maybe Int64, -- contact or group member ID
    createdAt :: UTCTime
  }
  deriving (Eq, Show)

data ConnStatus = ConnNew | ConnConfirmed | ConnAccepted | ConnReady
  deriving (Eq, Show)

instance FromField ConnStatus where fromField = fromTextField_ connStatusT

instance ToField ConnStatus where toField = toField . serializeConnStatus

connStatusT :: Text -> Maybe ConnStatus
connStatusT = \case
  "new" -> Just ConnNew
  "confirmed" -> Just ConnConfirmed
  "accepted" -> Just ConnAccepted
  "ready" -> Just ConnReady
  _ -> Nothing

serializeConnStatus :: ConnStatus -> Text
serializeConnStatus = \case
  ConnNew -> "new"
  ConnConfirmed -> "confirmed"
  ConnAccepted -> "accepted"
  ConnReady -> "ready"

data ConnType = ConnContact | ConnMember
  deriving (Eq, Show)

instance FromField ConnType where fromField = fromTextField_ connTypeT

instance ToField ConnType where toField = toField . serializeConnType

connTypeT :: Text -> Maybe ConnType
connTypeT = \case
  "contact" -> Just ConnContact
  "member" -> Just ConnMember
  _ -> Nothing

serializeConnType :: ConnType -> Text
serializeConnType = \case
  ConnContact -> "contact"
  ConnMember -> "member"

data NewConnection = NewConnection
  { agentConnId :: ByteString,
    connLevel :: Int,
    viaConn :: Maybe Int64
  }
