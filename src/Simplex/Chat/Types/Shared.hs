{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Types.Shared where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.TH as JQ
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import Simplex.Chat.Options.DB (FromField (..), ToField (..))
import Simplex.Messaging.Agent.Store.DB (fromTextField_)
import Simplex.Messaging.Encoding
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, sumTypeJSON)
import Simplex.Messaging.Util ((<$?>))

data GroupMemberRole
  = GRUnknown Text -- unknown role from a newer client
  | GRRelay -- chat relay: forwards messages, can't send its own messages
  | GRObserver -- connects to all group members and receives all messages, can't send messages
  | GRAuthor -- reserved, unused
  | GRMember -- + can send messages to all group members
  | GRModerator -- + moderate messages and block members (excl. Admins and Owners)
  | GRAdmin -- + add/remove members, change member role (excl. Owners)
  | GROwner -- + delete and change group information, add/remove/change roles for Owners
  deriving (Eq, Show, Ord)

instance FromField GroupMemberRole where fromField = fromTextField_ textDecode

instance ToField GroupMemberRole where toField = toField . textEncode

instance TextEncoding GroupMemberRole where
  textEncode = \case
    GROwner -> "owner"
    GRAdmin -> "admin"
    GRModerator -> "moderator"
    GRMember -> "member"
    GRAuthor -> "author"
    GRObserver -> "observer"
    GRRelay -> "relay"
    GRUnknown t -> t
  textDecode = Just . \case
    "owner" -> GROwner
    "admin" -> GRAdmin
    "moderator" -> GRModerator
    "member" -> GRMember
    "author" -> GRAuthor
    "observer" -> GRObserver
    "relay" -> GRRelay
    t -> GRUnknown t

instance FromJSON GroupMemberRole where
  parseJSON = textParseJSON "GroupMemberRole"

instance ToJSON GroupMemberRole where
  toJSON = textToJSON
  toEncoding = textToEncoding

-- Binary encoding for the roster blob; delegates to the canonical TextEncoding
-- (same member/moderator/admin form JSON and the DB use). GRUnknown round-trips.
instance Encoding GroupMemberRole where
  smpEncode = smpEncode . textEncode
  smpP = maybe (fail "bad GroupMemberRole") pure . textDecode =<< smpP

data GroupAcceptance = GAAccepted | GAPendingApproval | GAPendingReview  deriving (Eq, Show)

instance StrEncoding GroupAcceptance where
  strEncode = \case
    GAAccepted -> "accepted"
    GAPendingApproval -> "pending"
    GAPendingReview -> "pending_review"
  strDecode = \case
    "accepted" -> Right GAAccepted
    "pending" -> Right GAPendingApproval
    "pending_review" -> Right GAPendingReview
    r -> Left $ "bad GroupAcceptance " <> B.unpack r
  strP = strDecode <$?> A.takeByteString

instance FromJSON GroupAcceptance where
  parseJSON = strParseJSON "GroupAcceptance"

instance ToJSON GroupAcceptance where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data RelayStatus
  = RSNew -- only for owner
  | RSInvited
  | RSAccepted
  | RSAcknowledgedRoster
  | RSActive
  | RSInactive
  | RSRejected
  deriving (Eq, Show)

relayStatusText :: RelayStatus -> Text
relayStatusText = \case
  RSNew -> "new"
  RSInvited -> "invited"
  RSAccepted -> "accepted"
  RSAcknowledgedRoster -> "acknowledged_roster"
  RSActive -> "active"
  RSInactive -> "inactive"
  RSRejected -> "rejected"

instance TextEncoding RelayStatus where
  textEncode = \case
    RSNew -> "new"
    RSInvited -> "invited"
    RSAccepted -> "accepted"
    RSAcknowledgedRoster -> "acknowledged_roster"
    RSActive -> "active"
    RSInactive -> "inactive"
    RSRejected -> "rejected"
  textDecode = \case
    "new" -> Just RSNew
    "invited" -> Just RSInvited
    "accepted" -> Just RSAccepted
    "acknowledged_roster" -> Just RSAcknowledgedRoster
    "active" -> Just RSActive
    "inactive" -> Just RSInactive
    "rejected" -> Just RSRejected
    _ -> Nothing

instance FromField RelayStatus where fromField = fromTextField_ textDecode

instance ToField RelayStatus where toField = toField . textEncode

$(JQ.deriveJSON (enumJSON $ dropPrefix "RS") ''RelayStatus)


data MsgSigStatus = MSSVerified | MSSSignedNoKey
  deriving (Eq, Show)

instance TextEncoding MsgSigStatus where
  textEncode = \case
    MSSVerified -> "verified"
    MSSSignedNoKey -> "no_key"
  textDecode = \case
    "verified" -> Just MSSVerified
    "no_key" -> Just MSSSignedNoKey
    _ -> Nothing

instance ToField MsgSigStatus where toField = toField . textEncode

instance FromField MsgSigStatus where fromField = fromTextField_ textDecode

$(JQ.deriveJSON (enumJSON $ dropPrefix "MSS") ''MsgSigStatus)

data MsgVerified = MVSigned {sigStatus :: MsgSigStatus} | MVSigMissing | MVUnsigned
  deriving (Eq, Show)

instance TextEncoding MsgVerified where
  textEncode = \case
    MVSigned s -> textEncode s
    MVSigMissing -> "sig_missing"
    MVUnsigned -> "unsigned"
  textDecode = \case
    "sig_missing" -> Just MVSigMissing
    "unsigned" -> Just MVUnsigned
    s -> MVSigned <$> textDecode s

-- MVUnsigned is stored as NULL (not "unsigned") so downgraded clients, which read this column as
-- MsgSigStatus, decode it as Nothing instead of failing on unknown text. Read maps NULL to MVUnsigned.
instance ToField MsgVerified where
  toField = \case
    MVUnsigned -> toField (Nothing :: Maybe Text)
    v -> toField (textEncode v)

instance FromField MsgVerified where fromField = fromTextField_ textDecode

$(JQ.deriveToJSON (sumTypeJSON $ dropPrefix "MV") ''MsgVerified)

instance FromJSON MsgVerified where
  parseJSON = $(JQ.mkParseJSON (sumTypeJSON $ dropPrefix "MV") ''MsgVerified)
  omittedField = Just MVUnsigned

toMsgVerified :: Bool -> Maybe MsgSigStatus -> MsgVerified
toMsgVerified signRequired = \case
  Just s -> MVSigned s
  Nothing -> if signRequired then MVSigMissing else MVUnsigned
