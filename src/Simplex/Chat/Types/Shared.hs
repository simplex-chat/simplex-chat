{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Types.Shared where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Simplex.Chat.Options.DB (FromField (..), ToField (..))
import Simplex.Messaging.Agent.Store.DB (fromTextField_)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util ((<$?>))

data GroupMemberRole
  = GRObserver -- connects to all group members and receives all messages, can't send messages
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
  textDecode = \case
    "owner" -> Just GROwner
    "admin" -> Just GRAdmin
    "moderator" -> Just GRModerator
    "member" -> Just GRMember
    "author" -> Just GRAuthor
    "observer" -> Just GRObserver
    r -> Nothing

instance FromJSON GroupMemberRole where
  parseJSON = textParseJSON "GroupMemberRole"

instance ToJSON GroupMemberRole where
  toJSON = textToJSON
  toEncoding = textToEncoding

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
