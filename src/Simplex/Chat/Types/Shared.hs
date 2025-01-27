{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Types.Shared where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.TH as JQ
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.Text (Text)
import Simplex.Chat.Options.DB (FromField (..), ToField (..))
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (blobFieldDecoder, defaultJSON)
import Simplex.Messaging.Util ((<$?>))

data GroupMemberRole
  = GRObserver -- connects to all group members and receives all messages, can't send messages
  | GRAuthor -- reserved, unused
  | GRMember -- + can send messages to all group members
  | GRModerator -- + moderate messages and block members (excl. Admins and Owners)
  | GRAdmin -- + add/remove members, change member role (excl. Owners)
  | GROwner -- + delete and change group information, add/remove/change roles for Owners
  deriving (Eq, Show, Ord)

instance FromField GroupMemberRole where fromField = blobFieldDecoder strDecode

instance ToField GroupMemberRole where toField = toField . strEncode

instance StrEncoding GroupMemberRole where
  strEncode = \case
    GROwner -> "owner"
    GRAdmin -> "admin"
    GRModerator -> "moderator"
    GRMember -> "member"
    GRAuthor -> "author"
    GRObserver -> "observer"
  strDecode = \case
    "owner" -> Right GROwner
    "admin" -> Right GRAdmin
    "moderator" -> Right GRModerator
    "member" -> Right GRMember
    "author" -> Right GRAuthor
    "observer" -> Right GRObserver
    r -> Left $ "bad GroupMemberRole " <> B.unpack r
  strP = strDecode <$?> A.takeByteString

instance FromJSON GroupMemberRole where
  parseJSON = strParseJSON "GroupMemberRole"

instance ToJSON GroupMemberRole where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data GroupMemberMention = GroupMemberMention {memberName :: Text, groupMemberId :: Int64}
  deriving (Show)

$(JQ.deriveJSON defaultJSON ''GroupMemberMention)
