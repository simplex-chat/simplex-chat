{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Types.Shared where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Simplex.Chat.Options.DB (FromField (..), ToField (..))
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (blobFieldDecoder)
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

data GroupAcceptance = GAAuto | GAManual deriving (Eq, Show)

instance FromField GroupAcceptance where fromField = blobFieldDecoder strDecode

instance ToField GroupAcceptance where toField = toField . strEncode

instance StrEncoding GroupAcceptance where
  strEncode = \case
    GAAuto -> "auto"
    GAManual -> "manual"
  strDecode = \case
    "auto" -> Right GAAuto
    "manual" -> Right GAManual
    r -> Left $ "bad GroupAcceptance " <> B.unpack r
  strP = strDecode <$?> A.takeByteString

instance FromJSON GroupAcceptance where
  parseJSON = strParseJSON "GroupAcceptance"

instance ToJSON GroupAcceptance where
  toJSON = strToJSON
  toEncoding = strToJEncoding
