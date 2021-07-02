{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Types where

import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Simplex.Messaging.Agent.Store.SQLite (fromTextField_)

data User = User
  { userId :: UserId,
    profile :: Profile
  }

newtype Contact = Contact {fromContact :: ByteString} deriving (Eq, Show)

type UserId = Int64

data Contact' = Contact'
  { contactId :: Int64,
    localContactRef :: ContactRef,
    profile :: Maybe Profile,
    activeConn :: Connection
  }
  deriving (Eq, Show)

type ContactRef = Text

data Group = Group
  { groupId :: Int64,
    localGroupRef :: Text
  }
  deriving (Eq, Show)

data Profile = Profile
  { contactRef :: ContactRef,
    displayName :: Text
  }
  deriving (Eq, Show)

data Connection = Connection
  { connId :: Int64,
    agentConnId :: ByteString,
    connLevel :: Int,
    viaContact :: Maybe Int64,
    connStatus :: ConnStatus
  }
  deriving (Eq, Show)

data ConnStatus = ConnNew | ConnConfirmed | ConnAccepted | ConnReady
  deriving (Eq, Show)

instance FromField ConnStatus where fromField = fromTextField_ connStatusT

instance ToField ConnStatus where toField = toField . serializeConnStatus

connStatusT :: Text -> Maybe ConnStatus
connStatusT = \case
  "NEW" -> Just ConnNew
  "CONF" -> Just ConnConfirmed
  "ACPT" -> Just ConnAccepted
  "READY" -> Just ConnReady
  _ -> Nothing

serializeConnStatus :: ConnStatus -> Text
serializeConnStatus = \case
  ConnNew -> "NEW"
  ConnConfirmed -> "CONF"
  ConnAccepted -> "ACPT"
  ConnReady -> "READY"

data NewConnection = NewConnection
  { agentConnId :: ByteString,
    connLevel :: Int,
    viaConn :: Maybe Int64
  }
