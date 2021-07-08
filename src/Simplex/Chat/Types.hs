{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Types where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics
import Simplex.Messaging.Agent.Protocol (ConnId)
import Simplex.Messaging.Agent.Store.SQLite (fromTextField_)

data User = User
  { userId :: UserId,
    localContactRef :: ContactRef,
    profile :: Profile,
    activeUser :: Bool
  }

type UserId = Int64

data Contact
  = Contact
      { contactId :: Int64,
        localContactRef :: ContactRef,
        profile :: Profile,
        activeConn :: Connection
      }
  | NewContact {activeConn :: Connection}
  deriving (Eq, Show)

type ContactRef = Text

type GroupRef = Text

data Group = Group
  { groupId :: Int64,
    localGroupRef :: Text
  }
  deriving (Eq, Show)

data Profile = Profile
  { contactRef :: ContactRef,
    displayName :: Text
  }
  deriving (Generic, Eq, Show)

instance ToJSON Profile where toEncoding = J.genericToEncoding J.defaultOptions

instance FromJSON Profile

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
