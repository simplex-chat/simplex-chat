{-# LANGUAGE DuplicateRecordFields #-}

module Simplex.Chat.Types where

import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)

data User = User
  { userId :: Int64,
    profile :: Profile
  }

newtype Contact = Contact {fromContact :: ByteString} deriving (Eq, Show)

data Contact' = Contact'
  { contactId :: Int64,
    localContactRef :: Text,
    profile :: Profile,
    activeConn :: Connection,
    connections :: [Connection]
  }
  deriving (Eq, Show)

data Group = Group
  { groupId :: Int64,
    localGroupRef :: Text
  }
  deriving (Eq, Show)

data Profile = Profile
  { contactRef :: Text,
    displayName :: Text
  }
  deriving (Eq, Show)

data Connection = Connection
  { connId :: Int64,
    agentConnId :: ByteString,
    connLevel :: Int,
    viaConn :: Maybe Int64,
    connStatus :: ConnStatus
  }
  deriving (Eq, Show)

data ConnStatus = ConnNew | ConnConfirmed | ConnAccepted | ConnReady
  deriving (Eq, Show)
