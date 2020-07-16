{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Simplex.Messaging.Types where

import Data.Aeson
import Data.String
import Data.Text
import GHC.Generics

newtype CreateConnRequest = CreateConnRequest
  { recipientKey :: Key
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance IsString CreateConnRequest where
  fromString = CreateConnRequest

data CreateConnResponse = CreateConnResponse
  { recipientId :: String,
    senderId :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype SecureConnRequest = SecureConnRequest
  { senderKey :: Key
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance IsString SecureConnRequest where
  fromString = SecureConnRequest

data Message = Message
  { msgId :: MessageId,
    ts :: TimeStamp,
    msg :: Encrypted -- TODO make it Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data MessagesResponse = MessagesResponse
  { messages :: [Message],
    nextMessageId :: Maybe Base64EncodedString
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype SendMessageRequest = SendMessageRequest
  { msg :: Base64EncodedString
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance IsString SendMessageRequest where
  fromString = SendMessageRequest

data Invitation = Invitation
  { connId :: ConnId,
    brokerUri :: Text,
    encryptKey :: PublicKey
  }

type Key = Base64EncodedString -- deprecated, not to be used

type PublicKey = Base64EncodedString

type PrivateKey = Base64EncodedString

type ConnId = Base64EncodedString

type SenderConnId = Base64EncodedString

type MessageId = Base64EncodedString

type Encrypted = Base64EncodedString

type Base64EncodedString = String

type TimeStamp = String
