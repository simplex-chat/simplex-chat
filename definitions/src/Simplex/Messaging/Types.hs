module Simplex.Messaging.Types where

import GHC.Generics
import Data.Aeson

data NewConnectionRequest = NewConnectionRequest
  { recipientKey :: Base64EncodedString
  } deriving (Show, Generic, ToJSON, FromJSON)

data NewConnectionResponse = NewConnectionResponse
  { recipientId :: String
  , senderId    :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data SecureConnectionRequest = SecureConnectionRequest
  { senderKey :: Base64EncodedString
  } deriving (Show, Generic, ToJSON, FromJSON)

data Message = Message
  { id   :: Base64EncodedString
  , ts   :: TimeStamp
  , msg  :: Base64EncodedString
  } deriving (Show, Generic, ToJSON, FromJSON)

data MessagesResponse = MessagesResponse
  { messages      :: [Message]
  , nextMessageId :: Maybe Base64EncodedString
  } deriving (Show, Generic, ToJSON, FromJSON)

data SendMessageRequest = SendMessageRequest
  { msg :: Base64EncodedString
  } deriving (Show, Generic, ToJSON, FromJSON)

type Base64EncodedString = String
type TimeStamp = String
