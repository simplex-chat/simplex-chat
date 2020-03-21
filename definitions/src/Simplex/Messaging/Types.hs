module Simplex.Messaging.Types where

import GHC.Generics
import Data.Aeson

data NewConnectionReqBody = NewConnectionReqBody
  { recipientKey :: Base64EncodedString
  } deriving (Show, Generic, ToJSON, FromJSON)

data NewConnectionResBody = NewConnectionResBody
  { recipientId :: String
  , senderId    :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data SecureConnectionReqBody = SecureConnectionReqBody
  { senderKey :: Base64EncodedString
  } deriving (Show, Generic, ToJSON, FromJSON)

data Message = Message
  { id   :: Base64EncodedString
  , ts   :: TimeStamp
  , msg  :: Base64EncodedString
  } deriving (Show, Generic, ToJSON, FromJSON)

data MessagesResBody = MessagesResBody
  { messages      :: [Message]
  , nextMessageId :: Maybe Base64EncodedString
  } deriving (Show, Generic, ToJSON, FromJSON)

data SendMessageReqBody = SendMessageReqBody
  { msg :: Base64EncodedString
  } deriving (Show, Generic, ToJSON, FromJSON)

type Base64EncodedString = String
type TimeStamp = String
