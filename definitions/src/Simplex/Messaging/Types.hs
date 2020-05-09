{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Simplex.Messaging.Types where

import ClassyPrelude
import Data.Aeson
import GHC.Generics()

newtype CreateConnRequest = CreateConnRequest
  { recipientKey :: Key
  } deriving (Show, Generic, ToJSON, FromJSON)

data CreateConnResponse = CreateConnResponse
  { recipientId :: String
  , senderId    :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype SecureConnRequest = SecureConnRequest
  { senderKey :: Key
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

newtype SendMessageRequest = SendMessageRequest
  { msg :: Base64EncodedString
  } deriving (Show, Generic, ToJSON, FromJSON)

type Key = Base64EncodedString
type Base64EncodedString = Text
type TimeStamp = Text
