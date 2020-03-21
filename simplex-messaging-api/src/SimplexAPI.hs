{-# LANGUAGE OverloadedStrings  #-}

module SimplexAPI
  ( SimplexAPI
  , simplexApiIntro
  , simplexApiExtra
  ) where

import GHC.Generics
import Control.Lens
import Data.Aeson
import Data.Function()
import Servant
import Servant.Docs

type SimplexAPI =
       CreateConnection
  :<|> SecureConnection
  :<|> DeleteConnection
  :<|> GetMessages
  :<|> DeleteMessage
  :<|> SendMessage

type CreateConnection = "connection" :> ReqBody '[JSON] NewConnectionRequest
                                     :> PostCreated '[JSON] NewConnectionResponse

type SecureConnection = "connection" :> Capture "connectionId" Base64EncodedString
                                     :> ReqBody '[JSON] SecureConnectionRequest
                                     :> Put '[JSON] NoContent

type DeleteConnection = "connection" :> Capture "connectionId" Base64EncodedString
                                     :> Delete '[JSON] NoContent

type GetMessages      = "connection" :> Capture "connectionId" Base64EncodedString :>
                        "messages"   :> QueryParam "fromMessageId" (Maybe Base64EncodedString)
                                     :> Get '[JSON] MessagesResponse

type DeleteMessage    = "connection" :> Capture "connectionId" Base64EncodedString :>
                        "messages"   :> Capture "messageId" Base64EncodedString
                                     :> Delete '[JSON] NoContent

type SendMessage      = "connection" :> Capture "senderConnectionId" Base64EncodedString :>
                        "messages"   :> ReqBody '[JSON] SendMessageRequest
                                     :> PostCreated '[JSON] NoContent

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


-- API docs
simplexApiIntro :: DocIntro
simplexApiIntro = DocIntro "Simplex messaging protocol REST API"
  [ "This document lists all required REST endpoints of simplex messaging API."
  , "Also see [Simplex messaging protocol implementation](simplex-messaging-implementation.md) for more details."
  ]

simplexApiExtra :: ExtraInfo SimplexAPI
simplexApiExtra =
  endpointInfo (Proxy :: Proxy CreateConnection)
    "Create connection"
    []
  <>
  endpointInfo (Proxy :: Proxy SecureConnection)
    "Secure connection"
    []
  <>
  endpointInfo (Proxy :: Proxy DeleteConnection)
    "Delete connection"
    []
  <>
  endpointInfo (Proxy :: Proxy GetMessages)
    "Get messages"
    []
  <>
  endpointInfo (Proxy :: Proxy DeleteMessage)
    "Delete message"
    []
  <>
  endpointInfo (Proxy :: Proxy SendMessage)
    "Send message"
    []

endpointInfo :: (IsIn endpoint SimplexAPI, HasLink endpoint, HasDocs endpoint)
             => Proxy endpoint -> String -> [String] -> ExtraInfo SimplexAPI
endpointInfo p title comments =
  extraInfo p (defAction & notes <>~ [ DocNote title comments ])

instance ToCapture (Capture "connectionId" String) where
  toCapture _ =
    DocCapture "connectionId"
               "Recipient connection ID - unique connection ID to be used by connection recipient"

instance ToCapture (Capture "senderConnectionId" String) where
  toCapture _ =
    DocCapture "senderConnectionId"
               "Sender connection ID - unique connection ID to be used by connection sender"

instance ToCapture (Capture "messageId" String) where
  toCapture _ =
    DocCapture "messageId"
               "Message ID - unique message ID to be used by connection recipient"

instance ToParam (QueryParam "fromMessageId" (Maybe Base64EncodedString)) where
  toParam _ =
    DocQueryParam "fromMessageId"
                  ["message ID, e.g., `p8PCiGPZ`"]
                  "if set, the server will respond with the messages received starting from the message with server message ID (unique per server) passed in this parameter."
                  Normal

instance ToSample (NewConnectionRequest) where
  toSamples _ = singleSample $ NewConnectionRequest "BODbZxmtKUUF1l8pj4nVjQ"

instance ToSample (NewConnectionResponse) where
  toSamples _ = singleSample $ NewConnectionResponse "Qxz93A" "N9pA3g"

instance ToSample (SecureConnectionRequest) where
  toSamples _ = singleSample $ SecureConnectionRequest "XPaVEVNunkYKqqK0dnAT5Q"

dummyMessage :: Message
dummyMessage = Message
                { SimplexAPI.id = "p8PCiGPZ"
                , ts = "2020-03-15T19:58:33.695Z"
                , msg = "OQLMXoEA4iv-aR46puPJuY1Rdoc1KY0gfq8oElJwtAs"
                }

instance ToSample (MessagesResponse) where
  toSamples _ = singleSample $ MessagesResponse
                                { messages = [dummyMessage]
                                , nextMessageId = Nothing
                                }

instance ToSample (SendMessageRequest) where
  toSamples _ = singleSample $ SendMessageRequest
                                { msg = "OQLMXoEA4iv-aR46puPJuY1Rdoc1KY0gfq8oElJwtAs"
                                }
