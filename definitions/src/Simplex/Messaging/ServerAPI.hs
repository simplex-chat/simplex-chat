{-# LANGUAGE OverloadedStrings  #-}

module Simplex.Messaging.ServerAPI
  ( ServerAPI
  , serverApiIntro
  , serverApiExtra
  ) where

import Simplex.Messaging.Types as T
import Control.Lens
import Data.Function()
import Servant
import Servant.Docs

type ServerAPI =
       CreateConnection
  :<|> SecureConnection
  :<|> DeleteConnection
  :<|> GetMessages
  :<|> DeleteMessage
  :<|> SendMessage

type CreateConnection = "connection" :> ReqBody '[JSON] NewConnectionReqBody
                                     :> PostCreated '[JSON] NewConnectionResBody

type SecureConnection = "connection" :> Capture "connectionId" Base64EncodedString
                                     :> ReqBody '[JSON] SecureConnectionReqBody
                                     :> Put '[JSON] NoContent

type DeleteConnection = "connection" :> Capture "connectionId" Base64EncodedString
                                     :> Delete '[JSON] NoContent

type GetMessages      = "connection" :> Capture "connectionId" Base64EncodedString :>
                        "messages"   :> QueryParam "fromMessageId" (Maybe Base64EncodedString)
                                     :> Get '[JSON] MessagesResBody

type DeleteMessage    = "connection" :> Capture "connectionId" Base64EncodedString :>
                        "messages"   :> Capture "messageId" Base64EncodedString
                                     :> Delete '[JSON] NoContent

type SendMessage      = "connection" :> Capture "senderConnectionId" Base64EncodedString :>
                        "messages"   :> ReqBody '[JSON] SendMessageReqBody
                                     :> PostCreated '[JSON] NoContent

-- API docs
serverApiIntro :: DocIntro
serverApiIntro = DocIntro "Simplex messaging protocol REST API"
  [ "This document lists all required REST endpoints of simplex messaging API."
  , "Also see [Simplex messaging protocol implementation](simplex-messaging-implementation.md) for more details."
  ]

serverApiExtra :: ExtraInfo ServerAPI
serverApiExtra =
  info (Proxy :: Proxy CreateConnection)
    "Create connection"
    []
  <>
  info (Proxy :: Proxy SecureConnection)
    "Secure connection"
    []
  <>
  info (Proxy :: Proxy DeleteConnection)
    "Delete connection"
    []
  <>
  info (Proxy :: Proxy GetMessages)
    "Get messages"
    []
  <>
  info (Proxy :: Proxy DeleteMessage)
    "Delete message"
    []
  <>
  info (Proxy :: Proxy SendMessage)
    "Send message"
    []

info p title comments =
  extraInfo p $ defAction & notes <>~ [ DocNote title comments ]

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

instance ToSample (NewConnectionReqBody) where
  toSamples _ = singleSample $ NewConnectionReqBody "BODbZxmtKUUF1l8pj4nVjQ"

instance ToSample (NewConnectionResBody) where
  toSamples _ = singleSample $ NewConnectionResBody "Qxz93A" "N9pA3g"

instance ToSample (SecureConnectionReqBody) where
  toSamples _ = singleSample $ SecureConnectionReqBody "XPaVEVNunkYKqqK0dnAT5Q"

dummyMessage :: Message
dummyMessage = Message
                { T.id = "p8PCiGPZ"
                , ts = "2020-03-15T19:58:33.695Z"
                , msg = "OQLMXoEA4iv-aR46puPJuY1Rdoc1KY0gfq8oElJwtAs"
                }

instance ToSample (MessagesResBody) where
  toSamples _ = singleSample $ MessagesResBody
                                { messages = [dummyMessage]
                                , nextMessageId = Nothing
                                }

instance ToSample (SendMessageReqBody) where
  toSamples _ = singleSample $ SendMessageReqBody
                                { msg = "OQLMXoEA4iv-aR46puPJuY1Rdoc1KY0gfq8oElJwtAs"
                                }
