{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Server where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.Socket
import qualified Network.WebSockets as WS
import Numeric.Natural (Natural)
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Library.Commands
import Simplex.Chat.Options
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.Transport.Server (runLocalTCPServer)
import Simplex.Messaging.Util (raceAny_)
import UnliftIO.Exception
import UnliftIO.STM

data ChatSrvRequest = ChatSrvRequest {corrId :: Text, cmd :: Text}
  deriving (Generic, FromJSON)

data ChatSrvResponse r = ChatSrvResponse {corrId :: Maybe Text, resp :: r}

data AChatSrvResponse = forall r. ToJSON (ChatSrvResponse r) => ACR (ChatSrvResponse r)

$(pure [])

instance ToJSON r => ToJSON (ChatSrvResponse r) where
  toEncoding = $(JQ.mkToEncoding defaultJSON ''ChatSrvResponse)
  toJSON = $(JQ.mkToJSON defaultJSON ''ChatSrvResponse)

simplexChatServer :: ServiceName -> ChatConfig -> ChatOpts -> IO ()
simplexChatServer chatPort cfg opts =
  simplexChatCore cfg opts . const $ runChatServer defaultChatServerConfig {chatPort}

data ChatServerConfig = ChatServerConfig
  { chatPort :: ServiceName,
    clientQSize :: Natural
  }

defaultChatServerConfig :: ChatServerConfig
defaultChatServerConfig =
  ChatServerConfig
    { chatPort = "5225",
      clientQSize = 1
    }

data ChatClient = ChatClient
  { rcvQ :: TBQueue (Text, ChatCommand),
    sndQ :: TBQueue AChatSrvResponse
  }

newChatServerClient :: Natural -> STM ChatClient
newChatServerClient qSize = do
  rcvQ <- newTBQueue qSize
  sndQ <- newTBQueue qSize
  pure ChatClient {rcvQ, sndQ}

runChatServer :: ChatServerConfig -> ChatController -> IO ()
runChatServer ChatServerConfig {chatPort, clientQSize} cc = do
  started <- newEmptyTMVarIO
  runLocalTCPServer started chatPort $ \sock -> do
    ws <- liftIO $ getConnection sock
    c <- atomically $ newChatServerClient clientQSize
    putStrLn "client connected"
    raceAny_ [send ws c, client c, output c, receive ws c]
      `finally` clientDisconnected c
  where
    getConnection sock = WS.makePendingConnection sock WS.defaultConnectionOptions >>= WS.acceptRequest
    send ws ChatClient {sndQ} =
      forever $
        atomically (readTBQueue sndQ) >>= \(ACR r) -> WS.sendTextData ws (J.encode r)
    client ChatClient {rcvQ, sndQ} = forever $ do
      atomically (readTBQueue rcvQ)
        >>= processCommand
        >>= atomically . writeTBQueue sndQ . ACR
    output ChatClient {sndQ} = forever $ do
      (_, resp) <- atomically . readTBQueue $ outputQ cc
      atomically $ writeTBQueue sndQ $ ACR ChatSrvResponse {corrId = Nothing, resp}
    receive ws ChatClient {rcvQ, sndQ} = forever $ do
      s <- WS.receiveData ws
      case J.decodeStrict' s of
        Just ChatSrvRequest {corrId, cmd} -> do
          putStrLn $ "received command " <> show corrId <> " : " <> show cmd
          case parseChatCommand $ encodeUtf8 cmd of
            Right command -> atomically $ writeTBQueue rcvQ (corrId, command)
            Left e -> sendError (Just corrId) e
        Nothing -> sendError Nothing "invalid request"
      where
        sendError corrId e = atomically $ writeTBQueue sndQ $ ACR ChatSrvResponse {corrId, resp = chatCmdError Nothing e}
    processCommand (corrId, cmd) =
      runReaderT (runExceptT $ processChatCommand cmd) cc >>= \case
        Right resp -> response resp
        Left e -> response $ CRChatCmdError Nothing e
      where
        response resp = pure ChatSrvResponse {corrId = Just corrId, resp}
    clientDisconnected _ = pure ()
