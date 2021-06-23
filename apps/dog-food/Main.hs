{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ChatOptions
import ChatTerminal
import ChatTerminal.Core
import Control.Concurrent.STM
import Control.Logger.Simple
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock (DiffTime, UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
import Numeric.Natural
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Markdown
import Simplex.Chat.Styled
import Simplex.Messaging.Agent (getAgentClient, getSMPAgentClient, runAgentClient)
import Simplex.Messaging.Agent.Client (AgentClient (..))
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Client (smpDefaultConfig)
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (raceAny_)
import Simplex.View
import System.Console.ANSI.Types
import System.Directory (getAppUserDataDirectory)
import Types

cfg :: AgentConfig
cfg =
  AgentConfig
    { tcpPort = undefined, -- agent does not listen to TCP
      smpServers = undefined, -- filled in from options
      rsaKeySize = 2048 `div` 8,
      connIdBytes = 12,
      tbqSize = 16,
      dbFile = "smp-chat.db",
      smpCfg = smpDefaultConfig
    }

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}

data ChatClient = ChatClient
  { inQ :: TBQueue ChatCommand,
    outQ :: TBQueue ChatResponse
  }

data ChatResponse
  = Connected Contact
  | Confirmation Contact
  | ReceivedMessage Contact UTCTime ByteString MsgIntegrity
  | Disconnected Contact
  | YesYes
  | ContactError ConnectionErrorType Contact
  | ChatError AgentErrorType
  | NoChatResponse

serializeChatResponse :: ChatOpts -> TimeZone -> ZonedTime -> ChatResponse -> [StyledString]
serializeChatResponse _ localTz currentTime = \case
  Connected c -> [ttyContact c <> " connected"]
  Confirmation c -> [ttyContact c <> " ok"]
  ReceivedMessage c utcTime t mi -> receivedMessage utcTime t mi $ ttyFromContact c
  Disconnected c -> ["disconnected from " <> ttyContact c <> " - restart chat"]
  YesYes -> ["you got it!"]
  ContactError e c -> case e of
    NOT_FOUND -> ["no contact " <> ttyContact c]
    DUPLICATE -> ["contact " <> ttyContact c <> " already exists"]
    SIMPLEX -> ["contact " <> ttyContact c <> " did not accept invitation yet"]
  ChatError e -> ["chat error: " <> plain (show e)]
  NoChatResponse -> [""]
  where
    receivedMessage :: UTCTime -> ByteString -> MsgIntegrity -> StyledString -> [StyledString]
    receivedMessage utcTime t mi from =
      prependFirst (formatUTCTime utcTime <> " " <> from) (msgPlain t) ++ showIntegrity mi
    prependFirst :: StyledString -> [StyledString] -> [StyledString]
    prependFirst s [] = [s]
    prependFirst s (s' : ss) = (s <> s') : ss
    formatUTCTime :: UTCTime -> StyledString
    formatUTCTime utcTime = do
      let localTime = utcToLocalTime localTz utcTime
          format =
            if (localDay localTime < localDay (zonedTimeToLocalTime currentTime))
              && (timeOfDayToTime (localTimeOfDay localTime) > (6 * 60 * 60 :: DiffTime))
              then "%m-%d" -- if message is from yesterday or before and 6 hours has passed since midnight
              else "%H:%M"
      styleTime $ formatTime defaultTimeLocale format localTime
    msgPlain :: ByteString -> [StyledString]
    msgPlain = map styleMarkdownText . T.lines . safeDecodeUtf8
    showIntegrity :: MsgIntegrity -> [StyledString]
    showIntegrity MsgOk = []
    showIntegrity (MsgError err) = msgError $ case err of
      MsgSkipped fromId toId ->
        "skipped message ID " <> show fromId
          <> if fromId == toId then "" else ".." <> show toId
      MsgBadId msgId -> "unexpected message ID " <> show msgId
      MsgBadHash -> "incorrect message hash"
      MsgDuplicate -> "duplicate message ID"
    msgError :: String -> [StyledString]
    msgError s = [styled (Colored Red) s]

main :: IO ()
main = do
  opts@ChatOpts {dbFile, smpServers} <- welcomeGetOpts
  t <- atomically $ newChatClient (tbqSize cfg)
  ct <- newChatTerminal $ tbqSize cfg
  smpAgent <- getSMPAgentClient cfg {dbFile, smpServers}
  cc <- atomically $ newChatController smpAgent ct $ tbqSize cfg
  -- setLogLevel LogInfo -- LogError
  -- withGlobalLogging logCfg $ do
  env <- newSMPAgentEnv cfg {dbFile, smpServers}
  dogFoodChat cc t ct env opts

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {dbFile} <- getChatOpts appDir
  putStrLn "SimpleX chat prototype v0.3.1"
  putStrLn $ "db: " <> dbFile
  putStrLn "type \"/help\" or \"/h\" for usage info"
  pure opts

dogFoodChat :: ChatController -> ChatClient -> ChatTerminal -> Env -> ChatOpts -> IO ()
dogFoodChat cc t ct env opts = do
  c <- runReaderT getAgentClient env
  localTz <- liftIO getCurrentTimeZone
  raceAny_
    [ runReaderT (runAgentClient c) env,
      sendToAgent t ct c,
      sendToChatTerm t ct opts localTz,
      receiveFromAgent t ct c,
      receiveFromChatTerm t ct,
      runChatTerminal cc ct,
      runReaderT runChatController cc
    ]

newChatClient :: Natural -> STM ChatClient
newChatClient qSize = do
  inQ <- newTBQueue qSize
  outQ <- newTBQueue qSize
  return ChatClient {inQ, outQ}

receiveFromChatTerm :: ChatClient -> ChatTerminal -> IO ()
receiveFromChatTerm t ct = forever $ do
  atomically (readTBQueue $ inputQ ct)
    >>= processOrError . parseAll chatCommandP . encodeUtf8 . T.pack
  where
    processOrError = \case
      Left _ -> pure ()
      Right ChatHelp -> pure ()
      Right MarkdownHelp -> pure ()
      Right cmd -> atomically $ writeTBQueue (inQ t) cmd

sendToChatTerm :: ChatClient -> ChatTerminal -> ChatOpts -> TimeZone -> IO ()
sendToChatTerm ChatClient {outQ} ChatTerminal {outputQ} opts localTz = forever $ do
  atomically (readTBQueue outQ) >>= \case
    NoChatResponse -> return ()
    resp -> do
      currentTime <- liftIO getZonedTime
      atomically . writeTBQueue outputQ $ serializeChatResponse opts localTz currentTime resp

sendToAgent :: ChatClient -> ChatTerminal -> AgentClient -> IO ()
sendToAgent ChatClient {inQ} ct AgentClient {rcvQ} =
  forever . atomically $ do
    cmd <- readTBQueue inQ
    writeTBQueue rcvQ `mapM_` agentTransmission cmd
    setActiveTo cmd
  where
    setActiveTo :: ChatCommand -> STM ()
    setActiveTo = \case
      SendMessage a _ -> setActive ct $ ActiveC a
      DeleteContact a -> unsetActive ct $ ActiveC a
      _ -> pure ()
    agentTransmission :: ChatCommand -> Maybe (ATransmission 'Client)
    agentTransmission = \case
      AddContact _ -> Nothing
      Connect a qInfo -> transmission a $ JOIN qInfo $ ReplyMode On
      DeleteContact a -> transmission a DEL
      SendMessage a msg -> transmission a $ SEND msg
      ChatHelp -> Nothing
      MarkdownHelp -> Nothing
    transmission :: Contact -> ACommand 'Client -> Maybe (ATransmission 'Client)
    transmission (Contact a) cmd = Just ("1", a, cmd)

receiveFromAgent :: ChatClient -> ChatTerminal -> AgentClient -> IO ()
receiveFromAgent t ct c = forever . atomically $ do
  resp <- chatResponse <$> readTBQueue (subQ c)
  writeTBQueue (outQ t) resp
  setActiveTo resp
  where
    chatResponse :: ATransmission 'Agent -> ChatResponse
    chatResponse (_, a, resp) = case resp of
      CON -> Connected contact
      END -> Disconnected contact
      MSG {msgBody, msgIntegrity, brokerMeta} ->
        ReceivedMessage contact (snd brokerMeta) msgBody msgIntegrity
      SENT _ -> NoChatResponse
      OK -> Confirmation contact
      ERR (CONN e) -> ContactError e contact
      ERR e -> ChatError e
      _ -> ChatError $ CMD PROHIBITED
      where
        contact = Contact a
    setActiveTo :: ChatResponse -> STM ()
    setActiveTo = \case
      Connected a -> setActive ct $ ActiveC a
      ReceivedMessage a _ _ _ -> setActive ct $ ActiveC a
      Disconnected a -> unsetActive ct $ ActiveC a
      _ -> pure ()

setActive :: ChatTerminal -> ActiveTo -> STM ()
setActive ct = writeTVar (activeTo ct)

unsetActive :: ChatTerminal -> ActiveTo -> STM ()
unsetActive ct a = modifyTVar (activeTo ct) unset
  where
    unset a' = if a == a' then ActiveNone else a'
