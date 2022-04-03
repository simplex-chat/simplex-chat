{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Bot where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Logger.Simple
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Messages
import Simplex.Chat.Options (ChatOpts (..))
import Simplex.Chat.Store
import Simplex.Chat.Types (Contact (..), User (..))
import Simplex.Messaging.Encoding.String (strEncode)
import System.Exit (exitFailure)

simplexChatBot :: ChatConfig -> ChatOpts -> (User -> ChatController -> IO ()) -> IO ()
simplexChatBot cfg@ChatConfig {dbPoolSize, yesToMigrations} opts chatBot
  | logAgent opts = do
    setLogLevel LogInfo -- LogError
    withGlobalLogging logCfg initRun
  | otherwise = initRun
  where
    initRun = do
      let f = chatStoreFile $ dbFilePrefix opts
      st <- createStore f dbPoolSize yesToMigrations
      u <- getCreateActiveUser st
      cc <- newChatController st (Just u) cfg opts (const $ pure ())
      runSimplexChatBot u cc chatBot

runSimplexChatBot :: User -> ChatController -> (User -> ChatController -> IO ()) -> IO ()
runSimplexChatBot u cc chatBot = do
  a1 <- async $ chatBot u cc
  a2 <- runReaderT (startChatController u) cc
  waitEither_ a1 a2

chatBotRepl :: String -> (String -> String) -> User -> ChatController -> IO ()
chatBotRepl welcome answer _user cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, resp) <- atomically . readTBQueue $ outputQ cc
    case resp of
      CRContactConnected contact -> do
        contactConnected contact
        void $ sendMsg contact welcome
      CRNewChatItem (AChatItem _ SMDRcv (DirectChat contact) ChatItem {content}) -> do
        let msg = T.unpack $ ciContentToText content
        void . sendMsg contact $ answer msg
      _ -> pure ()
  where
    sendMsg Contact {contactId} msg = sendCmd cc $ "/_send @" <> show contactId <> " text " <> msg
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " connected"

initializeBotAddress :: ChatController -> IO ()
initializeBotAddress cc = do
  sendCmd cc "/show_address" >>= \case
    CRUserContactLink uri _ -> showBotAddress uri
    CRChatCmdError (ChatErrorStore SEUserContactLinkNotFound) -> do
      putStrLn $ "No bot address, creating..."
      sendCmd cc "/address" >>= \case
        CRUserContactLinkCreated uri -> showBotAddress uri
        _ -> putStrLn "can't create bot address" >> exitFailure
    _ -> putStrLn "unexpected response" >> exitFailure
  where
    showBotAddress uri = do
      putStrLn $ "Bot's contact address is: " <> B.unpack (strEncode uri)
      void $ sendCmd cc "/auto_accept on"

sendCmd :: ChatController -> String -> IO ChatResponse
sendCmd cc s = runReaderT (execChatCommand . encodeUtf8 $ T.pack s) cc
