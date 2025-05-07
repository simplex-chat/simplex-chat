{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.Bot
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Options
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Types
import Simplex.Messaging.Util (tshow)
import System.Directory (getAppUserDataDirectory)
import Text.Read

main :: IO ()
main = do
  opts <- welcomeGetOpts
  simplexChatCore terminalChatConfig opts mySquaringBot

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {coreOptions} <- getChatOpts appDir "simplex_bot"
  putStrLn $ "SimpleX Chat Bot v" ++ versionNumber
  printDbOpts coreOptions
  pure opts

welcomeMessage :: Text
welcomeMessage = "Hello! I am a simple squaring bot.\nIf you send me a number, I will calculate its square"

mySquaringBot :: User -> ChatController -> IO ()
mySquaringBot _user cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, evt) <- atomically . readTBQueue $ outputQ cc
    case evt of
      Right (CEvtContactConnected _ contact _) -> do
        contactConnected contact
        sendMessage cc contact welcomeMessage
      Right CEvtNewChatItems {chatItems = (AChatItem _ SMDRcv (DirectChat contact) ChatItem {content = mc@CIRcvMsgContent {}}) : _} -> do
        let msg = ciContentToText mc
            number_ = readMaybe (T.unpack msg) :: Maybe Integer
        sendMessage cc contact $ case number_ of
          Just n -> msg <> " * " <> msg <> " = " <> tshow (n * n)
          _ -> "\"" <> msg <> "\" is not a number"
      _ -> pure ()
  where
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " connected"
