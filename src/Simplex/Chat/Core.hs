{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Core where

import Control.Logger.Simple
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Options (ChatOpts (..))
import Simplex.Chat.Store
import Simplex.Chat.Types
import UnliftIO.Async

simplexChatCore :: ChatConfig -> ChatOpts -> Maybe (Notification -> IO ()) -> (User -> ChatController -> IO ()) -> IO ()
simplexChatCore cfg@ChatConfig {yesToMigrations} opts sendToast chat
  | logAgent opts = do
    setLogLevel LogInfo -- LogError
    withGlobalLogging logCfg initRun
  | otherwise = initRun
  where
    initRun = do
      let f = chatStoreFile $ dbFilePrefix opts
      st <- createStore f yesToMigrations
      u <- getCreateActiveUser st
      cc <- newChatController st (Just u) cfg opts sendToast
      runSimplexChat opts u cc chat

runSimplexChat :: ChatOpts -> User -> ChatController -> (User -> ChatController -> IO ()) -> IO ()
runSimplexChat ChatOpts {maintenance} u cc chat
  | maintenance = wait =<< async (chat u cc)
  | otherwise = do
    a1 <- async $ chat u cc
    a2 <- runReaderT (startChatController u True) cc
    waitEither_ a1 a2
    -- void $ waitBoth a1 a2

sendChatCmd :: ChatController -> String -> IO ChatResponse
sendChatCmd cc s = runReaderT (execChatCommand . encodeUtf8 $ T.pack s) cc
