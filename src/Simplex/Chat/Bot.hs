{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Bot where

import Control.Logger.Simple
import Control.Monad.Reader
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Options
import Simplex.Chat.Store
import Simplex.Chat.Types (User)
import UnliftIO (async, waitEither_)

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
