{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Core where

import Control.Logger.Simple
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Options (ChatOpts (..), CoreChatOpts (..))
import Simplex.Chat.Types
import UnliftIO.Async

simplexChatCore :: ChatConfig -> ChatOpts -> Maybe (Notification -> IO ()) -> (User -> ChatController -> IO ()) -> IO ()
simplexChatCore cfg@ChatConfig {yesToMigrations} opts@ChatOpts {coreOptions = CoreChatOpts {dbFilePrefix, dbKey, logAgent}} sendToast chat =
  case logAgent of
    Just level -> do
      setLogLevel level
      withGlobalLogging logCfg initRun
    _ -> initRun
  where
    initRun = do
      db@ChatDatabase {chatStore} <- createChatDatabase dbFilePrefix dbKey yesToMigrations
      u <- getCreateActiveUser chatStore
      cc <- newChatController db (Just u) cfg opts sendToast
      runSimplexChat opts u cc chat

runSimplexChat :: ChatOpts -> User -> ChatController -> (User -> ChatController -> IO ()) -> IO ()
runSimplexChat ChatOpts {maintenance} u cc chat
  | maintenance = wait =<< async (chat u cc)
  | otherwise = do
    a1 <- runReaderT (startChatController True True) cc
    a2 <- async $ chat u cc
    waitEither_ a1 a2

sendChatCmd :: ChatController -> String -> IO ChatResponse
sendChatCmd cc s = runReaderT (execChatCommand . encodeUtf8 $ T.pack s) cc
