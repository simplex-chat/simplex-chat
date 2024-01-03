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
import System.Exit (exitFailure)
import UnliftIO.Async

simplexChatCore :: ChatConfig -> ChatOpts -> (User -> ChatController -> IO ()) -> IO ()
simplexChatCore cfg@ChatConfig {confirmMigrations, testView} opts@ChatOpts {coreOptions = CoreChatOpts {dbFilePrefix, dbKey, logAgent}} chat =
  case logAgent of
    Just level -> do
      setLogLevel level
      withGlobalLogging logCfg initRun
    _ -> initRun
  where
    initRun = createChatDatabase dbFilePrefix dbKey False confirmMigrations >>= either exit run
    exit e = do
      putStrLn $ "Error opening database: " <> show e
      exitFailure
    run db@ChatDatabase {chatStore} = do
      u <- getCreateActiveUser chatStore testView
      cc <- newChatController db (Just u) cfg opts False
      runSimplexChat opts u cc chat

runSimplexChat :: ChatOpts -> User -> ChatController -> (User -> ChatController -> IO ()) -> IO ()
runSimplexChat ChatOpts {maintenance} u cc chat
  | maintenance = wait =<< async (chat u cc)
  | otherwise = do
      a1 <- runReaderT (startChatController True True True) cc
      a2 <- async $ chat u cc
      waitEither_ a1 a2

sendChatCmdStr :: ChatController -> String -> IO ChatResponse
sendChatCmdStr cc s = runReaderT (execChatCommand Nothing . encodeUtf8 $ T.pack s) cc

sendChatCmd :: ChatController -> ChatCommand -> IO ChatResponse
sendChatCmd cc cmd = runReaderT (execChatCommand' cmd) cc
