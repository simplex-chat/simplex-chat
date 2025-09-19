{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Terminal.Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe (fromMaybe)
import Network.Socket
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), ChatError, ChatEvent (..), PresetServers (..), SimpleNetCfg (..), currentRemoteHost, versionNumber, versionString)
import Simplex.Chat.Core
import Simplex.Chat.Options
import Simplex.Chat.Options.DB
import Simplex.Chat.Terminal
import Simplex.Chat.View (ChatResponseEvent, smpProxyModeStr)
import Simplex.Messaging.Client (NetworkConfig (..), SocksMode (..))
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)
import System.Terminal (withTerminal)

#if defined(picolisp)
import Simplex.Chat.Picolisp
#endif

simplexChatCLI :: ChatConfig -> Maybe (ServiceName -> ChatConfig -> ChatOpts -> IO ()) -> IO ()
simplexChatCLI cfg server_ = do
  appDir <- getAppUserDataDirectory "simplex"
  opts <- getChatOpts appDir "simplex_v1"
  simplexChatCLI' cfg opts server_

simplexChatCLI' :: ChatConfig -> ChatOpts -> Maybe (ServiceName -> ChatConfig -> ChatOpts -> IO ()) -> IO ()
simplexChatCLI' cfg opts@ChatOpts {chatCmd, chatCmdLog, chatCmdDelay, chatServerPort} server_
  | null chatCmd = case chatServerPort of
      Just chatPort -> case server_ of
        Just server -> server chatPort cfg opts
        Nothing -> putStrLn "Not allowed to run as a WebSockets server" >> exitFailure
#if defined(picolisp)
      Nothing -> case evaluatePicolisp opts of
        Just code -> do
          picolispInit 100000 ["picolisp"] -- ["picolisp", "lib.l"]
          res <- picolispEvaluate code
          putStrLn res
        Nothing -> runCLI
#else
      Nothing -> runCLI
#endif
  | otherwise = simplexChatCore cfg opts runCommand
  where
    runCLI = do
      welcome cfg opts
      t <- withTerminal pure
      simplexChatTerminal cfg opts t
    runCommand user cc = do
      when (chatCmdLog /= CCLNone) . void . forkIO . forever $ do
        (_, r) <- atomically . readTBQueue $ outputQ cc
        case r of
          Right CEvtNewChatItems {} -> printResponse r
          _ -> when (chatCmdLog == CCLAll) $ printResponse r
      sendChatCmdStr cc chatCmd >>= printResponse
      threadDelay $ chatCmdDelay * 1000000
      where
        printResponse :: ChatResponseEvent r => Either ChatError r -> IO ()
        printResponse r = do
          rh <- readTVarIO $ currentRemoteHost cc
          printResponseEvent (rh, Just user) cfg r

welcome :: ChatConfig -> ChatOpts -> IO ()
welcome ChatConfig {presetServers = PresetServers {netCfg}} ChatOpts {coreOptions = CoreChatOpts {dbOptions, simpleNetCfg = SimpleNetCfg {socksProxy, socksMode, smpProxyMode_, smpProxyFallback_}}} =
  mapM_
    putStrLn
    [ versionString versionNumber,
      "db: " <> dbString dbOptions,
      maybe
        "direct network connection - use `/network` command or `-x` CLI option to connect via SOCKS5 at :9050"
        ((\sp -> "using SOCKS5 proxy " <> sp <> if socksMode == SMOnion then " for onion servers ONLY." else " for ALL servers.") . show)
        socksProxy,
      smpProxyModeStr
        (fromMaybe (smpProxyMode netCfg) smpProxyMode_)
        (fromMaybe (smpProxyFallback netCfg) smpProxyFallback_),
      "type \"/help\" or \"/h\" for usage info"
    ]
