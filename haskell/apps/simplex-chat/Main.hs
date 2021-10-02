{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Simplex.Chat
import Simplex.Chat.Options
import System.Directory (getAppUserDataDirectory)
import System.Terminal (withTerminal)

main :: IO ()
main = do
  opts <- welcomeGetOpts
  t <- withTerminal pure
  simplexChat defaultChatConfig opts t

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {dbFile} <- getChatOpts appDir
  putStrLn "SimpleX chat prototype v0.4.0"
  putStrLn $ "db: " <> dbFile <> ".chat.db, " <> dbFile <> ".agent.db"
  putStrLn "type \"/help\" or \"/h\" for usage info"
  pure opts

-- defaultSettings :: C.Size -> C.VirtualTerminalSettings
-- defaultSettings size =
--   C.VirtualTerminalSettings
--     { C.virtualType = "xterm",
--       C.virtualWindowSize = pure size,
--       C.virtualEvent = retry,
--       C.virtualInterrupt = retry
--     }

-- main :: IO ()
-- main = do
--   void $ createStore "simplex-chat.db" 4

--   hFlush stdout
--   -- ChatTerminal {termSize} <- newChatTerminal
--   -- pos <- C.withVirtualTerminal (defaultSettings termSize) $
--   --   \t -> runTerminalT (C.setAlternateScreenBuffer True >> C.putString "a" >> C.flush >> C.getCursorPosition) t
--   -- print pos
--   -- race_ (printEvents t) (updateTerminal t)
--   void . withTerminal . runTerminalT $ chatLayoutDemo >> C.flush >> C.awaitEvent

-- printEvents :: C.VirtualTerminal -> IO ()
-- printEvents t = forever $ do
--   event <- withTerminal . runTerminalT $ C.flush >> C.awaitEvent
--   runTerminalT (putStringLn $ show event) t

-- updateTerminal :: C.VirtualTerminal -> IO ()
-- updateTerminal t = forever $ do
--   threadDelay 10000
--   win <- readTVarIO $ C.virtualWindow t
--   withTerminal . runTerminalT $ mapM_ C.putStringLn win >> C.flush
