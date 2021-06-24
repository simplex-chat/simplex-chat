{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (readTVarIO, retry)
import Control.Monad (forever, void)
import Simplex.Demo (chatLayoutDemo)
import Simplex.Store (createStore)
import System.IO (hFlush, stdout)
import System.Terminal (putStringLn, runTerminalT, withTerminal)
import qualified System.Terminal as C
import qualified System.Terminal.Internal as C

defaultSettings :: C.Size -> C.VirtualTerminalSettings
defaultSettings size =
  C.VirtualTerminalSettings
    { C.virtualType = "xterm",
      C.virtualWindowSize = pure size,
      C.virtualEvent = retry,
      C.virtualInterrupt = retry
    }

main :: IO ()
main = do
  void $ createStore "simplex-chat.db" 4

  hFlush stdout
  -- ChatTerminal {termSize} <- newChatTerminal
  -- pos <- C.withVirtualTerminal (defaultSettings termSize) $
  --   \t -> runTerminalT (C.setAlternateScreenBuffer True >> C.putString "a" >> C.flush >> C.getCursorPosition) t
  -- print pos
  -- race_ (printEvents t) (updateTerminal t)
  void . withTerminal . runTerminalT $ chatLayoutDemo >> C.flush >> C.awaitEvent

printEvents :: C.VirtualTerminal -> IO ()
printEvents t = forever $ do
  event <- withTerminal . runTerminalT $ C.flush >> C.awaitEvent
  runTerminalT (putStringLn $ show event) t

updateTerminal :: C.VirtualTerminal -> IO ()
updateTerminal t = forever $ do
  threadDelay 10000
  win <- readTVarIO $ C.virtualWindow t
  withTerminal . runTerminalT $ mapM_ C.putStringLn win >> C.flush
