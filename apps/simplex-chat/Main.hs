{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (readTVarIO, retry)
import Control.Monad (forever, void)
-- import Control.Monad.IO.Class (liftIO)
-- import Simplex.Keyboard (getKey)
import Simplex.Store (createStore)
import Simplex.Terminal (ChatTerminal (..), newChatTerminal)
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
  ChatTerminal {termSize} <- newChatTerminal
  t <- C.withVirtualTerminal (defaultSettings termSize) $
    \t -> runTerminalT (C.setAlternateScreenBuffer True >> C.flush) t >> pure t

  race_ (printEvents t) (updateTerminal t)

printEvents :: C.VirtualTerminal -> IO ()
printEvents t = forever $ do
  event <- withTerminal . runTerminalT $ C.flush >> C.awaitEvent
  runTerminalT (putStringLn $ show event) t

updateTerminal :: C.VirtualTerminal -> IO ()
updateTerminal t = forever $ do
  threadDelay 10000
  win <- readTVarIO $ C.virtualWindow t
  withTerminal . runTerminalT $ mapM_ C.putStringLn win >> C.flush
