{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatTerminal
  ( ChatTerminal (..),
    newChatTerminal,
    chatTerminal,
    updateUsername,
    ttyContact,
    ttyFromContact,
  )
where

import ChatTerminal.Basic (getLn, putLn)
import ChatTerminal.Core
import ChatTerminal.POSIX
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe (fromMaybe)
import Numeric.Natural
import Styled
import qualified System.Console.ANSI as C
import Types

newChatTerminal :: Natural -> Maybe Contact -> TermMode -> IO ChatTerminal
newChatTerminal qSize user termMode = do
  inputQ <- newTBQueueIO qSize
  outputQ <- newTBQueueIO qSize
  activeContact <- newTVarIO Nothing
  username <- newTVarIO user
  termSize <- fromMaybe (0, 0) <$> C.getTerminalSize
  let lastRow = fst termSize - 1
  termState <- newTVarIO $ newTermState user
  termLock <- newTMVarIO ()
  nextMessageRow <- newTVarIO lastRow
  threadDelay 500000 -- this delay is the same as timeout in getTerminalSize
  return ChatTerminal {inputQ, outputQ, activeContact, username, termMode, termState, termSize, nextMessageRow, termLock}

newTermState :: Maybe Contact -> TerminalState
newTermState user =
  TerminalState
    { inputString = "",
      inputPosition = 0,
      inputPrompt = promptString user
    }

chatTerminal :: ChatTerminal -> IO ()
chatTerminal ct
  | termSize ct == (0, 0) || termMode ct == TermModeBasic =
    run basicReceiveFromTTY basicSendToTTY
  | otherwise = do
    initTTY
    updateInput ct
    run receiveFromTTY sendToTTY
  where
    run receive send = race_ (receive ct) (send ct)

basicReceiveFromTTY :: ChatTerminal -> IO ()
basicReceiveFromTTY ct =
  forever $ getLn >>= atomically . writeTBQueue (inputQ ct)

basicSendToTTY :: ChatTerminal -> IO ()
basicSendToTTY ct = forever $ readOutputQ ct >>= putLn

withTermLock :: ChatTerminal -> IO () -> IO ()
withTermLock ChatTerminal {termLock} action = do
  _ <- atomically $ takeTMVar termLock
  action
  atomically $ putTMVar termLock ()

receiveFromTTY :: ChatTerminal -> IO ()
receiveFromTTY ct@ChatTerminal {inputQ, activeContact, termSize, termState} =
  forever $
    getKey >>= processKey >> withTermLock ct (updateInput ct)
  where
    processKey :: Key -> IO ()
    processKey = \case
      KeyEnter -> submitInput
      key -> atomically $ do
        ac <- readTVar activeContact
        modifyTVar termState $ updateTermState ac (snd termSize) key

    submitInput :: IO ()
    submitInput = do
      msg <- atomically $ do
        ts <- readTVar termState
        writeTVar termState $ ts {inputString = "", inputPosition = 0}
        let s = inputString ts
        writeTBQueue inputQ s
        return s
      withTermLock ct . printMessage ct $ styleMessage msg

sendToTTY :: ChatTerminal -> IO ()
sendToTTY ct = forever $ do
  msg <- readOutputQ ct
  withTermLock ct $ do
    printMessage ct msg
    updateInput ct

readOutputQ :: ChatTerminal -> IO StyledString
readOutputQ = atomically . readTBQueue . outputQ
