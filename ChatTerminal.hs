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

import ChatTerminal.Basic
import ChatTerminal.Core
import ChatTerminal.Editor
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Monad
import Numeric.Natural
import Styled
import System.Terminal
import Types
import UnliftIO.STM

newChatTerminal :: Natural -> Maybe Contact -> TermMode -> IO ChatTerminal
newChatTerminal qSize user termMode = do
  inputQ <- newTBQueueIO qSize
  outputQ <- newTBQueueIO qSize
  activeContact <- newTVarIO Nothing
  username <- newTVarIO user
  termSize <- withTerminal . runTerminalT $ getWindowSize
  let lastRow = height termSize - 1
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
      inputPrompt = promptString user,
      previousInput = ""
    }

chatTerminal :: ChatTerminal -> IO ()
chatTerminal ct
  | termSize ct == Size 0 0 || termMode ct == TermModeBasic =
    run basicReceiveFromTTY basicSendToTTY
  | otherwise = do
    withTerminal . runTerminalT $ updateInput ct
    run receiveFromTTY sendToTTY
  where
    run receive send = race_ (receive ct) (send ct)

basicReceiveFromTTY :: ChatTerminal -> IO ()
basicReceiveFromTTY ct =
  forever $ getLn >>= atomically . writeTBQueue (inputQ ct)

basicSendToTTY :: ChatTerminal -> IO ()
basicSendToTTY ct = forever $ atomically (readOutputQ ct) >>= putStyledLn

withTermLock :: MonadTerminal m => ChatTerminal -> m () -> m ()
withTermLock ChatTerminal {termLock} action = do
  _ <- atomically $ takeTMVar termLock
  action
  atomically $ putTMVar termLock ()

receiveFromTTY :: ChatTerminal -> IO ()
receiveFromTTY ct@ChatTerminal {inputQ, activeContact, termSize, termState} =
  withTerminal . runTerminalT . forever $
    getKey >>= processKey >> withTermLock ct (updateInput ct)
  where
    processKey :: MonadTerminal m => (Key, Modifiers) -> m ()
    processKey = \case
      (EnterKey, _) -> submitInput
      key -> atomically $ do
        ac <- readTVar activeContact
        modifyTVar termState $ updateTermState ac (width termSize) key

    submitInput :: MonadTerminal m => m ()
    submitInput = do
      msg <- atomically $ do
        ts <- readTVar termState
        let s = inputString ts
        writeTVar termState $ ts {inputString = "", inputPosition = 0, previousInput = s}
        writeTBQueue inputQ s
        return s
      withTermLock ct . printMessage ct $ styleMessage msg

sendToTTY :: ChatTerminal -> IO ()
sendToTTY ct = withTerminal . runTerminalT . forever $ do
  msg <- atomically $ readOutputQ ct
  withTermLock ct $ do
    printMessage ct msg
    updateInput ct

readOutputQ :: ChatTerminal -> STM StyledString
readOutputQ = readTBQueue . outputQ
