{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatTerminal
  ( ChatTerminal (..),
    newChatTerminal,
    chatTerminal,
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
import Control.Monad.IO.Class (liftIO)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import Numeric.Natural
import Styled
import System.Terminal
import Types
import UnliftIO.STM

newChatTerminal :: Natural -> TermMode -> IO ChatTerminal
newChatTerminal qSize termMode = do
  inputQ <- newTBQueueIO qSize
  outputQ <- newTBQueueIO qSize
  activeContact <- newTVarIO Nothing
  termSize <- withTerminal . runTerminalT $ getWindowSize
  let lastRow = height termSize - 1
  termState <- newTVarIO newTermState
  termLock <- newTMVarIO ()
  nextMessageRow <- newTVarIO lastRow
  threadDelay 500000 -- this delay is the same as timeout in getTerminalSize
  return ChatTerminal {inputQ, outputQ, activeContact, termMode, termState, termSize, nextMessageRow, termLock}

newTermState :: TerminalState
newTermState =
  TerminalState
    { inputString = "",
      inputPosition = 0,
      inputPrompt = "> ",
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
basicSendToTTY ct = forever $ readOutputQ ct >>= mapM_ putStyledLn

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
      withTermLock ct $ do
        localTime <- liftIO getZonedTime
        let localTimeStr = formatTime defaultTimeLocale "%H:%M" localTime
        printMessage ct [styleMessage localTimeStr msg]

sendToTTY :: ChatTerminal -> IO ()
sendToTTY ct = forever $ do
  -- `readOutputQ` should be outside of `withTerminal` (see #94)
  msg <- readOutputQ ct
  withTerminal . runTerminalT . withTermLock ct $ do
    printMessage ct msg
    updateInput ct

readOutputQ :: ChatTerminal -> IO [StyledString]
readOutputQ = atomically . readTBQueue . outputQ
