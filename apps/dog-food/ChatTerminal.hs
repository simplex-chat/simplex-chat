{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatTerminal
  ( ChatTerminal (..),
    newChatTerminal,
    runChatTerminal,
    printToTerminal,
  )
where

import ChatTerminal.Core
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import Numeric.Natural
import Simplex.Chat.Controller
import Simplex.Chat.Styled
import Simplex.Input
import Simplex.Terminal
import System.Terminal
import UnliftIO.STM

newChatTerminal :: Natural -> IO ChatTerminal
newChatTerminal qSize = do
  inputQ <- newTBQueueIO qSize
  outputQ <- newTBQueueIO qSize
  activeTo <- newTVarIO ActiveNone
  termSize <- withTerminal . runTerminalT $ getWindowSize
  let lastRow = height termSize - 1
  termState <- newTVarIO newTermState
  termLock <- newTMVarIO ()
  nextMessageRow <- newTVarIO lastRow
  threadDelay 500000 -- this delay is the same as timeout in getTerminalSize
  return ChatTerminal {inputQ, outputQ, activeTo, termState, termSize, nextMessageRow, termLock}

newTermState :: TerminalState
newTermState =
  TerminalState
    { inputString = "",
      inputPosition = 0,
      inputPrompt = "> ",
      previousInput = ""
    }

runChatTerminal :: ChatController -> ChatTerminal -> IO ()
runChatTerminal cc ct = do
  withTerminal . runTerminalT $ updateInput ct
  race_ (receiveFromTTY cc ct) (sendToTTY ct)

receiveFromTTY :: ChatController -> ChatTerminal -> IO ()
receiveFromTTY ChatController {inputEventQ} ct@ChatTerminal {inputQ, activeTo, termSize, termState} =
  withTerminal . runTerminalT . forever $
    getKey >>= processKey >> withTermLock ct (updateInput ct)
  where
    processKey :: MonadTerminal m => (Key, Modifiers) -> m ()
    processKey = \case
      (EnterKey, _) -> submitInput
      key -> atomically $ do
        ac <- readTVar activeTo
        modifyTVar termState $ updateTermState ac (width termSize) key

    submitInput :: MonadTerminal m => m ()
    submitInput = do
      msg <- atomically $ do
        ts <- readTVar termState
        let s = inputString ts
        writeTVar termState $ ts {inputString = "", inputPosition = 0, previousInput = s}
        writeTBQueue inputQ s
        writeTBQueue inputEventQ $ InputCommand s
        return s
      withTermLock ct $ do
        localTime <- liftIO getZonedTime
        let localTimeStr = formatTime defaultTimeLocale "%H:%M" localTime
        printMessage ct [styleMessage localTimeStr msg]

sendToTTY :: ChatTerminal -> IO ()
sendToTTY ct = forever $ readOutputQ ct >>= printToTerminal ct

readOutputQ :: ChatTerminal -> IO [StyledString]
readOutputQ = atomically . readTBQueue . outputQ
