{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Input where

import ChatTerminal.Core
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.List (dropWhileEnd)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import Simplex.Chat.Controller
import Simplex.Terminal
import System.Exit (exitSuccess)
import System.Terminal hiding (insertChars)
import Types
import UnliftIO.STM

getKey :: MonadTerminal m => m (Key, Modifiers)
getKey =
  flush >> awaitEvent >>= \case
    Left Interrupt -> liftIO exitSuccess
    Right (KeyEvent key ms) -> pure (key, ms)
    _ -> getKey

runTerminalInput :: ChatController -> ChatTerminal -> IO ()
runTerminalInput cc ct = do
  withTerminal . runTerminalT $ updateInput ct
  receiveFromTTY cc ct

receiveFromTTY :: ChatController -> ChatTerminal -> IO ()
receiveFromTTY ChatController {inputQ} ct@ChatTerminal {activeTo, termSize, termState} =
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
        writeTBQueue inputQ $ InputCommand s
        return s
      withTermLock ct $ do
        localTime <- liftIO getZonedTime
        let localTimeStr = formatTime defaultTimeLocale "%H:%M" localTime
        printMessage ct [styleMessage localTimeStr msg]

updateTermState :: ActiveTo -> Int -> (Key, Modifiers) -> TerminalState -> TerminalState
updateTermState ac tw (key, ms) ts@TerminalState {inputString = s, inputPosition = p} = case key of
  CharKey c
    | ms == mempty || ms == shiftKey -> insertCharsWithContact [c]
    | ms == altKey && c == 'b' -> setPosition prevWordPos
    | ms == altKey && c == 'f' -> setPosition nextWordPos
    | otherwise -> ts
  TabKey -> insertCharsWithContact "    "
  BackspaceKey -> backDeleteChar
  DeleteKey -> deleteChar
  HomeKey -> setPosition 0
  EndKey -> setPosition $ length s
  ArrowKey d -> case d of
    Leftwards -> setPosition leftPos
    Rightwards -> setPosition rightPos
    Upwards
      | ms == mempty && null s -> let s' = previousInput ts in ts' (s', length s')
      | ms == mempty -> let p' = p - tw in if p' > 0 then setPosition p' else ts
      | otherwise -> ts
    Downwards
      | ms == mempty -> let p' = p + tw in if p' <= length s then setPosition p' else ts
      | otherwise -> ts
  _ -> ts
  where
    insertCharsWithContact cs
      | null s && cs /= "@" && cs /= "#" && cs /= "/" =
        insertChars $ contactPrefix <> cs
      | otherwise = insertChars cs
    insertChars = ts' . if p >= length s then append else insert
    append cs = let s' = s <> cs in (s', length s')
    insert cs = let (b, a) = splitAt p s in (b <> cs <> a, p + length cs)
    contactPrefix = case ac of
      ActiveNone -> ""
      ActiveC (Contact c) -> "@" <> B.unpack c <> " "
      ActiveG (Group g) -> "#" <> B.unpack g <> " "
    backDeleteChar
      | p == 0 || null s = ts
      | p >= length s = ts' (init s, length s - 1)
      | otherwise = let (b, a) = splitAt p s in ts' (init b <> a, p - 1)
    deleteChar
      | p >= length s || null s = ts
      | p == 0 = ts' (tail s, 0)
      | otherwise = let (b, a) = splitAt p s in ts' (b <> tail a, p)
    leftPos
      | ms == mempty = max 0 (p - 1)
      | ms == shiftKey = 0
      | ms == ctrlKey = prevWordPos
      | ms == altKey = prevWordPos
      | otherwise = p
    rightPos
      | ms == mempty = min (length s) (p + 1)
      | ms == shiftKey = length s
      | ms == ctrlKey = nextWordPos
      | ms == altKey = nextWordPos
      | otherwise = p
    setPosition p' = ts' (s, p')
    prevWordPos
      | p == 0 || null s = p
      | otherwise =
        let before = take p s
            beforeWord = dropWhileEnd (/= ' ') $ dropWhileEnd (== ' ') before
         in max 0 $ p - length before + length beforeWord
    nextWordPos
      | p >= length s || null s = p
      | otherwise =
        let after = drop p s
            afterWord = dropWhile (/= ' ') $ dropWhile (== ' ') after
         in min (length s) $ p + length after - length afterWord
    ts' (s', p') = ts {inputString = s', inputPosition = p'}
