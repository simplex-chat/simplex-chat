{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChatTerminal.Editor where

import ChatTerminal.Basic
import ChatTerminal.Core
import Simplex.Chat.Styled
import System.Terminal
import UnliftIO.STM

-- debug :: MonadTerminal m => String -> m ()
-- debug s = do
--   saveCursor
--   setCursorPosition $ Position 0 0
--   putString s
--   restoreCursor

updateInput :: forall m. MonadTerminal m => ChatTerminal -> m ()
updateInput ct@ChatTerminal {termSize = Size {height, width}, termState, nextMessageRow} = do
  hideCursor
  ts <- readTVarIO termState
  nmr <- readTVarIO nextMessageRow
  let ih = inputHeight ts ct
      iStart = height - ih
      prompt = inputPrompt ts
      Position {row, col} = positionRowColumn width $ length prompt + inputPosition ts
  if nmr >= iStart
    then atomically $ writeTVar nextMessageRow iStart
    else clearLines nmr iStart
  setCursorPosition $ Position {row = max nmr iStart, col = 0}
  putString $ prompt <> inputString ts <> " "
  eraseInLine EraseForward
  setCursorPosition $ Position {row = iStart + row, col}
  showCursor
  flush
  where
    clearLines :: Int -> Int -> m ()
    clearLines from till
      | from >= till = return ()
      | otherwise = do
        setCursorPosition $ Position {row = from, col = 0}
        eraseInLine EraseForward
        clearLines (from + 1) till

printMessage :: forall m. MonadTerminal m => ChatTerminal -> [StyledString] -> m ()
printMessage ChatTerminal {termSize = Size {height, width}, nextMessageRow} msg = do
  nmr <- readTVarIO nextMessageRow
  setCursorPosition $ Position {row = nmr, col = 0}
  mapM_ printStyled msg
  flush
  let lc = sum $ map lineCount msg
  atomically . writeTVar nextMessageRow $ min (height - 1) (nmr + lc)
  where
    lineCount :: StyledString -> Int
    lineCount s = sLength s `div` width + 1
    printStyled :: StyledString -> m ()
    printStyled s = do
      putStyled s
      eraseInLine EraseForward
      putLn
