{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module ChatTerminal.POSIX where

import ChatTerminal.Core
import Control.Concurrent.STM
import Styled
import qualified System.Console.ANSI as C
import System.IO

initTTY :: IO ()
initTTY = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

updateInput :: ChatTerminal -> IO ()
updateInput ct@ChatTerminal {termSize, termState, nextMessageRow} = do
  C.hideCursor
  ts <- readTVarIO termState
  nmr <- readTVarIO nextMessageRow
  let (th, tw) = termSize
      ih = inputHeight ts ct
      iStart = th - ih
      prompt = inputPrompt ts
      (cRow, cCol) = relativeCursorPosition tw $ length prompt + inputPosition ts
  if nmr >= iStart
    then atomically $ writeTVar nextMessageRow iStart
    else clearLines nmr iStart
  C.setCursorPosition (max nmr iStart) 0
  putStr $ prompt <> inputString ts <> " "
  C.clearFromCursorToLineEnd
  C.setCursorPosition (iStart + cRow) cCol
  C.showCursor
  where
    clearLines :: Int -> Int -> IO ()
    clearLines from till
      | from >= till = return ()
      | otherwise = do
        C.setCursorPosition from 0
        C.clearFromCursorToLineEnd
        clearLines (from + 1) till

    relativeCursorPosition :: Int -> Int -> (Int, Int)
    relativeCursorPosition width pos =
      let row = pos `div` width
          col = pos - row * width
       in (row, col)

printMessage :: ChatTerminal -> StyledString -> IO ()
printMessage ChatTerminal {termSize, nextMessageRow} msg = do
  nmr <- readTVarIO nextMessageRow
  C.setCursorPosition nmr 0
  let (th, tw) = termSize
  lc <- printLines tw msg
  atomically . writeTVar nextMessageRow $ min (th - 1) (nmr + lc)
  where
    printLines :: Int -> StyledString -> IO Int
    printLines tw ss = do
      let s = styledToANSITerm ss
          ls
            | null s = [""]
            | otherwise = lines s <> ["" | last s == '\n']
      print_ ls
      return $ foldl (\lc l -> lc + (length l `div` tw) + 1) 0 ls

    print_ :: [String] -> IO ()
    print_ [] = return ()
    print_ (l : ls) = do
      putStr l
      C.clearFromCursorToLineEnd
      putStr "\n"
      print_ ls

getKey :: IO Key
getKey = charsToKey . reverse <$> keyChars ""
  where
    charsToKey = \case
      "\ESC" -> KeyEsc
      "\ESC[A" -> KeyUp
      "\ESC[B" -> KeyDown
      "\ESC[D" -> KeyLeft
      "\ESC[C" -> KeyRight
      "\ESCb" -> KeyAltLeft
      "\ESCf" -> KeyAltRight
      "\ESC[1;5D" -> KeyCtrlLeft
      "\ESC[1;5C" -> KeyCtrlRight
      "\ESC[1;2D" -> KeyShiftLeft
      "\ESC[1;2C" -> KeyShiftRight
      "\n" -> KeyEnter
      "\DEL" -> KeyBack
      "\t" -> KeyTab
      '\ESC' : _ -> KeyUnsupported
      cs -> KeyChars cs

    keyChars cs = do
      c <- getChar
      more <- hReady stdin
      -- for debugging - uncomment this, comment line after:
      -- (if more then keyChars else \c' -> print (reverse c') >> return c') (c : cs)
      (if more then keyChars else return) (c : cs)
