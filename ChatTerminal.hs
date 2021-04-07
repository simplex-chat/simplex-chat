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

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Numeric.Natural
import Styled
import qualified System.Console.ANSI as C
import System.IO
import Terminal (getLn, putLn)
import Types

data ChatTerminal = ChatTerminal
  { inputQ :: TBQueue String,
    outputQ :: TBQueue StyledString,
    activeContact :: TVar (Maybe Contact),
    username :: TVar (Maybe Contact),
    termMode :: TermMode,
    termState :: TVar TerminalState,
    termSize :: (Int, Int),
    nextMessageRow :: TVar Int,
    termLock :: TMVar ()
  }

data TerminalState = TerminalState
  { inputPrompt :: String,
    inputString :: String,
    inputPosition :: Int
  }

inputHeight :: TerminalState -> ChatTerminal -> Int
inputHeight ts ct = length (inputPrompt ts <> inputString ts) `div` snd (termSize ct) + 1

data Key
  = KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeyAltLeft
  | KeyAltRight
  | KeyCtrlLeft
  | KeyCtrlRight
  | KeyShiftLeft
  | KeyShiftRight
  | KeyEnter
  | KeyBack
  | KeyTab
  | KeyEsc
  | KeyChars String
  | KeyUnsupported
  deriving (Eq)

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
    run receiveFromTTY sendToTTY
  | otherwise = do
    setTTY NoBuffering
    hSetEcho stdin False
    updateInput ct
    run receiveFromTTY' sendToTTY'
  where
    run receive send = race_ (receive ct) (send ct)

receiveFromTTY :: ChatTerminal -> IO ()
receiveFromTTY ct =
  forever $ getLn >>= atomically . writeTBQueue (inputQ ct)

withTermLock :: ChatTerminal -> IO () -> IO ()
withTermLock ChatTerminal {termLock} action = do
  _ <- atomically $ takeTMVar termLock
  action
  atomically $ putTMVar termLock ()

receiveFromTTY' :: ChatTerminal -> IO ()
receiveFromTTY' ct@ChatTerminal {inputQ, activeContact, termSize, termState} =
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
      withTermLock ct . printMessage ct $ highlightContact msg

    updateTermState :: Maybe Contact -> Int -> Key -> TerminalState -> TerminalState
    updateTermState ac tw key ts@TerminalState {inputString = s, inputPosition = p} = case key of
      KeyChars cs -> insertCharsWithContact cs
      KeyTab -> insertChars "    "
      KeyBack -> backDeleteChar
      KeyLeft -> setPosition $ max 0 (p - 1)
      KeyRight -> setPosition $ min (length s) (p + 1)
      KeyUp -> setPosition $ let p' = p - tw in if p' > 0 then p' else p
      KeyDown -> setPosition $ let p' = p + tw in if p' <= length s then p' else p
      KeyAltLeft -> setPosition prevWordPos
      KeyAltRight -> setPosition nextWordPos
      KeyCtrlLeft -> setPosition prevWordPos
      KeyCtrlRight -> setPosition nextWordPos
      KeyShiftLeft -> setPosition 0
      KeyShiftRight -> setPosition $ length s
      _ -> ts
      where
        insertCharsWithContact cs
          | null s && cs /= "@" && cs /= "/" =
            insertChars $ contactPrefix <> cs
          | otherwise = insertChars cs
        insertChars = ts' . if p >= length s then append else insert
        append cs = let s' = s <> cs in (s', length s')
        insert cs = let (b, a) = splitAt p s in (b <> cs <> a, p + length cs)
        contactPrefix = case ac of
          Just (Contact c) -> "@" <> B.unpack c <> " "
          Nothing -> ""
        backDeleteChar
          | p == 0 || null s = ts
          | p >= length s = ts' backDeleteLast
          | otherwise = ts' backDelete
        backDeleteLast = if null s then (s, 0) else let s' = init s in (s', length s')
        backDelete = let (b, a) = splitAt p s in (init b <> a, p - 1)
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

highlightContact :: String -> StyledString
highlightContact = \case
  "" -> ""
  s@('@' : _) -> let (c, rest) = span (/= ' ') s in Styled selfSGR c <> plain rest
  s -> plain s

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

updateUsername :: ChatTerminal -> Maybe Contact -> STM ()
updateUsername ct a = do
  writeTVar (username ct) a
  modifyTVar (termState ct) $ \ts -> ts {inputPrompt = promptString a}

promptString :: Maybe Contact -> String
promptString a = maybe "" (B.unpack . toBs) a <> "> "

sendToTTY :: ChatTerminal -> IO ()
sendToTTY ct = forever $ readOutputQ ct >>= putLn

sendToTTY' :: ChatTerminal -> IO ()
sendToTTY' ct = forever $ do
  msg <- readOutputQ ct
  withTermLock ct $ do
    printMessage ct msg
    updateInput ct

readOutputQ :: ChatTerminal -> IO StyledString
readOutputQ = atomically . readTBQueue . outputQ

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

setTTY :: BufferMode -> IO ()
setTTY mode = do
  hSetBuffering stdin mode
  hSetBuffering stdout mode

ttyContact :: Contact -> StyledString
ttyContact (Contact a) = Styled contactSGR $ B.unpack a

ttyFromContact :: Contact -> StyledString
ttyFromContact (Contact a) = Styled contactSGR $ B.unpack a <> ">"

contactSGR :: [C.SGR]
contactSGR = [C.SetColor C.Foreground C.Vivid C.Yellow]

selfSGR :: [C.SGR]
selfSGR = [C.SetColor C.Foreground C.Vivid C.Cyan]
