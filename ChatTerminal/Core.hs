{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatTerminal.Core where

import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as B
import Data.List (dropWhileEnd)
import qualified Data.Text as T
import SimplexMarkdown
import Styled
import System.Console.ANSI.Types
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

inputHeight :: TerminalState -> ChatTerminal -> Int
inputHeight ts ct = length (inputPrompt ts <> inputString ts) `div` snd (termSize ct) + 1

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

styleMessage :: String -> StyledString
styleMessage = \case
  "" -> ""
  s@('@' : _) -> let (c, rest) = span (/= ' ') s in Styled selfSGR c <> markdown rest
  s -> markdown s
  where
    markdown :: String -> StyledString
    markdown = styleMarkdown . parseMarkdown . T.pack

updateUsername :: ChatTerminal -> Maybe Contact -> STM ()
updateUsername ct a = do
  writeTVar (username ct) a
  modifyTVar (termState ct) $ \ts -> ts {inputPrompt = promptString a}

promptString :: Maybe Contact -> String
promptString a = maybe "" (B.unpack . toBs) a <> "> "

ttyContact :: Contact -> StyledString
ttyContact (Contact a) = Styled contactSGR $ B.unpack a

ttyFromContact :: Contact -> StyledString
ttyFromContact (Contact a) = Styled contactSGR $ B.unpack a <> ">"

contactSGR :: [SGR]
contactSGR = [SetColor Foreground Vivid Yellow]

selfSGR :: [SGR]
selfSGR = [SetColor Foreground Vivid Cyan]
