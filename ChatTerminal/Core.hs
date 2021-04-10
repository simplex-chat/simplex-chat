{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatTerminal.Core where

import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as B
import Data.List (dropWhileEnd)
import qualified Data.Text as T
import Simplex.Markdown
import Styled
import System.Console.ANSI.Types
import System.Terminal hiding (insertChars)
import Types

data ChatTerminal = ChatTerminal
  { inputQ :: TBQueue String,
    outputQ :: TBQueue StyledString,
    activeContact :: TVar (Maybe Contact),
    username :: TVar (Maybe Contact),
    termMode :: TermMode,
    termState :: TVar TerminalState,
    termSize :: Size,
    nextMessageRow :: TVar Int,
    termLock :: TMVar ()
  }

data TerminalState = TerminalState
  { inputPrompt :: String,
    inputString :: String,
    inputPosition :: Int,
    previousInput :: String
  }

inputHeight :: TerminalState -> ChatTerminal -> Int
inputHeight ts ct = length (inputPrompt ts <> inputString ts) `div` width (termSize ct) + 1

positionRowColumn :: Int -> Int -> Position
positionRowColumn wid pos =
  let row = pos `div` wid
      col = pos - row * wid
   in Position {row, col}

updateTermState :: Maybe Contact -> Int -> (Key, Modifiers) -> TerminalState -> TerminalState
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
    Leftwards
      | ms == mempty -> setPosition $ max 0 (p - 1)
      | ms == shiftKey -> setPosition 0
      | ms == ctrlKey -> setPosition prevWordPos
      | ms == altKey -> setPosition prevWordPos
      | otherwise -> setPosition p
    Rightwards
      | ms == mempty -> setPosition $ min (length s) (p + 1)
      | ms == shiftKey -> setPosition $ length s
      | ms == ctrlKey -> setPosition nextWordPos
      | ms == altKey -> setPosition nextWordPos
      | otherwise -> setPosition p
    Upwards
      | ms == mempty && null s -> let s' = previousInput ts in ts' (s', length s')
      | ms == mempty -> let p' = p - tw in setPosition $ if p' > 0 then p' else p
      | otherwise -> setPosition p
    Downwards
      | ms == mempty -> let p' = p + tw in setPosition $ if p' <= length s then p' else p
      | otherwise -> setPosition p
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
      | p >= length s = ts' (init s, length s - 1)
      | otherwise = let (b, a) = splitAt p s in ts' (init b <> a, p - 1)
    deleteChar
      | p >= length s || null s = ts
      | p == 0 = ts' (tail s, 0)
      | otherwise = let (b, a) = splitAt p s in ts' (b <> tail a, p)
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
