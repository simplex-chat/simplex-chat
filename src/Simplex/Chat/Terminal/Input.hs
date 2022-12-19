{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.Terminal.Input where

import Control.Concurrent (forkFinally, forkIO, killThread, mkWeakThreadId, threadDelay)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Char (isAlphaNum)
import Data.List (dropWhileEnd, foldl')
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Weak (deRefWeak)
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Messages
import Simplex.Chat.Styled
import Simplex.Chat.Terminal.Output
import Simplex.Messaging.Util (safeDecodeUtf8, whenM)
import System.Exit (exitSuccess)
import System.Terminal hiding (insertChars)
import UnliftIO.STM

getKey :: MonadTerminal m => m (Key, Modifiers)
getKey =
  flush >> awaitEvent >>= \case
    Left Interrupt -> liftIO exitSuccess
    Right (KeyEvent key ms) -> pure (key, ms)
    _ -> getKey

runInputLoop :: ChatTerminal -> ChatController -> IO ()
runInputLoop ct@ChatTerminal {termState, liveMessageState} cc = forever $ do
  s <- atomically . readTBQueue $ inputQ cc
  let bs = encodeUtf8 $ T.pack s
      cmd = parseChatCommand bs
  unless (isMessage cmd) $ echo s
  r <- runReaderT (execChatCommand bs) cc
  case r of
    CRChatCmdError _ -> when (isMessage cmd) $ echo s
    _ -> pure ()
  printRespToTerminal ct cc False r
  startLiveMessage cmd r
  where
    echo s = printToTerminal ct [plain s]
    isMessage = \case
      Right SendMessage {} -> True
      Right SendLiveMessage {} -> True
      Right SendFile {} -> True
      Right SendMessageQuote {} -> True
      Right SendGroupMessageQuote {} -> True
      Right SendMessageBroadcast {} -> True
      _ -> False
    startLiveMessage :: Either a ChatCommand -> ChatResponse -> IO ()
    startLiveMessage (Right (SendLiveMessage chatName msg)) (CRNewChatItem (AChatItem cType SMDSnd _ ChatItem {meta = CIMeta {itemId}})) = do
      whenM (isNothing <$> readTVarIO liveMessageState) $ do
        let s = T.unpack $ safeDecodeUtf8 msg
            int = case cType of SCTGroup -> 5000000; _ -> 3000000 :: Int
        liveThreadId <- mkWeakThreadId =<< runLiveMessage int `forkFinally` const (atomically $ writeTVar liveMessageState Nothing)
        promptThreadId <- mkWeakThreadId =<< forkIO blinkLivePrompt
        atomically $ do
          let lm = LiveMessage {chatName, chatItemId = itemId, livePrompt = True, sentMsg = s, typedMsg = s, liveThreadId, promptThreadId}
          writeTVar liveMessageState (Just lm)
          modifyTVar termState $ \ts -> ts {inputString = s, inputPosition = length s, inputPrompt = liveInputPrompt lm}
      where
        liveInputPrompt LiveMessage {chatName = n, livePrompt} =
          "> " <> chatNameStr n <> " [" <> (if livePrompt then "LIVE" else "    ") <> "] "
        runLiveMessage :: Int -> IO ()
        runLiveMessage int = do
          threadDelay int
          TerminalState {inputString = s} <- readTVarIO termState
          readTVarIO liveMessageState
            >>= mapM_ (\lm -> updateLiveMessage s lm >> runLiveMessage int)
        blinkLivePrompt = readTVarIO liveMessageState >>= mapM_ updateLivePrompt
          where
            updateLivePrompt lm = do
              atomically $ updatePrompt lm
              updateInputView ct
              threadDelay 1000000
              blinkLivePrompt
            updatePrompt lm = do
              writeTVar liveMessageState $ Just lm {livePrompt = not $ livePrompt lm}
              modifyTVar termState (\ts -> ts {inputPrompt = liveInputPrompt lm})
        liveMessageToSend t LiveMessage {sentMsg, typedMsg} =
          let s = if t /= typedMsg then truncateToWords t else t
           in if s /= sentMsg then Just s else Nothing
        updateLiveMessage typedMsg lm = case liveMessageToSend typedMsg lm of
          Just sentMsg ->
            sendUpdatedLiveMessage cc sentMsg lm True >>= \case
              CRChatItemUpdated {} -> setLiveMessage lm {sentMsg, typedMsg}
              _ -> do
                -- TODO print error
                setLiveMessage lm {typedMsg}
          _ -> setLiveMessage lm {typedMsg}
        setLiveMessage :: LiveMessage -> IO ()
        setLiveMessage = atomically . writeTVar liveMessageState . Just
        truncateToWords = fst . foldl' acc ("", "")
          where
            acc (s, w) c
              | isAlphaNum c = (s, c : w)
              | otherwise = (s <> reverse (c : w), "")
    startLiveMessage _ _ = pure ()

sendUpdatedLiveMessage :: ChatController -> String -> LiveMessage -> Bool -> IO ChatResponse
sendUpdatedLiveMessage cc sentMsg LiveMessage {chatName, chatItemId} live = do
  let bs = encodeUtf8 $ T.pack sentMsg
      cmd = UpdateLiveMessage chatName chatItemId live bs
  either CRChatCmdError id <$> runExceptT (processChatCommand cmd) `runReaderT` cc

runTerminalInput :: ChatTerminal -> ChatController -> IO ()
runTerminalInput ct cc = withChatTerm ct $ do
  updateInput ct
  receiveFromTTY cc ct

receiveFromTTY :: forall m. MonadTerminal m => ChatController -> ChatTerminal -> m ()
receiveFromTTY cc@ChatController {inputQ, activeTo} ct@ChatTerminal {termSize, termState, liveMessageState} =
  forever $ getKey >>= liftIO . processKey >> withTermLock ct (updateInput ct)
  where
    processKey :: (Key, Modifiers) -> IO ()
    processKey key = case key of
      (EnterKey, ms)
        | ms == mempty -> submit False
        | ms == altKey -> submit True
        | otherwise -> pure ()
      (CharKey c, ms)
        | (c == 'l' || c == 'L') && ms == ctrlKey -> submit True
        | otherwise -> update key
      _ -> update key
    submit live =
      atomically (readTVar termState >>= submitInput live)
        >>= mapM_ (uncurry endLiveMessage)
    update key = atomically $ do
      ac <- readTVar activeTo
      live <- isJust <$> readTVar liveMessageState
      modifyTVar termState $ updateTermState ac live (width termSize) key

    endLiveMessage :: String -> LiveMessage -> IO ()
    endLiveMessage sentMsg lm = do
      kill liveThreadId
      kill promptThreadId
      atomically $ writeTVar liveMessageState Nothing
      r <- sendUpdatedLiveMessage cc sentMsg lm False
      printRespToTerminal ct cc False r
      where
        kill sel = deRefWeak (sel lm) >>= mapM_ killThread

    submitInput :: Bool -> TerminalState -> STM (Maybe (String, LiveMessage))
    submitInput live ts = do
      let s = inputString ts
      lm_ <- readTVar liveMessageState
      case lm_ of
        Just LiveMessage {chatName}
          | live -> do
            writeTVar termState ts' {previousInput}
            writeTBQueue inputQ $ "/live " <> chatNameStr chatName
          | otherwise ->
            writeTVar termState ts' {inputPrompt = "> ", previousInput}
          where
            previousInput = chatNameStr chatName <> " " <> s
        _
          | live -> when (isSend s) $ do
            writeTVar termState ts' {previousInput = s}
            writeTBQueue inputQ $ "/live " <> s
          | otherwise -> do
            writeTVar termState ts' {inputPrompt = "> ", previousInput = s}
            writeTBQueue inputQ s
      pure $ (s,) <$> lm_
      where
        isSend s = length s > 1 && (head s == '@' || head s == '#')
        ts' = ts {inputString = "", inputPosition = 0}

updateTermState :: ActiveTo -> Bool -> Int -> (Key, Modifiers) -> TerminalState -> TerminalState
updateTermState ac live tw (key, ms) ts@TerminalState {inputString = s, inputPosition = p} = case key of
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
      | ms == mempty && null s -> let s' = upArrowCmd $ previousInput ts in ts' (s', length s')
      | ms == mempty -> let p' = p - tw in if p' > 0 then setPosition p' else ts
      | otherwise -> ts
    Downwards
      | ms == mempty -> let p' = p + tw in if p' <= length s then setPosition p' else ts
      | otherwise -> ts
  _ -> ts
  where
    insertCharsWithContact cs
      | live = insertChars cs
      | null s && cs /= "@" && cs /= "#" && cs /= "/" && cs /= ">" && cs /= "\\" && cs /= "!" =
        insertChars $ contactPrefix <> cs
      | (s == ">" || s == "\\" || s == "!") && cs == " " =
        insertChars $ cs <> contactPrefix
      | otherwise = insertChars cs
    insertChars = ts' . if p >= length s then append else insert
    append cs = let s' = s <> cs in (s', length s')
    insert cs = let (b, a) = splitAt p s in (b <> cs <> a, p + length cs)
    contactPrefix = case ac of
      ActiveNone -> ""
      ActiveC c -> "@" <> T.unpack c <> " "
      ActiveG g -> "#" <> T.unpack g <> " "
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
    upArrowCmd inp = case parseChatCommand . encodeUtf8 $ T.pack inp of
      Left _ -> inp
      Right cmd -> case cmd of
        SendMessage {} -> "! " <> inp
        SendMessageQuote {contactName, message} -> T.unpack $ "! @" <> contactName <> " " <> safeDecodeUtf8 message
        SendGroupMessageQuote {groupName, message} -> T.unpack $ "! #" <> groupName <> " " <> safeDecodeUtf8 message
        _ -> inp
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
