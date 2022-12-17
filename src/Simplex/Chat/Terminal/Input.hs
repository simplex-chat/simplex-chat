{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.Terminal.Input where

import Control.Concurrent (forkFinally, mkWeakThreadId, threadDelay)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor (($>))
import Data.List (dropWhileEnd)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Messages
import Simplex.Chat.Styled
import Simplex.Chat.Terminal.Output
import Simplex.Messaging.Util (safeDecodeUtf8, whenM, (<$$>))
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
runInputLoop ct cc = forever $ do
  s <- atomically . readTBQueue $ inputQ cc
  let bs = encodeUtf8 $ T.pack s
      cmd = parseChatCommand bs
  unless (isMessage cmd) $ echo s
  r <- runReaderT (execChatCommand bs) cc
  case r of
    CRChatCmdError _ -> when (isMessage cmd) $ echo s
    _ -> pure ()
  -- let testV = testView $ config cc
  -- user <- readTVarIO $ currentUser cc
  -- ts <- getCurrentTime
  -- printToTerminal ct $ responseToView user testV False ts r
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
    startLiveMessage (Right (SendLiveMessage chatName msg)) (CRNewChatItem (AChatItem _ SMDSnd _ ChatItem {meta = CIMeta {itemId}})) = do
      whenM (isNothing <$> readTVarIO (liveMessageState ct)) $ do
        tId <- mkWeakThreadId =<< runLiveMessage `forkFinally` const (atomically $ writeTVar (liveMessageState ct) Nothing)
        let msgStr = T.unpack $ safeDecodeUtf8 msg
        atomically $ writeTVar (liveMessageState ct) (Just $ LiveMessage chatName itemId msgStr msgStr tId)
      where
        runLiveMessage :: IO ()
        runLiveMessage = do
          threadDelay 3000000
          TerminalState {inputString = s} <- readTVarIO $ termState ct
          atomically (liveMessageToSend s <$$> readTVar (liveMessageState ct))
            >>= mapM_ (uncurry (updateLiveMessage s) >=> const runLiveMessage)
        liveMessageToSend typedMsg' lm@LiveMessage {sentMsg, typedMsg} =
          (lm,) $
            if typedMsg' /= typedMsg
              then let s = truncateToWords typedMsg' in toMaybe (s /= sentMsg) s
              else toMaybe (typedMsg /= sentMsg) typedMsg
        toMaybe cond a = if cond then Just a else Nothing
        updateLiveMessage typedMsg lm = \case
          Just sentMsg ->
            sendUpdatedLiveMessage cc sentMsg lm True >>= \case
              CRChatItemUpdated {} -> setLiveMessage lm {sentMsg, typedMsg}
              _ -> do
                -- TODO print error
                setLiveMessage lm {typedMsg}
          _ -> setLiveMessage lm {typedMsg}
        setLiveMessage :: LiveMessage -> IO ()
        setLiveMessage = atomically . writeTVar (liveMessageState ct) . Just
        truncateToWords s = s
    startLiveMessage _ _ = pure ()

sendUpdatedLiveMessage :: ChatController -> String -> LiveMessage -> Bool -> IO ChatResponse
sendUpdatedLiveMessage cc sentMsg LiveMessage {chatName, chatItemId} live = do
  let bs = encodeUtf8 $ T.pack sentMsg
      cmd = UpdateLiveMessage chatName chatItemId live bs
  either CRChatCmdError id <$> runExceptT (processChatCommand cmd) `runReaderT` cc

-- let typedMsg = composeState.message
-- if let liveMessage = composeState.liveMessage {
--     if let sentMsg = liveMessageToSend(liveMessage, typedMsg),
--        let ci = await sendMessageAsync(sentMsg, live: true) {
--         await MainActor.run {
--             composeState = composeState.copy(liveMessage: LiveMessage(chatItem: ci, typedMsg: typedMsg, sentMsg: sentMsg))
--         }
--     } else if liveMessage.typedMsg != typedMsg {
--         await MainActor.run {
--             var lm = liveMessage
--             lm.typedMsg = typedMsg
--             composeState = composeState.copy(liveMessage: lm)
--         }
--     }
-- }
--
--
-- private func liveMessageToSend(_ liveMessage: LiveMessage, _ typedMsg: String) -> String? {
--     if liveMessage.typedMsg != typedMsg {
--         let s = truncateToWords(typedMsg)
--         return s == liveMessage.sentMsg ? nil : s
--     }
--     return liveMessage.changed
--     ? liveMessage.typedMsg
--     : nil
-- }
--
--
-- private func truncateToWords(_ s: String) -> String {
--     if let i = s.lastIndex(where: { !alphaNumeric($0) }) {
--         let s1 = s[...i]
--         if let j = s1.lastIndex(where: alphaNumeric), i < s1.endIndex {
--             return String(s1[...j])
--         }
--         return String(s1)
--     }
--     return ""

--     func alphaNumeric(_ c: Character) -> Bool {
--         c.isLetter || c.isNumber
--     }
-- }

runTerminalInput :: ChatTerminal -> ChatController -> IO ()
runTerminalInput ct cc = withChatTerm ct $ do
  updateInput ct
  receiveFromTTY cc ct

receiveFromTTY :: forall m. MonadTerminal m => ChatController -> ChatTerminal -> m ()
receiveFromTTY cc@ChatController {inputQ, activeTo} ct@ChatTerminal {termSize, termState, liveMessageState} =
  forever $ getKey >>= processKey >> withTermLock ct (updateInput ct)
  where
    processKey :: (Key, Modifiers) -> m ()
    processKey = \case
      (EnterKey, _) ->
        atomically submitInput >>= mapM_ (liftIO . uncurry endLiveMessage)
      key -> atomically $ do
        ac <- readTVar activeTo
        modifyTVar termState $ updateTermState ac (width termSize) key

    endLiveMessage :: String -> LiveMessage -> IO ()
    endLiveMessage sentMsg lm =
      sendUpdatedLiveMessage cc sentMsg lm False
        >>= printRespToTerminal ct cc False

    submitInput :: STM (Maybe (String, LiveMessage))
    submitInput = do
      ts <- readTVar termState
      let s = inputString ts
      writeTVar termState $ ts {inputString = "", inputPosition = 0, previousInput = s, inputPrompt = "> "}
      readTVar liveMessageState >>= \case
        Just lm -> writeTVar liveMessageState Nothing $> Just (s, lm)
        _ -> writeTBQueue inputQ s $> Nothing

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
      | ms == mempty && null s -> let s' = upArrowCmd $ previousInput ts in ts' (s', length s')
      | ms == mempty -> let p' = p - tw in if p' > 0 then setPosition p' else ts
      | otherwise -> ts
    Downwards
      | ms == mempty -> let p' = p + tw in if p' <= length s then setPosition p' else ts
      | otherwise -> ts
  _ -> ts
  where
    insertCharsWithContact cs
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
