{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.Terminal.Input where

import Control.Applicative (optional, (<|>))
import Control.Concurrent (forkFinally, forkIO, killThread, mkWeakThreadId, threadDelay)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as B
import Data.Char (isAlpha, isAlphaNum, isAscii)
import Data.Either (fromRight)
import Data.List (dropWhileEnd, foldl', sort)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple (Only (..))
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.QQ (sql)
import GHC.Weak (deRefWeak)
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Styled
import Simplex.Chat.Terminal.Output
import Simplex.Chat.Types (User (..))
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import Simplex.Messaging.Util (catchAll_, safeDecodeUtf8, whenM)
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
  rh <- readTVarIO $ currentRemoteHost cc
  let bs = encodeUtf8 $ T.pack s
      cmd = parseChatCommand bs
      rh' = if either (const False) allowRemoteCommand cmd then rh else Nothing
  unless (isMessage cmd) $ echo s
  r <- runReaderT (execChatCommand rh' bs) cc
  processResp s cmd rh r
  printRespToTerminal ct cc False rh r
  startLiveMessage cmd r
  where
    echo s = printToTerminal ct [plain s]
    processResp s cmd rh = \case
      CRActiveUser u -> case rh of
        Nothing -> setActive ct ""
        Just rhId -> updateRemoteUser ct u rhId
      CRChatItems u chatName_ _ -> whenCurrUser cc u $ mapM_ (setActive ct . chatActiveTo) chatName_
      CRNewChatItem u (AChatItem _ SMDSnd cInfo _) -> whenCurrUser cc u $ setActiveChat ct cInfo
      CRChatItemUpdated u (AChatItem _ SMDSnd cInfo _) -> whenCurrUser cc u $ setActiveChat ct cInfo
      CRChatItemDeleted u (AChatItem _ _ cInfo _) _ _ _ -> whenCurrUser cc u $ setActiveChat ct cInfo
      CRContactDeleted u c -> whenCurrUser cc u $ unsetActiveContact ct c
      CRGroupDeletedUser u g -> whenCurrUser cc u $ unsetActiveGroup ct g
      CRSentGroupInvitation u g _ _ -> whenCurrUser cc u $ setActiveGroup ct g
      CRChatCmdError _ _ -> when (isMessage cmd) $ echo s
      CRChatError _ _ -> when (isMessage cmd) $ echo s
      CRCmdOk _ -> case cmd of
        Right APIDeleteUser {} -> setActive ct ""
        _ -> pure ()
      _ -> pure ()
    isMessage = \case
      Right SendMessage {} -> True
      Right SendLiveMessage {} -> True
      Right SendFile {} -> True
      Right SendMessageQuote {} -> True
      Right ForwardMessage {} -> True
      Right ForwardLocalMessage {} -> True
      Right SendGroupMessageQuote {} -> True
      Right ForwardGroupMessage {} -> True
      Right SendMessageBroadcast {} -> True
      _ -> False
    startLiveMessage :: Either a ChatCommand -> ChatResponse -> IO ()
    startLiveMessage (Right (SendLiveMessage chatName msg)) (CRNewChatItem _ (AChatItem cType SMDSnd _ ChatItem {meta = CIMeta {itemId}})) = do
      whenM (isNothing <$> readTVarIO liveMessageState) $ do
        let s = T.unpack msg
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
  let cmd = UpdateLiveMessage chatName chatItemId live $ T.pack sentMsg
  either (CRChatCmdError Nothing) id <$> runExceptT (processChatCommand cmd) `runReaderT` cc

runTerminalInput :: ChatTerminal -> ChatController -> IO ()
runTerminalInput ct cc = withChatTerm ct $ do
  updateInput ct
  receiveFromTTY cc ct

receiveFromTTY :: forall m. MonadTerminal m => ChatController -> ChatTerminal -> m ()
receiveFromTTY cc@ChatController {inputQ, currentUser, currentRemoteHost, chatStore} ct@ChatTerminal {termSize, termState, liveMessageState, activeTo} =
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
    submit live = do
      ts <- readTVarIO termState
      isLive <- isJust <$> readTVarIO liveMessageState
      when (inputString ts /= "" || isLive) $
        atomically (submitInput live ts) >>= mapM_ (uncurry endLiveMessage)
    update key = do
      chatPrefix <- readTVarIO activeTo
      live <- isJust <$> readTVarIO liveMessageState
      ts <- readTVarIO termState
      user_ <- readTVarIO currentUser
      ts' <- updateTermState user_ chatStore chatPrefix live (width termSize) key ts
      atomically $ writeTVar termState $! ts'

    endLiveMessage :: String -> LiveMessage -> IO ()
    endLiveMessage sentMsg lm = do
      kill liveThreadId
      kill promptThreadId
      atomically $ writeTVar liveMessageState Nothing
      r <- sendUpdatedLiveMessage cc sentMsg lm False
      rh <- readTVarIO currentRemoteHost -- XXX: should be inherited from live message state
      printRespToTerminal ct cc False rh r
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
        ts' = ts {inputString = "", inputPosition = 0, autoComplete = mkAutoComplete}

data AutoComplete
  = ACContact Text
  | ACContactRequest Text
  | ACMember Text Text
  | ACGroup Text
  | ACCommand Text
  | ACNone

updateTermState :: Maybe User -> SQLiteStore -> String -> Bool -> Int -> (Key, Modifiers) -> TerminalState -> IO TerminalState
updateTermState user_ st chatPrefix live tw (key, ms) ts@TerminalState {inputString = s, inputPosition = p, autoComplete = acp} = case key of
  CharKey c
    | ms == mempty || ms == shiftKey -> pure $ insertChars $ charsWithContact [c]
    | ms == altKey && c == 'b' -> pure $ setPosition prevWordPos
    | ms == altKey && c == 'f' -> pure $ setPosition nextWordPos
    | otherwise -> pure ts
  TabKey -> do
    (pfx, vs) <- autoCompleteVariants user_
    let sv = acShowVariants acp
        sv'
          | not (acTabPressed acp) = if null pfx || sv /= SVNone then SVSome else SVNone
          | sv == SVNone = SVSome
          | sv == SVSome && length vs > 4 = SVAll
          | otherwise = SVNone
        acp' = acp {acVariants = vs, acInputString = s, acShowVariants = sv', acTabPressed = True}
    pure $ (insertChars pfx) {autoComplete = acp'}
  BackspaceKey -> pure backDeleteChar
  DeleteKey -> pure deleteChar
  HomeKey -> pure $ setPosition 0
  EndKey -> pure $ setPosition $ length s
  ArrowKey d -> pure $ case d of
    Leftwards -> setPosition leftPos
    Rightwards -> setPosition rightPos
    Upwards
      | ms == mempty && null s -> let s' = upArrowCmd $ previousInput ts in ts' (s', length s')
      | ms == mempty -> let p' = p - tw in if p' > 0 then setPosition p' else ts
      | otherwise -> ts
    Downwards
      | ms == mempty -> let p' = p + tw in if p' <= length s then setPosition p' else ts
      | otherwise -> ts
  _ -> pure ts
  where
    autoCompleteVariants Nothing = pure ("", [charsWithContact "    "])
    autoCompleteVariants (Just User {userId, userContactId}) =
      getAutoCompleteChars $ fromRight ACNone $ A.parseOnly autoCompleteP $ encodeUtf8 $ T.pack s
      where
        autoCompleteP =
          A.choice
            [ ACContact <$> (contactPfx *> displayName <* A.endOfInput),
              ACContactRequest <$> (contactReqPfx *> displayName <* A.endOfInput),
              ACMember <$> (groupMemberPfx *> displayName) <* A.space <* optional (A.char '@') <*> displayName <* A.endOfInput,
              ACGroup <$> (groupPfx *> displayName <* A.endOfInput),
              ACCommand . safeDecodeUtf8 <$> ((<>) <$> ("/" *> alphaP) <*> (B.cons <$> A.space <*> alphaP <|> "")) <* A.endOfInput
            ]
        displayName = safeDecodeUtf8 <$> (B.cons <$> A.satisfy refChar <*> A.takeTill (== ' ') <|> "")
        refChar c = c > ' ' && c /= '#' && c /= '@'
        alphaP = A.takeWhile $ \c -> isAscii c && isAlpha c
        contactPfx =
          A.choice $
            ops '@' [">>", ">", "!", "\\"]
              <> cmd '@' ["t", "tail", "?", "search", "set voice", "set delete", "set disappear"]
              <> cmd_ '@' ["i ", "info ", "f ", "file ", "clear", "d ", "delete ", "code ", "verify "]
              <> ["@"]
        contactReqPfx = A.choice $ cmd_ '@' ["ac", "accept", "rc", "reject"]
        groupPfx =
          A.choice $
            ops '#' [">", "!", "\\\\", "\\"]
              <> cmd '#' ["t", "tail", "?", "search", "i", "info", "f", "file", "clear", "d", "delete", "code", "verify", "set voice", "set delete", "set disappear", "set direct"]
              <> cmd_ '#' ["a", "add", "j", "join", "rm", "remove", "l", "leave", "ms", "members", "mr", "member role"]
              <> ["#"]
        groupMemberPfx =
          A.choice $
            ops '#' [">", "\\\\"]
              <> cmd '#' ["i", "info", "code", "verify"]
              <> cmd_ '#' ["rm", "remove", "l", "leave", "mr", "member role"]
        ops c = map (<* (optional A.space <* A.char c))
        cmd c = map $ \t -> A.char '/' *> t <* A.space <* A.char c
        cmd_ c = map $ \t -> A.char '/' *> t <* A.space <* optional (A.char c)
        getAutoCompleteChars = \case
          ACContact pfx -> common pfx <$> getContactSfxs pfx
          ACContactRequest pfx -> common pfx <$> getNameSfxs "contact_requests" pfx
          ACGroup pfx -> common pfx <$> getNameSfxs "groups" pfx
          ACMember gName pfx -> common pfx <$> getMemberNameSfxs gName pfx
          ACCommand pfx -> pure $ second (map ('/' :)) $ common pfx $ hasPfx pfx commands
          ACNone -> pure ("", [charsWithContact ""])
          where
            getMemberNameSfxs gName pfx =
              getNameSfxs_
                pfx
                (userId, userContactId, gName, pfx <> "%")
                [sql|
                  SELECT m.local_display_name
                  FROM group_members m
                  JOIN groups g USING (group_id)
                  WHERE g.user_id = ?
                    AND (m.contact_id IS NULL OR m.contact_id != ?)
                    AND g.local_display_name = ?
                    AND m.local_display_name LIKE ?
                |]
            getContactSfxs pfx =
              getNameSfxs_
                pfx
                (userId, pfx <> "%")
                "SELECT local_display_name FROM contacts WHERE is_user = 0 AND user_id = ? AND local_display_name LIKE ?"
            getNameSfxs table pfx =
              getNameSfxs_ pfx (userId, pfx <> "%") $
                "SELECT local_display_name FROM " <> table <> " WHERE user_id = ? AND local_display_name LIKE ?"
            getNameSfxs_ :: SQL.ToRow p => Text -> p -> SQL.Query -> IO [String]
            getNameSfxs_ pfx ps q =
              withTransaction st (\db -> hasPfx pfx . map fromOnly <$> DB.query db q ps) `catchAll_` pure []
            commands =
              ["connect", "search", "tail", "info", "clear", "delete", "code", "verify"]
                <> ["file", "freceive", "fcancel", "fstatus", "fforward", "image", "image_forward"]
                <> ["address", "delete_address", "show_address", "auto_accept", "accept @", "reject @"]
                <> ["group", "groups", "members #", "member role #", "add #", "join #", "remove #", "leave #"]
                <> ["create link #", "set link role #", "delete link #", "show link #"]
                <> ["set voice", "set delete", "set direct #", "set disappear", "mute", "unmute"]
                <> ["create user", "profile", "users", "user", "mute user", "unmute user", "hide user", "unhide user", "delete user"]
                <> ["chats", "contacts", "help", "markdown", "quit", "db export", "db encrypt", "db decrypt", "db key"]
            hasPfx pfx = map T.unpack . sort . filter (pfx `T.isPrefixOf`)
            common pfx xs = (commonPrefix $ map (drop $ T.length pfx) xs, xs)
    commonPrefix = \case
      x : xs -> foldl go x xs
      _ -> ""
      where
        go (c : cs) (c' : cs')
          | c == c' = c : go cs cs'
          | otherwise = ""
        go _ _ = ""
    charsWithContact cs
      | live = cs
      | null s && cs /= "@" && cs /= "#" && cs /= "/" && cs /= ">" && cs /= "\\" && cs /= "!" && cs /= "+" && cs /= "-" =
          chatPrefix <> cs
      | (s == ">" || s == "\\" || s == "!") && cs == " " =
          cs <> chatPrefix
      | otherwise = cs
    insertChars = ts' . if p >= length s then append else insert
    append cs = let s' = s <> cs in (s', length s')
    insert cs = let (b, a) = splitAt p s in (b <> cs <> a, p + length cs)
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
        SendMessageQuote {contactName, message} -> T.unpack $ "! @" <> contactName <> " " <> message
        SendGroupMessageQuote {groupName, message} -> T.unpack $ "! #" <> groupName <> " " <> message
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
    ts' (s', p') = ts {inputString = s', inputPosition = p', autoComplete = acp {acTabPressed = False}}
