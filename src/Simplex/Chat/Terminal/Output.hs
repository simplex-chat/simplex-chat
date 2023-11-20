{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Terminal.Output where

import Control.Concurrent (ThreadId)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Simplex.Chat (processChatCommand)
import Simplex.Chat.Controller
import Simplex.Chat.Markdown
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent (CIContent(..), SMsgDirection (..))
import Simplex.Chat.Options
import Simplex.Chat.Protocol (MsgContent (..), msgContentText)
import Simplex.Chat.Remote.Types (RemoteHostId)
import Simplex.Chat.Styled
import Simplex.Chat.Terminal.Notification (Notification (..), initializeNotifications)
import Simplex.Chat.Types
import Simplex.Chat.View
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util (safeDecodeUtf8)
import System.Console.ANSI.Types
import System.IO (IOMode (..), hPutStrLn, withFile)
import System.Mem.Weak (Weak)
import System.Terminal
import System.Terminal.Internal (LocalTerminal, Terminal, VirtualTerminal)
import UnliftIO.STM

data ChatTerminal = ChatTerminal
  { termDevice :: TerminalDevice,
    termState :: TVar TerminalState,
    termSize :: Size,
    liveMessageState :: TVar (Maybe LiveMessage),
    nextMessageRow :: TVar Int,
    termLock :: TMVar (),
    sendNotification :: Maybe (Notification -> IO ()),
    activeTo :: TVar String
  }

data TerminalState = TerminalState
  { inputPrompt :: String,
    inputString :: String,
    inputPosition :: Int,
    previousInput :: String,
    autoComplete :: AutoCompleteState
  }

data ACShowVariants = SVNone | SVSome | SVAll
  deriving (Eq, Enum)

data AutoCompleteState = ACState
  { acVariants :: [String],
    acInputString :: String,
    acTabPressed :: Bool,
    acShowVariants :: ACShowVariants
  }

data LiveMessage = LiveMessage
  { chatName :: ChatName,
    chatItemId :: ChatItemId,
    livePrompt :: Bool,
    sentMsg :: String,
    typedMsg :: String,
    liveThreadId :: Weak ThreadId,
    promptThreadId :: Weak ThreadId
  }

class Terminal t => WithTerminal t where
  withTerm :: (MonadIO m, MonadMask m) => t -> (t -> m a) -> m a

data TerminalDevice = forall t. WithTerminal t => TerminalDevice t

instance WithTerminal LocalTerminal where
  withTerm _ = withTerminal

instance WithTerminal VirtualTerminal where
  withTerm t = ($ t)

withChatTerm :: (MonadIO m, MonadMask m) => ChatTerminal -> (forall t. WithTerminal t => TerminalT t m a) -> m a
withChatTerm ChatTerminal {termDevice = TerminalDevice t} action = withTerm t $ runTerminalT action

newChatTerminal :: WithTerminal t => t -> ChatOpts -> IO ChatTerminal
newChatTerminal t opts = do
  termSize <- withTerm t . runTerminalT $ getWindowSize
  let lastRow = height termSize - 1
  termState <- newTVarIO mkTermState
  liveMessageState <- newTVarIO Nothing
  termLock <- newTMVarIO ()
  nextMessageRow <- newTVarIO lastRow
  sendNotification <- if muteNotifications opts then pure Nothing else Just <$> initializeNotifications
  activeTo <- newTVarIO ""
  -- threadDelay 500000 -- this delay is the same as timeout in getTerminalSize
  pure
    ChatTerminal
      { termDevice = TerminalDevice t,
        termState,
        termSize,
        liveMessageState,
        nextMessageRow,
        termLock,
        sendNotification,
        activeTo
      }

mkTermState :: TerminalState
mkTermState =
  TerminalState
    { inputString = "",
      inputPosition = 0,
      inputPrompt = "> ",
      previousInput = "",
      autoComplete = mkAutoComplete
    }

mkAutoComplete :: AutoCompleteState
mkAutoComplete = ACState {acVariants = [], acInputString = "", acTabPressed = False, acShowVariants = SVNone}

withTermLock :: MonadTerminal m => ChatTerminal -> m () -> m ()
withTermLock ChatTerminal {termLock} action = do
  _ <- atomically $ takeTMVar termLock
  action
  atomically $ putTMVar termLock ()

runTerminalOutput :: ChatTerminal -> ChatController -> IO ()
runTerminalOutput ct cc@ChatController {outputQ, showLiveItems, logFilePath} = do
  forever $ do
    (_, outputRH, r) <- atomically $ readTBQueue outputQ
    case r of
      CRNewChatItem u ci -> markChatItemRead u ci
      CRChatItemUpdated u ci -> markChatItemRead u ci
      _ -> pure ()
    let printResp = case logFilePath of
          Just path -> if logResponseToFile r then logResponse path else printToTerminal ct
          _ -> printToTerminal ct
    liveItems <- readTVarIO showLiveItems
    responseString cc liveItems outputRH r >>= printResp
    responseNotification ct cc r
  where
    markChatItemRead u (AChatItem _ _ chat ci@ChatItem {chatDir, meta = CIMeta {itemStatus}}) =
      case (chatDirNtf u chat chatDir (isMention ci), itemStatus) of
        (True, CISRcvNew) -> do
          let itemId = chatItemId' ci
              chatRef = chatInfoToRef chat
          void $ runReaderT (runExceptT $ processChatCommand (APIChatRead chatRef (Just (itemId, itemId)))) cc
        _ -> pure ()
    logResponse path s = withFile path AppendMode $ \h -> mapM_ (hPutStrLn h . unStyle) s

responseNotification :: ChatTerminal -> ChatController -> ChatResponse -> IO ()
responseNotification t@ChatTerminal {sendNotification} cc = \case
  CRNewChatItem u (AChatItem _ SMDRcv cInfo ci@ChatItem {chatDir, content = CIRcvMsgContent mc, formattedText}) ->
    when (chatDirNtf u cInfo chatDir $ isMention ci) $ do
      whenCurrUser cc u $ setActiveChat t cInfo
      case (cInfo, chatDir) of
        (DirectChat ct, _) -> sendNtf (viewContactName ct <> "> ", text)
        (GroupChat g, CIGroupRcv m) -> sendNtf (fromGroup_ g m, text)
        _ -> pure ()
    where
      text = msgText mc formattedText
  CRChatItemUpdated u (AChatItem _ SMDRcv cInfo ci@ChatItem {chatDir, content = CIRcvMsgContent _}) ->
    whenCurrUser cc u $ when (chatDirNtf u cInfo chatDir $ isMention ci) $ setActiveChat t cInfo
  CRContactConnected u ct _ -> when (contactNtf u ct False) $ do
    whenCurrUser cc u $ setActiveContact t ct
    sendNtf (viewContactName ct <> "> ", "connected")
  CRContactAnotherClient u ct -> do
    whenCurrUser cc u $ unsetActiveContact t ct
    when (contactNtf u ct False) $ sendNtf (viewContactName ct <> "> ", "connected to another client")
  CRContactsDisconnected srv _ -> serverNtf srv "disconnected"
  CRContactsSubscribed srv _ -> serverNtf srv "connected"
  CRReceivedGroupInvitation u g ct _ _ ->
    when (contactNtf u ct False) $
      sendNtf ("#" <> viewGroupName g <> " " <> viewContactName ct <> "> ", "invited you to join the group")
  CRUserJoinedGroup u g _ -> when (groupNtf u g False) $ do
    whenCurrUser cc u $ setActiveGroup t g
    sendNtf ("#" <> viewGroupName g, "you are connected to group")
  CRJoinedGroupMember u g m ->
    when (groupNtf u g False) $ sendNtf ("#" <> viewGroupName g, "member " <> viewMemberName m <> " is connected")
  CRConnectedToGroupMember u g m _ ->
    when (groupNtf u g False) $ sendNtf ("#" <> viewGroupName g, "member " <> viewMemberName m <> " is connected")
  CRReceivedContactRequest u UserContactRequest {localDisplayName = n} ->
    when (userNtf u) $ sendNtf (viewName n <> ">", "wants to connect to you")
  _ -> pure ()
  where
    sendNtf = maybe (\_ -> pure ()) (. uncurry Notification) sendNotification
    serverNtf (SMPServer host _ _) str = sendNtf ("server " <> str, safeDecodeUtf8 $ strEncode host)

msgText :: MsgContent -> Maybe MarkdownList -> Text
msgText (MCFile _) _ = "wants to send a file"
msgText mc md_ = maybe (msgContentText mc) (mconcat . map hideSecret) md_
  where
    hideSecret :: FormattedText -> Text
    hideSecret FormattedText {format = Just Secret} = "..."
    hideSecret FormattedText {text} = text

chatActiveTo :: ChatName -> String
chatActiveTo (ChatName cType name) = case cType of
  CTDirect -> T.unpack $ "@" <> viewName name <> " "
  CTGroup -> T.unpack $ "#" <> viewName name <> " "
  _ -> ""

chatInfoActiveTo :: ChatInfo c -> String
chatInfoActiveTo = \case
  DirectChat c -> contactActiveTo c
  GroupChat g -> groupActiveTo g
  _ -> ""

contactActiveTo :: Contact -> String
contactActiveTo c = T.unpack $ "@" <> viewContactName c <> " "

groupActiveTo :: GroupInfo -> String
groupActiveTo g = T.unpack $ "#" <> viewGroupName g <> " "

setActiveChat :: ChatTerminal -> ChatInfo c -> IO ()
setActiveChat t = setActive t . chatInfoActiveTo

setActiveContact :: ChatTerminal -> Contact -> IO ()
setActiveContact t = setActive t . contactActiveTo

setActiveGroup :: ChatTerminal -> GroupInfo -> IO ()
setActiveGroup t = setActive t . groupActiveTo

setActive :: ChatTerminal -> String -> IO ()
setActive ChatTerminal {activeTo} to = atomically $ writeTVar activeTo to

unsetActiveContact :: ChatTerminal -> Contact -> IO ()
unsetActiveContact t = unsetActive t . contactActiveTo

unsetActiveGroup :: ChatTerminal -> GroupInfo -> IO ()
unsetActiveGroup t = unsetActive t . groupActiveTo

unsetActive :: ChatTerminal -> String -> IO ()
unsetActive ChatTerminal {activeTo} to' = atomically $ modifyTVar activeTo unset
  where
    unset to = if to == to' then "" else to

whenCurrUser :: ChatController -> User -> IO () -> IO ()
whenCurrUser cc u a = do
  u_ <- readTVarIO $ currentUser cc
  when (sameUser u u_) a
  where
    sameUser User {userId = uId} = maybe False $ \User {userId} -> userId == uId

printRespToTerminal :: ChatTerminal -> ChatController -> Bool -> Maybe RemoteHostId -> ChatResponse -> IO ()
printRespToTerminal ct cc liveItems outputRH r = responseString cc liveItems outputRH r >>= printToTerminal ct

responseString :: ChatController -> Bool -> Maybe RemoteHostId -> ChatResponse -> IO [StyledString]
responseString cc liveItems outputRH r = do
  currentRH <- readTVarIO $ currentRemoteHost cc
  user <- readTVarIO $ currentUser cc -- XXX: local user, should be subsumed by remote when connected
  ts <- getCurrentTime
  tz <- getCurrentTimeZone
  pure $ responseToView (currentRH, user) (config cc) liveItems ts tz outputRH r

printToTerminal :: ChatTerminal -> [StyledString] -> IO ()
printToTerminal ct s =
  withChatTerm ct $
    withTermLock ct $ do
      printMessage ct s
      updateInput ct

updateInputView :: ChatTerminal -> IO ()
updateInputView ct = withChatTerm ct $ withTermLock ct $ updateInput ct

updateInput :: forall m. MonadTerminal m => ChatTerminal -> m ()
updateInput ChatTerminal {termSize = Size {height, width}, termState, nextMessageRow} = do
  hideCursor
  ts <- readTVarIO termState
  nmr <- readTVarIO nextMessageRow
  let ih = inputHeight ts
      iStart = height - ih
      prompt = inputPrompt ts
      acPfx = autoCompletePrefix ts
      Position {row, col} = positionRowColumn width $ length acPfx + length prompt + inputPosition ts
  if nmr >= iStart
    then atomically $ writeTVar nextMessageRow iStart
    else clearLines nmr iStart
  setCursorPosition $ Position {row = max nmr iStart, col = 0}
  putStyled $ Styled [SetColor Foreground Dull White] acPfx
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
    inputHeight :: TerminalState -> Int
    inputHeight ts = length (autoCompletePrefix ts <> inputPrompt ts <> inputString ts) `div` width + 1
    autoCompletePrefix :: TerminalState -> String
    autoCompletePrefix TerminalState {autoComplete = ac}
      | length vars <= 1 || sv == SVNone = ""
      | sv == SVAll || length vars <= 4 = "(" <> intercalate ", " vars <> ") "
      | otherwise = "(" <> intercalate ", " (take 3 vars) <> "... +" <> show (length vars - 3) <> ") "
      where
        sv = acShowVariants ac
        vars = acVariants ac
    positionRowColumn :: Int -> Int -> Position
    positionRowColumn wid pos =
      let row = pos `div` wid
          col = pos - row * wid
       in Position {row, col}

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

-- Currently it is assumed that the message does not have internal line breaks.
-- Previous implementation "kind of" supported them,
-- but it was not determining the number of printed lines correctly
-- because of accounting for control sequences in length
putStyled :: MonadTerminal m => StyledString -> m ()
putStyled (s1 :<>: s2) = putStyled s1 >> putStyled s2
putStyled (Styled [] s) = putString s
putStyled (Styled sgr s) = setSGR sgr >> putString s >> resetAttributes

setSGR :: MonadTerminal m => [SGR] -> m ()
setSGR = mapM_ $ \case
  Reset -> resetAttributes
  SetConsoleIntensity BoldIntensity -> setAttribute bold
  SetConsoleIntensity _ -> resetAttribute bold
  SetItalicized True -> setAttribute italic
  SetItalicized _ -> resetAttribute italic
  SetUnderlining NoUnderline -> resetAttribute underlined
  SetUnderlining _ -> setAttribute underlined
  SetSwapForegroundBackground True -> setAttribute inverted
  SetSwapForegroundBackground _ -> resetAttribute inverted
  SetColor l i c -> setAttribute . layer l . intensity i $ color c
  SetBlinkSpeed _ -> pure ()
  SetVisible _ -> pure ()
  SetRGBColor _ _ -> pure ()
  SetPaletteColor _ _ -> pure ()
  SetDefaultColor _ -> pure ()
  where
    layer = \case
      Foreground -> foreground
      Background -> background
    intensity = \case
      Dull -> id
      Vivid -> bright
    color = \case
      Black -> black
      Red -> red
      Green -> green
      Yellow -> yellow
      Blue -> blue
      Magenta -> magenta
      Cyan -> cyan
      White -> white
