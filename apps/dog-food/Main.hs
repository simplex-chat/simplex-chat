{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ChatOptions
import ChatTerminal
import ChatTerminal.Core
import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Control.Logger.Simple
import Control.Monad.Reader
    ( MonadIO(liftIO), ReaderT(runReaderT), forever )
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Functor (($>))
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock (DiffTime, UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
import Notification
import Numeric.Natural
import Simplex.Chat.Markdown
import Simplex.Messaging.Agent (getSMPAgentClient, runSMPAgentClient)
import Simplex.Messaging.Agent.Client (AgentClient (..))
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Client (smpDefaultConfig)
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (raceAny_)
import Styled
import System.Console.ANSI.Types
import System.Directory (getAppUserDataDirectory)
import Types

cfg :: AgentConfig
cfg =
  AgentConfig
    { tcpPort = undefined, -- agent does not listen to TCP
      smpServers = undefined, -- filled in from options
      rsaKeySize = 2048 `div` 8,
      connIdBytes = 12,
      tbqSize = 16,
      dbFile = "smp-chat.db",
      smpCfg = smpDefaultConfig
    }

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}

data ChatClient = ChatClient
  { inQ :: TBQueue ChatCommand,
    outQ :: TBQueue ChatResponse,
    notifyQ :: TBQueue Notification,
    displayNotification :: Notification -> IO ()
  }

-- | GroupMessage ChatGroup ByteString
-- | AddToGroup Contact
data ChatCommand
  = ChatHelp
  | MarkdownHelp
  | AddConnection Contact
  | Connect Contact SMPQueueInfo
  | DeleteConnection Contact
  | SendMessage Contact ByteString
  | NewGroup Group
  | AddToGroup Group Contact
  | RemoveFromGroup Group Contact
  | DeleteGroup Group
  | ListGroup Group
  | SendGroupMessage Group ByteString
  deriving (Show)

chatCommandP :: Parser ChatCommand
chatCommandP =
  ("/help" <|> "/h") $> ChatHelp
    <|> ("/group #" <|> "/g #") *> (NewGroup <$> group)
    <|> ("/add #" <|> "/a #") *> (AddToGroup <$> group <* A.space <*> contact)
    <|> ("/remove #" <|> "/rm #") *> (RemoveFromGroup <$> group <* A.space <*> contact)
    <|> ("/delete #" <|> "/d #") *> (DeleteGroup <$> group)
    <|> ("/members #" <|> "/ms #") *> (ListGroup <$> group)
    <|> A.char '#' *> (SendGroupMessage <$> group <* A.space <*> A.takeByteString)
    <|> ("/add " <|> "/a ") *> (AddConnection <$> contact)
    <|> ("/connect " <|> "/c ") *> (Connect <$> contact <* A.space <*> smpQueueInfoP)
    <|> ("/delete " <|> "/d ") *> (DeleteConnection <$> contact)
    <|> A.char '@' *> (SendMessage <$> contact <* A.space <*> A.takeByteString)
    <|> ("/markdown" <|> "/m") $> MarkdownHelp
  where
    contact = Contact <$> A.takeTill (== ' ')
    group = Group <$> A.takeTill (== ' ')

data ChatResponse
  = ChatHelpInfo
  | MarkdownInfo
  | Invitation SMPQueueInfo
  | Connected Contact
  | Confirmation Contact
  | ReceivedMessage Contact UTCTime ByteString MsgIntegrity
  | Disconnected Contact
  | GroupMembers Group [Contact]
  | ReceivedGroupMessage Group Contact UTCTime ByteString MsgIntegrity
  | GroupConfirmation Group
  | YesYes
  | ContactError ConnectionErrorType Contact
  | GroupError AgentErrorType Group
  | ErrorInput ByteString
  | ChatError AgentErrorType
  | NoChatResponse

serializeChatResponse :: ChatOpts -> TimeZone -> ZonedTime -> ChatResponse -> [StyledString]
serializeChatResponse _ localTz currentTime = \case
  ChatHelpInfo -> chatHelpInfo
  MarkdownInfo -> markdownInfo
  Invitation qInfo ->
    [ "pass this invitation to your contact (via any channel): ",
      "",
      (bPlain . serializeSmpQueueInfo) qInfo,
      "",
      "and ask them to connect: /c <name_for_you> <invitation_above>"
    ]
  Connected c -> [ttyContact c <> " connected"]
  Confirmation c -> [ttyContact c <> " ok"]
  ReceivedMessage c utcTime t mi -> receivedMessage utcTime t mi $ ttyFromContact c
  ReceivedGroupMessage g c utcTime t mi -> receivedMessage utcTime t mi $ ttyFromGroup g c
  Disconnected c -> ["disconnected from " <> ttyContact c <> " - restart chat"]
  GroupMembers g cs -> [ttyGroup g <> ": " <> plain (B.unpack . B.intercalate ", " $ map toBs cs)]
  GroupConfirmation g -> [ttyGroup g <> " ok"]
  YesYes -> ["you got it!"]
  ContactError e c -> case e of
    NOT_FOUND -> ["no contact " <> ttyContact c]
    DUPLICATE -> ["contact " <> ttyContact c <> " already exists"]
    SIMPLEX -> ["contact " <> ttyContact c <> " did not accept invitation yet"]
  GroupError e g -> case e of
    BCAST B_NOT_FOUND -> ["no group " <> ttyGroup g]
    BCAST B_DUPLICATE -> ["group " <> ttyGroup g <> " already exists"]
    CONN NOT_FOUND -> ["cannot add unknown contact to the group " <> ttyGroup g]
    CONN DUPLICATE -> ["this contact is already in the group " <> ttyGroup g]
    CONN SIMPLEX -> ["this contact did not not accept invitation yet"]
    _ -> ["chat error: " <> plain (show e)]
  ErrorInput t -> ["invalid input: " <> bPlain t]
  ChatError e -> ["chat error: " <> plain (show e)]
  NoChatResponse -> [""]
  where
    receivedMessage :: UTCTime -> ByteString -> MsgIntegrity -> StyledString -> [StyledString]
    receivedMessage utcTime t mi from =
      prependFirst (formatUTCTime utcTime <> " " <> from) (msgPlain t) ++ showIntegrity mi
    prependFirst :: StyledString -> [StyledString] -> [StyledString]
    prependFirst s [] = [s]
    prependFirst s (s' : ss) = (s <> s') : ss
    formatUTCTime :: UTCTime -> StyledString
    formatUTCTime utcTime = do
      let localTime = utcToLocalTime localTz utcTime
          format =
            if (localDay localTime < localDay (zonedTimeToLocalTime currentTime))
              && (timeOfDayToTime (localTimeOfDay localTime) > (6 * 60 * 60 :: DiffTime))
              then "%m-%d" -- if message is from yesterday or before and 6 hours has passed since midnight
              else "%H:%M"
      styleTime $ formatTime defaultTimeLocale format localTime
    msgPlain :: ByteString -> [StyledString]
    msgPlain = map styleMarkdownText . T.lines . safeDecodeUtf8
    showIntegrity :: MsgIntegrity -> [StyledString]
    showIntegrity MsgOk = []
    showIntegrity (MsgError err) = msgError $ case err of
      MsgSkipped fromId toId ->
        "skipped message ID " <> show fromId
          <> if fromId == toId then "" else ".." <> show toId
      MsgBadId msgId -> "unexpected message ID " <> show msgId
      MsgBadHash -> "incorrect message hash"
      MsgDuplicate -> "duplicate message ID"
    msgError :: String -> [StyledString]
    msgError s = [styled (Colored Red) s]

chatHelpInfo :: [StyledString]
chatHelpInfo =
  map
    styleMarkdown
    [ Markdown (Colored Cyan) "Using Simplex chat prototype.",
      "Follow these steps to set up a connection:",
      "",
      Markdown (Colored Green) "Step 1: " <> Markdown (Colored Cyan) "/add bob" <> " -- Alice adds her contact, Bob (she can use any name).",
      indent <> "Alice should send the invitation printed by the /add command",
      indent <> "to her contact, Bob, out-of-band, via any trusted channel.",
      "",
      Markdown (Colored Green) "Step 2: " <> Markdown (Colored Cyan) "/connect alice <invitation>" <> " -- Bob accepts the invitation.",
      indent <> "Bob also can use any name for his contact, Alice,",
      indent <> "followed by the invitation he received out-of-band.",
      "",
      Markdown (Colored Green) "Step 3: " <> "Bob and Alice are notified that the connection is set up,",
      indent <> "both can now send messages:",
      indent <> Markdown (Colored Cyan) "@bob Hello, Bob!" <> " -- Alice messages Bob.",
      indent <> Markdown (Colored Cyan) "@alice Hey, Alice!" <> " -- Bob replies to Alice.",
      "",
      Markdown (Colored Green) "Other commands:",
      indent <> Markdown (Colored Cyan) "/delete" <> " -- deletes contact and all messages with them.",
      indent <> Markdown (Colored Cyan) "/markdown" <> " -- prints the supported markdown syntax.",
      "",
      "The commands may be abbreviated to a single letter: " <> listCommands ["/a", "/c", "/d", "/m"]
    ]
  where
    listCommands = mconcat . intersperse ", " . map highlight
    highlight = Markdown (Colored Cyan)
    indent = "        "

markdownInfo :: [StyledString]
markdownInfo =
  map
    styleMarkdown
    [ "Markdown:",
      "  *bold*          - " <> Markdown Bold "bold text",
      "  _italic_        - " <> Markdown Italic "italic text" <> " (shown as underlined)",
      "  +underlined+    - " <> Markdown Underline "underlined text",
      "  ~strikethrough~ - " <> Markdown StrikeThrough "strikethrough text" <> " (shown as inverse)",
      "  `code snippet`  - " <> Markdown Snippet "a + b // no *markdown* here",
      "  !1 text!        - " <> red "red text" <> " (1-6: red, green, blue, yellow, cyan, magenta)",
      "  #secret#        - " <> Markdown Secret "secret text" <> " (can be copy-pasted)"
    ]
  where
    red = Markdown (Colored Red)

main :: IO ()
main = do
  opts@ChatOpts {dbFile, smpServers, termMode} <- welcomeGetOpts
  t <- newChatClient (tbqSize cfg)
  ct <- newChatTerminal (tbqSize cfg) termMode
  -- setLogLevel LogInfo -- LogError
  -- withGlobalLogging logCfg $ do
  env <- newSMPAgentEnv cfg {dbFile, smpServers}
  dogFoodChat t ct env opts

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {dbFile} <- getChatOpts appDir
  putStrLn "SimpleX chat prototype v0.3.1"
  putStrLn $ "db: " <> dbFile
  putStrLn "type \"/help\" or \"/h\" for usage info"
  pure opts

dogFoodChat :: ChatClient -> ChatTerminal -> Env -> ChatOpts -> IO ()
dogFoodChat t ct env opts = do
  c <- runReaderT getSMPAgentClient env
  localTz <- liftIO getCurrentTimeZone
  raceAny_
    [ runReaderT (runSMPAgentClient c) env,
      sendToAgent t ct c,
      sendToChatTerm t ct opts localTz,
      receiveFromAgent t ct c,
      receiveFromChatTerm t ct,
      showNotifications t,
      chatTerminal ct
    ]

newChatClient :: Natural -> IO ChatClient
newChatClient qSize = do
  inQ <- newTBQueueIO qSize
  outQ <- newTBQueueIO qSize
  notifyQ <- newTBQueueIO qSize
  displayNotification <- initializeNotifications
  return ChatClient {inQ, outQ, notifyQ, displayNotification}

receiveFromChatTerm :: ChatClient -> ChatTerminal -> IO ()
receiveFromChatTerm t ct = forever $ do
  atomically (readTBQueue $ inputQ ct)
    >>= processOrError . parseAll chatCommandP . encodeUtf8 . T.pack
  where
    processOrError = \case
      Left err -> writeOutQ . ErrorInput $ B.pack err
      Right ChatHelp -> writeOutQ ChatHelpInfo
      Right MarkdownHelp -> writeOutQ MarkdownInfo
      Right cmd -> atomically $ writeTBQueue (inQ t) cmd
    writeOutQ = atomically . writeTBQueue (outQ t)

sendToChatTerm :: ChatClient -> ChatTerminal -> ChatOpts -> TimeZone -> IO ()
sendToChatTerm ChatClient {outQ} ChatTerminal {outputQ} opts localTz = forever $ do
  atomically (readTBQueue outQ) >>= \case
    NoChatResponse -> return ()
    resp -> do
      currentTime <- liftIO getZonedTime
      atomically . writeTBQueue outputQ $ serializeChatResponse opts localTz currentTime resp

sendToAgent :: ChatClient -> ChatTerminal -> AgentClient -> IO ()
sendToAgent ChatClient {inQ} ct AgentClient {rcvQ} = do
  atomically $ writeTBQueue rcvQ $ ATransmission "1" (Conn "") SUBALL -- hack for subscribing to all
  forever . atomically $ do
    cmd <- readTBQueue inQ
    writeTBQueue rcvQ `mapM_` agentTransmission cmd
    setActiveTo cmd
  where
    setActiveTo :: ChatCommand -> STM ()
    setActiveTo = \case
      SendMessage a _ -> setActive ct $ ActiveC a
      SendGroupMessage g _ -> setActive ct $ ActiveG g
      DeleteConnection a -> unsetActive ct $ ActiveC a
      DeleteGroup g -> unsetActive ct $ ActiveG g
      _ -> pure ()
    agentTransmission :: ChatCommand -> Maybe (ATransmission 'Client)
    agentTransmission = \case
      AddConnection a -> transmission a NEW
      Connect a qInfo -> transmission a $ JOIN qInfo $ ReplyMode On
      DeleteConnection a -> transmission a DEL
      SendMessage a msg -> transmission a $ SEND msg
      NewGroup g -> bTransmission g NEW
      AddToGroup g a -> bTransmission g $ ADD (Conn $ toBs a)
      RemoveFromGroup g a -> bTransmission g $ REM (Conn $ toBs a)
      DeleteGroup g -> bTransmission g DEL
      ListGroup g -> bTransmission g LS
      SendGroupMessage g msg -> bTransmission g $ SEND $ serializeGroupMessage g msg
      ChatHelp -> Nothing
      MarkdownHelp -> Nothing
    transmission :: EntityCommand 'Conn_ c => Contact -> ACommand 'Client c -> Maybe (ATransmission 'Client)
    transmission (Contact a) cmd = Just $ ATransmission "1" (Conn a) cmd
    bTransmission :: EntityCommand 'Broadcast_ c => Group -> ACommand 'Client c -> Maybe (ATransmission 'Client)
    bTransmission (Group g) cmd = Just $ ATransmission "1" (Broadcast g) cmd

receiveFromAgent :: ChatClient -> ChatTerminal -> AgentClient -> IO ()
receiveFromAgent t ct c = forever . atomically $ do
  resp <- chatResponse <$> readTBQueue (sndQ c)
  writeTBQueue (outQ t) resp
  setActiveTo resp
  sendNotification resp
  where
    chatResponse :: ATransmission 'Agent -> ChatResponse
    chatResponse (ATransmission _ entity resp) = case entity of
      Conn a -> connectionResponse a resp
      Broadcast g -> broadcastResponse g resp
      _ -> NoChatResponse
    connectionResponse :: EntityCommand 'Conn_ c => ByteString -> ACommand 'Agent c -> ChatResponse
    connectionResponse a = \case
      INV qInfo -> Invitation qInfo
      CON -> Connected contact
      END -> Disconnected contact
      MSG {msgBody, msgIntegrity, brokerMeta} -> case parseAll groupMessageP msgBody of
        Right (group, msg) -> ReceivedGroupMessage group contact (snd brokerMeta) msg msgIntegrity
        _ -> ReceivedMessage contact (snd brokerMeta) msgBody msgIntegrity
      SENT _ -> NoChatResponse
      OK -> Confirmation contact
      ERR (CONN e) -> ContactError e contact
      ERR e -> ChatError e
      where
        contact = Contact a
    broadcastResponse :: EntityCommand 'Broadcast_ c => ByteString -> ACommand 'Agent c -> ChatResponse
    broadcastResponse g = \case
      MS as -> GroupMembers group $ map (Contact . fromConn) as
      SENT _ -> NoChatResponse
      OK -> GroupConfirmation group
      ERR e@(CONN _) -> GroupError e group
      ERR e@(BCAST _) -> GroupError e group
      ERR e -> ChatError e
      where
        group = Group g
    setActiveTo :: ChatResponse -> STM ()
    setActiveTo = \case
      Connected a -> setActive ct $ ActiveC a
      ReceivedMessage a _ _ _ -> setActive ct $ ActiveC a
      ReceivedGroupMessage g _ _ _ _ -> setActive ct $ ActiveG g
      Disconnected a -> unsetActive ct $ ActiveC a
      _ -> pure ()
    sendNotification :: ChatResponse -> STM ()
    sendNotification = \case
      ReceivedMessage c' _ msg _ -> notify $ Notification ("@" <> toBs c') msg
      ReceivedGroupMessage g c' _ msg _ -> notify $ Notification ("#" <> fromGroup g <> " @" <> toBs c') msg
      Disconnected c' -> notify $ Notification ("@" <> toBs c') "disconnected"
      _ -> pure ()
    notify :: Notification -> STM ()
    notify n = writeTBQueue (notifyQ t) n

showNotifications :: ChatClient -> IO ()
showNotifications t = forever $ atomically (readTBQueue $ notifyQ t) >>= displayNotification t

groupMessageP :: Parser (Group, ByteString)
groupMessageP =
  let group = Group <$> A.takeTill (== ' ')
   in "####" *> ((,) <$> group <* A.space <*> A.takeByteString)

serializeGroupMessage :: Group -> ByteString -> ByteString
serializeGroupMessage (Group g) msg = "####" <> g <> " " <> msg

setActive :: ChatTerminal -> ActiveTo -> STM ()
setActive ct = writeTVar (activeTo ct)

unsetActive :: ChatTerminal -> ActiveTo -> STM ()
unsetActive ct a = modifyTVar (activeTo ct) unset
  where
    unset a' = if a == a' then ActiveNone else a'
