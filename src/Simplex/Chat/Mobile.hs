{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fobject-code #-}

module Simplex.Chat.Mobile where

import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Bifunctor (first)
import qualified Data.ByteString.Base64.URL as U
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Functor (($>))
import Data.List (find)
import qualified Data.List.NonEmpty as L
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Foreign.C.String
import Foreign.C.Types (CInt (..))
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable (poke)
import GHC.IO.Encoding (setFileSystemEncoding, setForeignEncoding, setLocaleEncoding)
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Library.Commands
import Simplex.Chat.Markdown (ParsedMarkdown (..), parseMaybeMarkdownList)
import Simplex.Chat.Mobile.File
import Simplex.Chat.Mobile.Shared
import Simplex.Chat.Mobile.WebRTC
import Simplex.Chat.Options
import Simplex.Chat.Options.DB
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Client (agentClientStore)
import Simplex.Messaging.Agent.Env.SQLite (createAgentStore)
import Simplex.Messaging.Agent.Store.Interface (closeDBStore, reopenDBStore)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfirmation (..), MigrationError)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, sumTypeJSON)
import Simplex.Messaging.Protocol (AProtoServerWithAuth (..), AProtocolType (..), BasicAuth (..), ProtoServerWithAuth (..), ProtocolServer (..))
import Simplex.Messaging.Util (catchAll, liftEitherWith, safeDecodeUtf8)
import System.IO (utf8)
import System.Timeout (timeout)
#if !defined(dbPostgres)
import Data.ByteArray (ScrubbedBytes)
import Database.SQLite.Simple (SQLError (..))
import qualified Database.SQLite.Simple as DB
import qualified Simplex.Messaging.Agent.Store.DB as DB
#endif

data DBMigrationResult
  = DBMOk
  | DBMInvalidConfirmation
  | DBMErrorNotADatabase {dbFile :: String}
  | DBMErrorMigration {dbFile :: String, migrationError :: MigrationError}
  | DBMErrorSQL {dbFile :: String, migrationSQLError :: String}
  deriving (Show)

$(JQ.deriveToJSON (sumTypeJSON $ dropPrefix "DBM") ''DBMigrationResult)

data APIResult r
  = APIResult  {remoteHostId :: Maybe RemoteHostId, result :: r}
  | APIError {remoteHostId :: Maybe RemoteHostId, error :: ChatError}

eitherToResult :: Maybe RemoteHostId -> Either ChatError r -> APIResult r
eitherToResult rhId = either (APIError rhId) (APIResult rhId)
{-# INLINE eitherToResult #-}

$(pure [])

instance ToJSON r => ToJSON (APIResult r) where
  toEncoding = $(JQ.mkToEncoding (defaultJSON {J.sumEncoding = J.UntaggedValue}) ''APIResult)
  toJSON = $(JQ.mkToJSON (defaultJSON {J.sumEncoding = J.UntaggedValue}) ''APIResult)

foreign export ccall "chat_migrate_init" cChatMigrateInit :: CString -> CString -> CString -> Ptr (StablePtr ChatController) -> IO CJSONString

foreign export ccall "chat_migrate_init_key" cChatMigrateInitKey :: CString -> CString -> CInt -> CString -> CInt -> Ptr (StablePtr ChatController) -> IO CJSONString

foreign export ccall "chat_close_store" cChatCloseStore :: StablePtr ChatController -> IO CString

foreign export ccall "chat_reopen_store" cChatReopenStore :: StablePtr ChatController -> IO CString

foreign export ccall "chat_send_cmd" cChatSendCmd :: StablePtr ChatController -> CString -> IO CJSONString

foreign export ccall "chat_send_remote_cmd" cChatSendRemoteCmd :: StablePtr ChatController -> CInt -> CString -> IO CJSONString

foreign export ccall "chat_recv_msg" cChatRecvMsg :: StablePtr ChatController -> IO CJSONString

foreign export ccall "chat_recv_msg_wait" cChatRecvMsgWait :: StablePtr ChatController -> CInt -> IO CJSONString

foreign export ccall "chat_parse_markdown" cChatParseMarkdown :: CString -> IO CJSONString

foreign export ccall "chat_parse_server" cChatParseServer :: CString -> IO CJSONString

foreign export ccall "chat_password_hash" cChatPasswordHash :: CString -> CString -> IO CString

foreign export ccall "chat_valid_name" cChatValidName :: CString -> IO CString

foreign export ccall "chat_json_length" cChatJsonLength :: CString -> IO CInt

foreign export ccall "chat_encrypt_media" cChatEncryptMedia :: StablePtr ChatController -> CString -> Ptr Word8 -> CInt -> IO CString

foreign export ccall "chat_decrypt_media" cChatDecryptMedia :: CString -> Ptr Word8 -> CInt -> IO CString

foreign export ccall "chat_write_file" cChatWriteFile :: StablePtr ChatController -> CString -> Ptr Word8 -> CInt -> IO CJSONString

foreign export ccall "chat_read_file" cChatReadFile :: CString -> CString -> CString -> IO (Ptr Word8)

foreign export ccall "chat_encrypt_file" cChatEncryptFile :: StablePtr ChatController -> CString -> CString -> IO CJSONString

foreign export ccall "chat_decrypt_file" cChatDecryptFile :: CString -> CString -> CString -> CString -> IO CString

-- | check / migrate database and initialize chat controller on success
-- For postgres first param is schema prefix, second param is database connection string.
cChatMigrateInit :: CString -> CString -> CString -> Ptr (StablePtr ChatController) -> IO CJSONString
cChatMigrateInit fp key conf = cChatMigrateInitKey fp key 0 conf 0

-- For postgres first param is schema prefix, second param is database connection string.
cChatMigrateInitKey :: CString -> CString -> CInt -> CString -> CInt -> Ptr (StablePtr ChatController) -> IO CJSONString
cChatMigrateInitKey fp key keepKey conf background ctrl = do
  -- ensure we are set to UTF-8; iOS does not have locale, and will default to
  -- US-ASCII all the time.
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8

  chatDbOpts <- mobileDbOpts fp key
  confirm <- peekCAString conf
  r <-
    chatMigrateInitKey chatDbOpts (keepKey /= 0) confirm (background /= 0) >>= \case
      Right cc -> (newStablePtr cc >>= poke ctrl) $> DBMOk
      Left e -> pure e
  newCStringFromLazyBS $ J.encode r

cChatCloseStore :: StablePtr ChatController -> IO CString
cChatCloseStore cPtr = deRefStablePtr cPtr >>= chatCloseStore >>= newCAString

cChatReopenStore :: StablePtr ChatController -> IO CString
cChatReopenStore cPtr = do
  c <- deRefStablePtr cPtr
  newCAString =<< chatReopenStore c

-- | send command to chat (same syntax as in terminal for now)
cChatSendCmd :: StablePtr ChatController -> CString -> IO CJSONString
cChatSendCmd cPtr cCmd = do
  c <- deRefStablePtr cPtr
  cmd <- B.packCString cCmd
  newCStringFromLazyBS =<< chatSendCmd c cmd

-- | send command to chat (same syntax as in terminal for now)
cChatSendRemoteCmd :: StablePtr ChatController -> CInt -> CString -> IO CJSONString
cChatSendRemoteCmd cPtr cRemoteHostId cCmd = do
  c <- deRefStablePtr cPtr
  cmd <- B.packCString cCmd
  let rhId = Just $ fromIntegral cRemoteHostId
  newCStringFromLazyBS =<< chatSendRemoteCmd c rhId cmd

-- | receive message from chat (blocking)
cChatRecvMsg :: StablePtr ChatController -> IO CJSONString
cChatRecvMsg cc = deRefStablePtr cc >>= chatRecvMsg >>= newCStringFromLazyBS

-- |  receive message from chat (blocking up to `t` microseconds (1/10^6 sec), returns empty string if times out)
cChatRecvMsgWait :: StablePtr ChatController -> CInt -> IO CJSONString
cChatRecvMsgWait cc t = deRefStablePtr cc >>= (`chatRecvMsgWait` fromIntegral t) >>= newCStringFromLazyBS

-- | parse markdown - returns ParsedMarkdown type JSON
cChatParseMarkdown :: CString -> IO CJSONString
cChatParseMarkdown s = newCStringFromLazyBS . chatParseMarkdown =<< B.packCString s

-- | parse server address - returns ParsedServerAddress JSON
cChatParseServer :: CString -> IO CJSONString
cChatParseServer s = newCStringFromLazyBS . chatParseServer =<< B.packCString s

cChatPasswordHash :: CString -> CString -> IO CString
cChatPasswordHash cPwd cSalt = do
  pwd <- B.packCString cPwd
  salt <- B.packCString cSalt
  newCStringFromBS $ chatPasswordHash pwd salt

-- This function supports utf8 strings
cChatValidName :: CString -> IO CString
cChatValidName cName = newCString . mkValidName =<< peekCString cName

-- | returns length of JSON encoded string
cChatJsonLength :: CString -> IO CInt
cChatJsonLength s = fromIntegral . subtract 2 . LB.length . J.encode . safeDecodeUtf8 <$> B.packCString s

mobileChatOpts :: ChatDbOpts -> ChatOpts
mobileChatOpts dbOptions =
  ChatOpts
    { coreOptions =
        CoreChatOpts
          { dbOptions,
            smpServers = [],
            xftpServers = [],
            simpleNetCfg = defaultSimpleNetCfg,
            logLevel = CLLImportant,
            logConnections = False,
            logServerHosts = True,
            logAgent = Nothing,
            logFile = Nothing,
            tbqSize = 4096,
            deviceName = Nothing,
            highlyAvailable = False,
            yesToUpMigrations = False
          },
      chatCmd = "",
      chatCmdDelay = 3,
      chatCmdLog = CCLNone,
      chatServerPort = Nothing,
      optFilesFolder = Nothing,
      optTempDirectory = Nothing,
      showReactions = False,
      allowInstantFiles = True,
      autoAcceptFileSize = 0,
      muteNotifications = True,
      markRead = False,
      maintenance = True
    }

defaultMobileConfig :: ChatConfig
defaultMobileConfig =
  defaultChatConfig
    { confirmMigrations = MCYesUp,
      logLevel = CLLError,
      coreApi = True,
      deviceNameForRemote = "Mobile"
    }

getActiveUser_ :: DBStore -> IO (Maybe User)
getActiveUser_ st = find activeUser <$> withTransaction st getUsers

#if !defined(dbPostgres)
-- only used in tests
chatMigrateInit :: String -> ScrubbedBytes -> String -> IO (Either DBMigrationResult ChatController)
chatMigrateInit dbFilePrefix dbKey confirm = do
  let chatDBOpts = ChatDbOpts {dbFilePrefix, dbKey, trackQueries = DB.TQSlow 5000, vacuumOnMigration = True}
  chatMigrateInitKey chatDBOpts False confirm False
#endif

chatMigrateInitKey :: ChatDbOpts -> Bool -> String -> Bool -> IO (Either DBMigrationResult ChatController)
chatMigrateInitKey chatDbOpts keepKey confirm backgroundMode = runExceptT $ do
  confirmMigrations <- liftEitherWith (const DBMInvalidConfirmation) $ strDecode $ B.pack confirm
  chatStore <- migrate createChatStore (toDBOpts chatDbOpts chatSuffix keepKey) confirmMigrations
  agentStore <- migrate createAgentStore (toDBOpts chatDbOpts agentSuffix keepKey) confirmMigrations
  liftIO $ initialize chatStore ChatDatabase {chatStore, agentStore}
  where
    opts = mobileChatOpts $ removeDbKey chatDbOpts
    initialize st db = do
      user_ <- getActiveUser_ st
      newChatController db user_ defaultMobileConfig opts backgroundMode
    migrate createStore dbOpts confirmMigrations =
      ExceptT $
        (first (DBMErrorMigration errDbStr) <$> createStore dbOpts confirmMigrations)
#if !defined(dbPostgres)
          `catch` (pure . checkDBError)
#endif
          `catchAll` (pure . dbError)
      where
        errDbStr = errorDbStr dbOpts
#if !defined(dbPostgres)
        checkDBError e = case sqlError e of
          DB.ErrorNotADatabase -> Left $ DBMErrorNotADatabase errDbStr
          _ -> dbError e
#endif
        dbError e = Left . DBMErrorSQL errDbStr $ show e

chatCloseStore :: ChatController -> IO String
chatCloseStore ChatController {chatStore, smpAgent} = handleErr $ do
  closeDBStore chatStore
  closeDBStore $ agentClientStore smpAgent

chatReopenStore :: ChatController -> IO String
chatReopenStore ChatController {chatStore, smpAgent} = handleErr $ do
  reopenDBStore chatStore
  reopenDBStore (agentClientStore smpAgent)

handleErr :: IO () -> IO String
handleErr a = (a $> "") `catch` (pure . show @SomeException)

chatSendCmd :: ChatController -> B.ByteString -> IO JSONByteString
chatSendCmd cc = chatSendRemoteCmd cc Nothing

chatSendRemoteCmd :: ChatController -> Maybe RemoteHostId -> B.ByteString -> IO JSONByteString
chatSendRemoteCmd cc rh s = J.encode . eitherToResult rh <$> runReaderT (execChatCommand rh s) cc

chatRecvMsg :: ChatController -> IO JSONByteString
chatRecvMsg ChatController {outputQ} = J.encode . uncurry eitherToResult <$> readChatResponse
  where
    readChatResponse =
      atomically (readTBQueue outputQ) >>= \case
        (_, Right CEvtTerminalEvent {}) -> readChatResponse
        out -> pure out

chatRecvMsgWait :: ChatController -> Int -> IO JSONByteString
chatRecvMsgWait cc time = fromMaybe "" <$> timeout time (chatRecvMsg cc)

chatParseMarkdown :: ByteString -> JSONByteString
chatParseMarkdown = J.encode . ParsedMarkdown . parseMaybeMarkdownList . safeDecodeUtf8

chatParseServer :: ByteString -> JSONByteString
chatParseServer = J.encode . toServerAddress . strDecode
  where
    toServerAddress :: Either String AProtoServerWithAuth -> ParsedServerAddress
    toServerAddress = \case
      Right (AProtoServerWithAuth protocol (ProtoServerWithAuth ProtocolServer {host, port, keyHash = C.KeyHash kh} auth)) ->
        let basicAuth = maybe "" (\(BasicAuth a) -> enc a) auth
         in ParsedServerAddress (Just ServerAddress {serverProtocol = AProtocolType protocol, hostnames = L.map enc host, port, keyHash = enc kh, basicAuth}) ""
      Left e -> ParsedServerAddress Nothing e
    enc :: StrEncoding a => a -> String
    enc = B.unpack . strEncode

chatPasswordHash :: ByteString -> ByteString -> ByteString
chatPasswordHash pwd salt = either (const "") passwordHash salt'
  where
    salt' = U.decode salt
    passwordHash = U.encode . C.sha512Hash . (pwd <>)
