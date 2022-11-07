{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Mobile where

import Control.Concurrent.STM
import Control.Exception (catch)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Database.SQLite.Simple (SQLError (..))
import qualified Database.SQLite.Simple as DB
import Foreign.C.String
import Foreign.C.Types (CInt (..))
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable (poke)
import GHC.Generics (Generic)
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Markdown (ParsedMarkdown (..), parseMaybeMarkdownList)
import Simplex.Chat.Options
import Simplex.Chat.Store
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (yesToMigrations), createAgentStore)
import Simplex.Messaging.Agent.Store.SQLite (closeSQLiteStore)
import Simplex.Messaging.Client (defaultNetworkConfig)
import Simplex.Messaging.Parsers (dropPrefix, sumTypeJSON)
import Simplex.Messaging.Protocol (CorrId (..))
import Simplex.Messaging.Util (catchAll, safeDecodeUtf8)
import System.Timeout (timeout)

foreign export ccall "chat_migrate_init" cChatMigrateInit :: CString -> CString -> Ptr (StablePtr ChatController) -> IO CJSONString

-- TODO remove
foreign export ccall "chat_migrate_db" cChatMigrateDB :: CString -> CString -> IO CJSONString

-- chat_init is deprecated
foreign export ccall "chat_init" cChatInit :: CString -> IO (StablePtr ChatController)

-- TODO remove
foreign export ccall "chat_init_key" cChatInitKey :: CString -> CString -> IO (StablePtr ChatController)

foreign export ccall "chat_send_cmd" cChatSendCmd :: StablePtr ChatController -> CString -> IO CJSONString

foreign export ccall "chat_recv_msg" cChatRecvMsg :: StablePtr ChatController -> IO CJSONString

foreign export ccall "chat_recv_msg_wait" cChatRecvMsgWait :: StablePtr ChatController -> CInt -> IO CJSONString

foreign export ccall "chat_parse_markdown" cChatParseMarkdown :: CString -> IO CJSONString

-- | check / migrate database and initialize chat controller on success
cChatMigrateInit :: CString -> CString -> Ptr (StablePtr ChatController) -> IO CJSONString
cChatMigrateInit fp key ctrl = do
  dbPath <- peekCAString fp
  dbKey <- peekCAString key
  r <-
    chatMigrateInit dbPath dbKey >>= \case
      Right cc -> (newStablePtr cc >>= poke ctrl) $> DBMOk
      Left e -> pure e
  newCAString . LB.unpack $ J.encode r

-- | check and migrate the database
-- This function validates that the encryption is correct and runs migrations - it should be called before cChatInitKey
-- TODO remove
cChatMigrateDB :: CString -> CString -> IO CJSONString
cChatMigrateDB fp key =
  ((,) <$> peekCAString fp <*> peekCAString key) >>= uncurry chatMigrateDB >>= newCAString . LB.unpack . J.encode

-- | initialize chat controller (deprecated)
-- The active user has to be created and the chat has to be started before most commands can be used.
cChatInit :: CString -> IO (StablePtr ChatController)
cChatInit fp = peekCAString fp >>= chatInit >>= newStablePtr

-- | initialize chat controller with encrypted database
-- The active user has to be created and the chat has to be started before most commands can be used.
-- TODO remove
cChatInitKey :: CString -> CString -> IO (StablePtr ChatController)
cChatInitKey fp key =
  ((,) <$> peekCAString fp <*> peekCAString key) >>= uncurry chatInitKey >>= newStablePtr

-- | send command to chat (same syntax as in terminal for now)
cChatSendCmd :: StablePtr ChatController -> CString -> IO CJSONString
cChatSendCmd cPtr cCmd = do
  c <- deRefStablePtr cPtr
  cmd <- peekCAString cCmd
  newCAString =<< chatSendCmd c cmd

-- | receive message from chat (blocking)
cChatRecvMsg :: StablePtr ChatController -> IO CJSONString
cChatRecvMsg cc = deRefStablePtr cc >>= chatRecvMsg >>= newCAString

-- |  receive message from chat (blocking up to `t` microseconds (1/10^6 sec), returns empty string if times out)
cChatRecvMsgWait :: StablePtr ChatController -> CInt -> IO CJSONString
cChatRecvMsgWait cc t = deRefStablePtr cc >>= (`chatRecvMsgWait` fromIntegral t) >>= newCAString

-- | parse markdown - returns ParsedMarkdown type JSON
cChatParseMarkdown :: CString -> IO CJSONString
cChatParseMarkdown s = newCAString . chatParseMarkdown =<< peekCAString s

mobileChatOpts :: ChatOpts
mobileChatOpts =
  ChatOpts
    { dbFilePrefix = undefined,
      dbKey = "",
      smpServers = [],
      networkConfig = defaultNetworkConfig,
      logConnections = False,
      logServerHosts = True,
      logAgent = False,
      chatCmd = "",
      chatCmdDelay = 3,
      chatServerPort = Nothing,
      maintenance = True
    }

defaultMobileConfig :: ChatConfig
defaultMobileConfig =
  defaultChatConfig
    { yesToMigrations = True,
      agentConfig = (agentConfig defaultChatConfig) {yesToMigrations = True}
    }

type CJSONString = CString

getActiveUser_ :: SQLiteStore -> IO (Maybe User)
getActiveUser_ st = find activeUser <$> withTransaction st getUsers

data DBMigrationResult
  = DBMOk
  | DBMErrorNotADatabase {dbFile :: String}
  | DBMError {dbFile :: String, migrationError :: String}
  deriving (Show, Generic)

instance ToJSON DBMigrationResult where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "DBM"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "DBM"

chatMigrateInit :: String -> String -> IO (Either DBMigrationResult ChatController)
chatMigrateInit dbFilePrefix dbKey = runExceptT $ do
  chatStore <- migrate createChatStore $ chatStoreFile dbFilePrefix
  agentStore <- migrate createAgentStore $ agentStoreFile dbFilePrefix
  liftIO $ initialize chatStore ChatDatabase {chatStore, agentStore}
  where
    initialize st db = do
      user_ <- getActiveUser_ st
      newChatController db user_ defaultMobileConfig mobileChatOpts {dbFilePrefix, dbKey} Nothing
    migrate createStore dbFile =
      ExceptT $
        (Right <$> createStore dbFile dbKey True)
          `catch` (pure . checkDBError)
            `catchAll` (pure . dbError)
      where
        checkDBError e = case sqlError e of
          DB.ErrorNotADatabase -> Left $ DBMErrorNotADatabase dbFile
          _ -> dbError e
        dbError e = Left . DBMError dbFile $ show e

-- TODO remove
chatMigrateDB :: String -> String -> IO DBMigrationResult
chatMigrateDB dbFilePrefix dbKey =
  migrate createChatStore (chatStoreFile dbFilePrefix) >>= \case
    DBMOk -> migrate createAgentStore (agentStoreFile dbFilePrefix)
    e -> pure e
  where
    migrate createStore dbFile =
      ((createStore dbFile dbKey True >>= closeSQLiteStore) $> DBMOk)
        `catch` (pure . checkDBError)
          `catchAll` (pure . dbError)
      where
        checkDBError e = case sqlError e of
          DB.ErrorNotADatabase -> DBMErrorNotADatabase dbFile
          _ -> dbError e
        dbError e = DBMError dbFile $ show e

chatInit :: String -> IO ChatController
chatInit = (`chatInitKey` "")

-- TODO remove
chatInitKey :: String -> String -> IO ChatController
chatInitKey dbFilePrefix dbKey = do
  db@ChatDatabase {chatStore} <- createChatDatabase dbFilePrefix dbKey True
  user_ <- getActiveUser_ chatStore
  newChatController db user_ defaultMobileConfig mobileChatOpts {dbFilePrefix, dbKey} Nothing

chatSendCmd :: ChatController -> String -> IO JSONString
chatSendCmd cc s = LB.unpack . J.encode . APIResponse Nothing <$> runReaderT (execChatCommand $ B.pack s) cc

chatRecvMsg :: ChatController -> IO JSONString
chatRecvMsg ChatController {outputQ} = json <$> atomically (readTBQueue outputQ)
  where
    json (corr, resp) = LB.unpack $ J.encode APIResponse {corr, resp}

chatRecvMsgWait :: ChatController -> Int -> IO JSONString
chatRecvMsgWait cc time = fromMaybe "" <$> timeout time (chatRecvMsg cc)

chatParseMarkdown :: String -> JSONString
chatParseMarkdown = LB.unpack . J.encode . ParsedMarkdown . parseMaybeMarkdownList . safeDecodeUtf8 . B.pack

data APIResponse = APIResponse {corr :: Maybe CorrId, resp :: ChatResponse}
  deriving (Generic)

instance ToJSON APIResponse where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}
