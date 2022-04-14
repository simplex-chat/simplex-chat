{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Mobile where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad.Reader
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List (find)
import Foreign.C.String
import Foreign.StablePtr
import GHC.Generics (Generic)
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Options
import Simplex.Chat.Store
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (yesToMigrations))
import Simplex.Messaging.Protocol (CorrId (..))
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, removeFile)

foreign export ccall "chat_init" cChatInit :: CString -> IO (StablePtr ChatController)

foreign export ccall "chat_send_cmd" cChatSendCmd :: StablePtr ChatController -> CString -> IO CJSONString

foreign export ccall "chat_recv_msg" cChatRecvMsg :: StablePtr ChatController -> IO CJSONString

-- | initialize chat controller
-- The active user has to be created and the chat has to be started before most commands can be used.
cChatInit :: CString -> IO (StablePtr ChatController)
cChatInit fp = peekCAString fp >>= chatInit >>= newStablePtr

-- | send command to chat (same syntax as in terminal for now)
cChatSendCmd :: StablePtr ChatController -> CString -> IO CJSONString
cChatSendCmd cPtr cCmd = do
  c <- deRefStablePtr cPtr
  cmd <- peekCAString cCmd
  newCAString =<< chatSendCmd c cmd

-- | receive message from chat (blocking)
cChatRecvMsg :: StablePtr ChatController -> IO CJSONString
cChatRecvMsg cc = deRefStablePtr cc >>= chatRecvMsg >>= newCAString

mobileChatOpts :: ChatOpts
mobileChatOpts =
  ChatOpts
    { dbFilePrefix = undefined,
      smpServers = [],
      logConnections = False,
      logAgent = False,
      chatCmd = "",
      chatCmdDelay = 3
    }

defaultMobileConfig :: ChatConfig
defaultMobileConfig =
  defaultChatConfig
    { yesToMigrations = True,
      agentConfig = (agentConfig defaultChatConfig) {yesToMigrations = True}
    }

type CJSONString = CString

getActiveUser_ :: SQLiteStore -> IO (Maybe User)
getActiveUser_ st = find activeUser <$> getUsers st

chatInit :: String -> IO ChatController
chatInit filesFolder = do
  moveDbFilesToFilesFolder -- one time migration to make android file paths parallel to those on ios
  let dbFilePrefix = filesFolder <> "/mobile_v1"
  let f = chatStoreFile dbFilePrefix
  chatStore <- createStore f (dbPoolSize defaultMobileConfig) (yesToMigrations (defaultMobileConfig :: ChatConfig))
  user_ <- getActiveUser_ chatStore
  newChatController chatStore user_ defaultMobileConfig {filesFolder = Just filesFolder} mobileChatOpts {dbFilePrefix} Nothing
  where
    moveDbFilesToFilesFolder :: IO ()
    moveDbFilesToFilesFolder = do
      createDirectoryIfMissing False filesFolder
      moveFileIfExists (filesFolder <> "_chat.db") (filesFolder <> "/mobile_v1_chat.db")
      moveFileIfExists (filesFolder <> "_chat.db.bak") (filesFolder <> "/mobile_v1_chat.db.bak")
      moveFileIfExists (filesFolder <> "_agent.db") (filesFolder <> "/mobile_v1_agent.db")
      moveFileIfExists (filesFolder <> "_agent.db.bak") (filesFolder <> "/mobile_v1_agent.db.bak")
    moveFileIfExists :: FilePath -> FilePath -> IO ()
    moveFileIfExists file mvFile = do
      fileExists <- doesFileExist file
      when fileExists $ do
        copyFile file mvFile
        removeFile file `E.catch` \(_ :: E.SomeException) -> pure () -- don't catch?

chatSendCmd :: ChatController -> String -> IO JSONString
chatSendCmd cc s = LB.unpack . J.encode . APIResponse Nothing <$> runReaderT (execChatCommand $ B.pack s) cc

chatRecvMsg :: ChatController -> IO JSONString
chatRecvMsg ChatController {outputQ} = json <$> atomically (readTBQueue outputQ)
  where
    json (corr, resp) = LB.unpack $ J.encode APIResponse {corr, resp}

data APIResponse = APIResponse {corr :: Maybe CorrId, resp :: ChatResponse}
  deriving (Generic)

instance ToJSON APIResponse where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}
