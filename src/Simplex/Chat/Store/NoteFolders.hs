{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.NoteFolders where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Store.Shared (StoreError, insertedRowId, withLocalDisplayName)
import Simplex.Chat.Types (NoteFolder (..))
import Simplex.Messaging.Agent.Protocol (UserId)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB

createNewNoteFolder :: DB.Connection -> UserId -> Text -> ExceptT StoreError IO NoteFolder
createNewNoteFolder db userId displayName = do
  ts <- liftIO getCurrentTime
  ExceptT $ withLocalDisplayName db userId displayName $ \localDisplayName -> runExceptT $ do
    liftIO $ do
      DB.execute
        db
        [sql|
          INSERT INTO note_folders
            (user_id, display_name, local_display_name, created_at, updated_at, chat_ts, favorite, unread_chat)
          VALUES
            (?, ?, ?, ?, ?, ?, ?, ?)
        |]
        (userId, displayName, localDisplayName, ts, ts, ts, favorite, unread)
      noteFolderId <- insertedRowId db
      pure
        NoteFolder
          { noteFolderId,
            userId,
            displayName,
            localDisplayName,
            createdAt = ts,
            updatedAt = ts,
            chatTs = ts,
            favorite,
            unread
          }
  where
    favorite = False
    unread = False
