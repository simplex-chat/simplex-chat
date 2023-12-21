{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.NoteFolders where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Store.Shared (StoreError (..), insertedRowId, withLocalDisplayName)
import Simplex.Chat.Types (NoteFolder (..), NoteFolderId, NoteFolderName, User (..))
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.SQLite (firstRow)
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

getNoteFolderIdByName :: DB.Connection -> User -> NoteFolderName -> ExceptT StoreError IO NoteFolderId
getNoteFolderIdByName db User {userId} ldn =
  ExceptT . firstRow fromOnly (SENoteFolderNotFoundByName ldn) $
    DB.query db [sql| SELECT note_folder_id FROM note_folders WHERE user_id = ? AND local_display_name = ? |] (userId, ldn)

getNoteFolder :: DB.Connection -> User -> NoteFolderId -> ExceptT StoreError IO NoteFolder
getNoteFolder db User {userId} noteFolderId =
  ExceptT . firstRow toNoteFolder (SENoteFolderNotFound noteFolderId) $
    DB.query
      db
      [sql|
        SELECT
          display_name, local_display_name, created_at, updated_at, chat_ts, favorite, unread_chat
        FROM note_folders
        WHERE user_id = ?
          AND note_folder_id = ?
      |]
      (userId, noteFolderId)
  where
    toNoteFolder (displayName, localDisplayName, createdAt, updatedAt, chatTs, favorite, unread) =
      NoteFolder {noteFolderId, userId, displayName, localDisplayName, createdAt, updatedAt, chatTs, favorite, unread}
