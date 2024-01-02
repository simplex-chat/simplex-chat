{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.NoteFolders where

import Control.Monad.Except (ExceptT (..))
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Chat.Types (NoteFolder (..), NoteFolderId, NoteFolderName, User (..))
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.SQLite (firstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB

getNoteFolderIdByName :: DB.Connection -> User -> NoteFolderName -> ExceptT StoreError IO NoteFolderId
getNoteFolderIdByName db User {userId} ldn =
  ExceptT . firstRow fromOnly (SENoteFolderNotFoundByName ldn) $
    DB.query db [sql| SELECT note_folder_id FROM note_folders WHERE user_id = ? AND "-" = ? |] (userId, ldn)

getNoteFolder :: DB.Connection -> User -> NoteFolderId -> ExceptT StoreError IO NoteFolder
getNoteFolder db User {userId} noteFolderId =
  ExceptT . firstRow toNoteFolder (SENoteFolderNotFound noteFolderId) $
    DB.query
      db
      [sql|
        SELECT
          created_at, updated_at, chat_ts, favorite, unread_chat
        FROM note_folders
        WHERE user_id = ?
          AND note_folder_id = ?
      |]
      (userId, noteFolderId)
  where
    toNoteFolder (createdAt, updatedAt, chatTs, favorite, unread) =
      NoteFolder {noteFolderId, userId, createdAt, updatedAt, chatTs, favorite, unread}

updateNoteFolderUnreadChat :: DB.Connection -> User -> NoteFolder -> Bool -> IO ()
updateNoteFolderUnreadChat db User {userId} NoteFolder {noteFolderId} unreadChat = do
  updatedAt <- getCurrentTime
  DB.execute db [sql| UPDATE note_folders SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND note_folder_id = ? |] (unreadChat, updatedAt, userId, noteFolderId)

deleteNoteFolderFiles :: DB.Connection -> UserId -> NoteFolder -> IO ()
deleteNoteFolderFiles db userId NoteFolder {noteFolderId} = do
  DB.execute
    db
    [sql|
      DELETE FROM files
      WHERE user_id = ?
        AND chat_item_id IN (
          SELECT chat_item_id FROM chat_items WHERE user_id = ? AND note_folder_id = ?
        )
    |]
    (userId, userId, noteFolderId)

deleteNoteFolderCIs :: DB.Connection -> User -> NoteFolder -> IO ()
deleteNoteFolderCIs db User {userId} NoteFolder {noteFolderId} =
  DB.execute db [sql| DELETE FROM chat_items WHERE user_id = ? AND note_folder_id = ? |] (userId, noteFolderId)
