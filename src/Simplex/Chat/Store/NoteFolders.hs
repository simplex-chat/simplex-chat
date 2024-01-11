{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Store.NoteFolders where

import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Chat.Types (NoteFolder (..), NoteFolderId, User (..))
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.SQLite (firstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB

createNoteFolder :: DB.Connection -> User -> ExceptT StoreError IO ()
createNoteFolder db User {userId} = do
  liftIO (DB.query db "SELECT note_folder_id FROM note_folders WHERE user_id = ? LIMIT 1" $ Only userId) >>= \case
    [] -> liftIO $ DB.execute db "INSERT INTO note_folders (user_id) VALUES (?)" (Only userId)
    Only noteFolderId : _ -> throwError $ SENoteFolderAlreadyExists noteFolderId

getUserNoteFolderId :: DB.Connection -> User -> ExceptT StoreError IO NoteFolderId
getUserNoteFolderId db User {userId} =
  ExceptT . firstRow fromOnly SEUserNoteFolderNotFound $
    DB.query db "SELECT note_folder_id FROM note_folders WHERE user_id = ?" (Only userId)

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
  DB.execute db "UPDATE note_folders SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND note_folder_id = ?" (unreadChat, updatedAt, userId, noteFolderId)

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
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND note_folder_id = ?" (userId, noteFolderId)
