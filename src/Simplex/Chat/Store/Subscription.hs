{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Simplex.Chat.Store.Subscription
  ( getContactConnsToSub,
    getUCLConnsToSub,
    getMemberConnsToSub,
    getPendingConnsToSub,
    unsetConnectionToSubscribe
  )
where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (ConnId)
import qualified Simplex.Messaging.Agent.Store.DB as DB
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

getContactConnsToSub :: DB.Connection -> User -> Bool -> IO (Map ConnId Int64)
getContactConnsToSub db User {userId} filterToSubscribe =
  M.fromList <$> DB.query db query (userId, ConnDeleted, CSActive)
  where
    query
      | filterToSubscribe = baseQuery <> " AND c.to_subscribe = 1" <> cond
      | otherwise = baseQuery <> cond
    baseQuery =
      [sql|
        SELECT c.agent_conn_id, c.connection_id
        FROM connections c
        JOIN contacts ct ON ct.contact_id = c.contact_id
        WHERE c.user_id = ?
      |]
    cond =
      [sql|
        AND c.conn_status != ?
        AND ct.contact_status = ? AND ct.deleted = 0
      |]

getUCLConnsToSub :: DB.Connection -> User -> Bool -> IO (Map ConnId Int64)
getUCLConnsToSub db User {userId} filterToSubscribe =
  M.fromList <$> DB.query db query (userId, ConnDeleted)
  where
    query
      | filterToSubscribe = baseQuery <> " AND c.to_subscribe = 1" <> cond
      | otherwise = baseQuery <> cond
    baseQuery =
      [sql|
        SELECT c.agent_conn_id, c.connection_id
        FROM connections c
        JOIN user_contact_links ucl ON ucl.user_contact_link_id = c.user_contact_link_id
        WHERE c.user_id = ?
      |]
    cond = " AND c.conn_status != ?"

getMemberConnsToSub :: DB.Connection -> User -> Bool -> IO (Map ConnId Int64)
getMemberConnsToSub db User {userId, userContactId} filterToSubscribe =
  M.fromList <$>
    DB.query
      db
      query
      ((userId, ConnDeleted, userContactId)
        :. (GSMemRemoved, GSMemLeft, GSMemGroupDeleted, GSMemRemoved, GSMemLeft, GSMemGroupDeleted))
  where
    query
      | filterToSubscribe = baseQuery <> " AND c.to_subscribe = 1" <> cond
      | otherwise = baseQuery <> cond
    baseQuery =
      [sql|
        SELECT c.agent_conn_id, c.connection_id
        FROM connections c
        JOIN group_members m ON m.group_member_id = c.group_member_id
        JOIN groups g ON g.group_id = m.group_id
        JOIN group_members mu ON mu.group_id = g.group_id
        WHERE c.user_id = ?
      |]
    cond =
      [sql|
        AND c.conn_status != ?
        AND mu.contact_id = ? AND mu.member_status NOT IN (?,?,?)
        AND m.member_status NOT IN (?,?,?)
      |]

getPendingConnsToSub :: DB.Connection -> User -> Bool -> IO (Map ConnId Int64)
getPendingConnsToSub db User {userId} filterToSubscribe =
  M.fromList <$> DB.query db query (userId, ConnContact, ConnDeleted)
  where
    query
      | filterToSubscribe = baseQuery <> " AND c.to_subscribe = 1" <> cond
      | otherwise = baseQuery <> cond
    baseQuery =
      [sql|
        SELECT agent_conn_id, connection_id
        FROM connections
        WHERE user_id = ?
      |]
    cond =
      [sql|
        AND conn_type = ?
        AND contact_id IS NULL
        AND conn_status != ?
      |]

unsetConnectionToSubscribe :: DB.Connection -> User -> IO ()
unsetConnectionToSubscribe db User {userId} =
  DB.execute
    db
    "UPDATE connections SET to_subscribe = 0 WHERE user_id = ? AND to_subscribe = 1"
    (Only userId)
