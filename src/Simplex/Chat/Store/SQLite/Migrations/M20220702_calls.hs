{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220702_calls where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220702_calls :: Query
m20220702_calls =
  [sql|
CREATE TABLE calls ( -- stores call invitations state for communicating state between NSE and app when call notification comes
  call_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE CASCADE,
  shared_call_id BLOB NOT NULL,
  chat_item_id INTEGER NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  call_state BLOB NOT NULL,
  call_ts TEXT NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);
|]
