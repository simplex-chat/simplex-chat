{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240530_user_contact_links_user_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240530_user_contact_links_user_id :: Query
m20240530_user_contact_links_user_id =
  [sql|
CREATE INDEX idx_user_contact_links_user_id ON user_contact_links(user_id);
|]

down_m20240530_user_contact_links_user_id :: Query
down_m20240530_user_contact_links_user_id =
  [sql|
DROP INDEX idx_user_contact_links_user_id;
|]
