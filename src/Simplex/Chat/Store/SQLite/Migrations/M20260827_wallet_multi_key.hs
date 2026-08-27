{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260827_wallet_multi_key where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- | Several keys per device, and provenance for the ones that are not derived.
--
--   * @backed_up@ — the phrase has been shown and acknowledged.
--   * @current_seed@ on users — which key a purchase goes under once there is
--     more than one. Selection is a stored pointer, never a prompt: no chat
--     command reads stdin, and a blocking read would hang every GUI client.
--   * @provenance@ on wallet_name_keys — 'derived' names come back from the
--     phrase; an 'imported' raw key does not, and has to be stored. That
--     difference is the one the recovery-key wording depends on, so it is
--     recorded rather than inferred.
m20260827_wallet_multi_key :: Query
m20260827_wallet_multi_key =
  [sql|
ALTER TABLE wallet_seeds ADD COLUMN backed_up INTEGER NOT NULL DEFAULT 0;
ALTER TABLE users ADD COLUMN wallet_current_seed_id INTEGER REFERENCES wallet_seeds ON DELETE RESTRICT;
CREATE INDEX idx_users_wallet_current_seed_id ON users(wallet_current_seed_id);

ALTER TABLE wallet_name_keys ADD COLUMN provenance TEXT NOT NULL DEFAULT 'derived';
ALTER TABLE wallet_name_keys ADD COLUMN priv_key BLOB;
|]

down_m20260827_wallet_multi_key :: Query
down_m20260827_wallet_multi_key =
  [sql|
ALTER TABLE wallet_name_keys DROP COLUMN priv_key;
ALTER TABLE wallet_name_keys DROP COLUMN provenance;

DROP INDEX idx_users_wallet_current_seed_id;
ALTER TABLE users DROP COLUMN wallet_current_seed_id;

ALTER TABLE wallet_seeds DROP COLUMN backed_up;
|]
