{-# LANGUAGE QuasiQuotes #-}

-- Member profile dissemination in relay-mediated groups (task 001):
--   delivery_jobs.sender_group_member_ids replaces delivery_jobs.single_sender_group_member_id.
--   One column carries either [s] (single-sender job) or [s1, s2, ...] (multi-sender batch).
--   The (group_members.member_relations_vector, MRIntroduced) bit tracks
--   which recipients have been announced for each sender — reused, not duplicated.
module Simplex.Chat.Store.SQLite.Migrations.M20260515_delivery_job_senders where

import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Int (Int64)
import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite3 (funcArgBlob, funcArgInt64, funcResultBlob, funcResultNull, funcResultInt64)
import Database.SQLite3.Bindings
import Foreign.C.Types
import Foreign.Ptr
import Simplex.Messaging.Agent.Store.SQLite.Util (SQLiteFunc, mkSQLiteFunc)
import Simplex.Messaging.Encoding (smpEncodeList, smpListP)
import Simplex.Messaging.Parsers (parseAll)

-- Backfill helper: encode a singleton Int64 list in the same wire format
-- as runtime writes (smpEncodeList [s]). Used by the up migration to
-- convert each old single-sender row into the new list-encoded column.
foreign export ccall "simplex_encode_singleton_list" sqliteEncodeSingletonList :: SQLiteFunc

foreign import ccall "&simplex_encode_singleton_list" sqliteEncodeSingletonListPtr :: FunPtr SQLiteFunc

sqliteEncodeSingletonList :: SQLiteFunc
sqliteEncodeSingletonList = mkSQLiteFunc $ \cxt args -> do
  i <- funcArgInt64 args 0
  funcResultBlob cxt $ smpEncodeList [i :: Int64]

-- Backfill helper: decode a singleton Int64 list back to a plain Int64.
-- Used by the down migration; returns NULL for empty, multi-element, or
-- malformed blobs (only the single-sender shape can be reversed).
foreign export ccall "simplex_decode_singleton_list" sqliteDecodeSingletonList :: SQLiteFunc

foreign import ccall "&simplex_decode_singleton_list" sqliteDecodeSingletonListPtr :: FunPtr SQLiteFunc

sqliteDecodeSingletonList :: SQLiteFunc
sqliteDecodeSingletonList = mkSQLiteFunc $ \cxt args -> do
  blob <- funcArgBlob args 0
  case parseAll (smpListP :: Parser [Int64]) blob of
    Right [s] -> funcResultInt64 cxt s
    _ -> funcResultNull cxt

m20260515_delivery_job_senders :: Query
m20260515_delivery_job_senders =
  [sql|
DROP INDEX idx_delivery_jobs_single_sender_group_member_id;

ALTER TABLE delivery_jobs ADD COLUMN sender_group_member_ids BLOB;

UPDATE delivery_jobs
SET sender_group_member_ids = encode_singleton_list(single_sender_group_member_id)
WHERE single_sender_group_member_id IS NOT NULL;

ALTER TABLE delivery_jobs DROP COLUMN single_sender_group_member_id;
|]

down_m20260515_delivery_job_senders :: Query
down_m20260515_delivery_job_senders =
  [sql|
ALTER TABLE delivery_jobs ADD COLUMN single_sender_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE CASCADE;

UPDATE delivery_jobs
SET single_sender_group_member_id = decode_singleton_list(sender_group_member_ids)
WHERE sender_group_member_ids IS NOT NULL;

ALTER TABLE delivery_jobs DROP COLUMN sender_group_member_ids;

CREATE INDEX idx_delivery_jobs_single_sender_group_member_id ON delivery_jobs(single_sender_group_member_id);
|]
