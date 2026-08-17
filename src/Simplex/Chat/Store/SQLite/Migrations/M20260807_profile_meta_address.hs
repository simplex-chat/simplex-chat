{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260807_profile_meta_address where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- | The stealth meta-address a profile publishes, as hex.
--
-- Two compressed secp256k1 public keys: a spending key and a viewing key. It is
-- how a contact sends a name without a handshake — they derive a one-time
-- destination from it, and only the holder of the viewing key can find what
-- lands there.
--
-- Safe to distribute widely, which is why it rides the profile rather than
-- needing its own exchange: it is not an address, never appears on chain, and
-- holding it confers only the ability to send. Locating a destination derived
-- from it requires either the sender's ephemeral secret or the recipient's
-- private viewing key, and a meta-address is neither.
--
-- Incognito profiles must leave this NULL: an incognito profile carrying the
-- user's meta-address would hand the contact a correlator straight back to
-- their main identity.
m20260807_profile_meta_address :: Query
m20260807_profile_meta_address =
  [sql|
ALTER TABLE contact_profiles ADD COLUMN meta_address TEXT;
|]

down_m20260807_profile_meta_address :: Query
down_m20260807_profile_meta_address =
  [sql|
ALTER TABLE contact_profiles DROP COLUMN meta_address;
|]
