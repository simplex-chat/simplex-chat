{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231106_preset_contact where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- TODO preset contact
-- update contacts to PCSimpleX where contact_profiles contact_link = admin link?
-- may obstruct connecting with simplex contact if another contact shared same address
-- insert PCNoteToSelf contact
-- insert PCSimpleX contact if doesn't exist? or only for new users?
-- make transaction in code? (could reuse APIAddPresetContact logic)
m20231106_preset_contact :: Query
m20231106_preset_contact =
  [sql|
ALTER TABLE contacts ADD COLUMN preset_contact TEXT;
|]

down_m20231106_preset_contact :: Query
down_m20231106_preset_contact =
  [sql|
ALTER TABLE contacts DROP COLUMN preset_contact;
|]
