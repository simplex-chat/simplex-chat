{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260612_smp_role_names where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20260612_smp_role_names :: Text
m20260612_smp_role_names =
  T.pack
    [r|
ALTER TABLE server_operators ADD COLUMN smp_role_names SMALLINT NOT NULL DEFAULT 0;

UPDATE server_operators SET smp_role_names = 1 WHERE server_operator_tag = 'simplex';
|]

down_m20260612_smp_role_names :: Text
down_m20260612_smp_role_names =
  T.pack
    [r|
ALTER TABLE server_operators DROP COLUMN smp_role_names;
|]
