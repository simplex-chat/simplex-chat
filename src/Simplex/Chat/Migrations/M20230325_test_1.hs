{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230325_test_1 where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230325_test_1 :: Query
m20230325_test_1 =
  [sql|
create table test1 (id1 integer primary key);
|]

down_m20230325_test_1 :: Query
down_m20230325_test_1 =
  [sql|
drop table test1;
|]
