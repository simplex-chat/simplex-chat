{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261120_handled_rcv_msgs where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261120_handled_rcv_msgs :: Text
m20261120_handled_rcv_msgs =
  [r|
CREATE TABLE handled_rcv_msgs(
  connection_id BIGINT NOT NULL REFERENCES connections ON DELETE CASCADE,
  agent_msg_id BIGINT NOT NULL,
  rcpt SMALLINT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (connection_id, agent_msg_id)
);
|]

down_m20261120_handled_rcv_msgs :: Text
down_m20261120_handled_rcv_msgs =
  [r|
DROP TABLE handled_rcv_msgs;
|]
