{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Request where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Simplex.Messaging.Agent.QueryString
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util ((<$?>))

-- sendRequest :: ToJSON a => ByteString -> IO a
-- sendRequest s =
