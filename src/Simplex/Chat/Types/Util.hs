{-# LANGUAGE LambdaCase #-}

module Simplex.Chat.Types.Util where

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable
import Database.SQLite.Simple (ResultError (..), SQLData (..))
import Database.SQLite.Simple.FromField (FieldParser, returnError)
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok (Ok (Ok))
import Simplex.Messaging.Util (safeDecodeUtf8)

encodeJSON :: ToJSON a => a -> Text
encodeJSON = safeDecodeUtf8 . LB.toStrict . J.encode

decodeJSON :: FromJSON a => Text -> Maybe a
decodeJSON = J.decode . LB.fromStrict . encodeUtf8

fromBlobField_ :: Typeable k => (ByteString -> Either String k) -> FieldParser k
fromBlobField_ p = \case
  f@(Field (SQLBlob b) _) ->
    case p b of
      Right k -> Ok k
      Left e -> returnError ConversionFailed f ("could not parse field: " ++ e)
  f -> returnError ConversionFailed f "expecting SQLBlob column type"

defOpts :: J.Options
defOpts = J.defaultOptions {J.omitNothingFields = True}
