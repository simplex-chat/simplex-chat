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

data RequestVerb = GET | POST | PUT | DELETE

instance StrEncoding RequestVerb where
  strEncode = \case
    GET -> "GET"
    POST -> "POST"
    PUT -> "PUT"
    DELETE -> "DELETE"
  strDecode = \case
    "GET" -> Right GET
    "POST" -> Right POST
    "PUT" -> Right PUT
    "DELETE" -> Right DELETE
    _ -> Left "bad RequestVerb"
  strP = strDecode <$?> A.takeByteString

newtype RequestPath = RequestPath [ByteString]

instance StrEncoding RequestPath where
  strEncode (RequestPath ss) = "/" <> B.intercalate "/" ss
  strP = RequestPath <$> (A.char '/' *> A.takeTill (A.inClass "/") `A.sepBy1'` A.char '/')

type RawChatRequest = (ByteString, ByteString, ByteString, ByteString)

data ChatRequest = ChatRequest
  { crVerb :: RequestVerb,
    crPath :: RequestPath,
    crQueryString :: QueryStringParams,
    crBody :: J.Value
  }

decodeChatRequest :: RawChatRequest -> Either String ChatRequest
decodeChatRequest (verb, path, qs, body) = do
  crVerb <- strDecode verb
  crPath <- strDecode path
  crQueryString <- strDecode qs
  crBody <- J.eitherDecodeStrict' body
  pure ChatRequest {crVerb, crPath, crQueryString, crBody}

-- processChatRequest :: ToJSON a -> ChatMonad m => User -> ChatRequest -> m a

-- sendChatRequest :: ToJSON a => RequestVerb -> ByteString -> ByteString -> ByteString -> IO a
-- sendChatRequest verb path qs body = case (verb, path) of
--   (DELETE, "/contact") ->
