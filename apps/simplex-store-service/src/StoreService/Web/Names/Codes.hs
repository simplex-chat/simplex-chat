{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Web.Names.Codes (codeCheckHandler, codeRefusalResponse) where

import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Network.HTTP.Types (status200, status400, status409)
import Network.Wai (Request, Response, ResponseReceived)
import Simplex.Chat.Badges.Code (NameCode (..), parseNameCode)
import Simplex.Messaging.Agent.Store.Common (DBStore)
import StoreService.Names.Codes (CodeRefusal (..), codeRefusalTag, codeState, codeStateText, readPrepaidCode)
import StoreService.Names.Store (PrepaidCode (..))
import StoreService.Web.Http (Respond, badRequest, jsonError, jsonResponse, maxBodyBytes, readBoundedBody)

-- | It names no registration, even for a reserved code: holding a code proves nothing about an order.
codeCheckHandler :: DBStore -> Request -> Respond -> IO ResponseReceived
codeCheckHandler st req respond =
  readBoundedBody maxBodyBytes req >>= \case
    Just body | Just text <- codeField body -> case parseNameCode text of
      Just (NCPrepaid code) -> do
        now <- getCurrentTime
        readPrepaidCode st code >>= respond . maybe (codeRefusalResponse CRInvalid) (codeAnswer now)
      _ -> respond $ codeRefusalResponse CRInvalid
    _ -> respond badRequest

codeField :: LB.ByteString -> Maybe Text
codeField body = case J.decode body of
  Just (J.Object o) | Just (J.String t) <- KM.lookup "code" o -> Just t
  _ -> Nothing

codeAnswer :: UTCTime -> PrepaidCode -> Response
codeAnswer now code@PrepaidCode {pcMinLength, pcYears} =
  jsonResponse status200 $ J.object ["minLength" .= pcMinLength, "years" .= pcYears, "state" .= codeStateText (codeState now code)]

-- | Not 404 for an unknown code, which the page reads as not_found whatever the body.
codeRefusalResponse :: CodeRefusal -> Response
codeRefusalResponse refusal = jsonError status (codeRefusalTag refusal)
  where
    status = case refusal of
      CRInvalid -> status400
      _ -> status409
