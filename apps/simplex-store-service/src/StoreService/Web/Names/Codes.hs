{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Web.Names.Codes (codeCheckHandler, codeRefusalResponse) where

import Control.Logger.Simple (logNote)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Network.HTTP.Types (status200, status400, status409)
import Network.Wai (Request, Response, ResponseReceived)
import Simplex.Chat.Badges.Code (parseNameCode)
import Simplex.Messaging.Agent.Store.Common (DBStore)
import Simplex.Messaging.Encoding.String (textEncode)
import StoreService.Names.Codes (CodeRefusal (..), codeRefusalTag, codeState, readPrepaidCode)
import StoreService.Names.Store (PrepaidCode (..))
import StoreService.Web.Http (NameRefusal (..), Respond, jsonError, jsonResponse, maxBodyBytes, readBoundedBody, refusalResponse)

-- | It names no registration, even for a reserved code: holding a code proves nothing about an order.
codeCheckHandler :: DBStore -> Request -> Respond -> IO ResponseReceived
codeCheckHandler st req respond =
  readBoundedBody maxBodyBytes req >>= \case
    Just body -> case codeField body of
      Right text -> case parseNameCode text of
        Just code -> do
          now <- getCurrentTime
          readPrepaidCode st code >>= respond . maybe (refusalResponse NRCodeInvalid) (codeAnswer now)
        Nothing -> respond $ refusalResponse NRCodeInvalid
      Left e -> logNote ("POST /api/code/check refused: a malformed code check: " <> T.pack e) >> respond (refusalResponse NRBadRequest)
    Nothing -> respond $ refusalResponse NRBadRequest

codeField :: LB.ByteString -> Either String Text
codeField body = J.eitherDecode body >>= JT.parseEither (J.withObject "code check" (.: "code"))

codeAnswer :: UTCTime -> PrepaidCode -> Response
codeAnswer now code@PrepaidCode {pcMinLength, pcYears} =
  jsonResponse status200 $ J.object ["minLength" .= pcMinLength, "years" .= pcYears, "state" .= (textEncode (codeState now code) :: Text)]

-- | Not 404 for an unknown code, which the page reads as not_found whatever the body.
codeRefusalResponse :: CodeRefusal -> Response
codeRefusalResponse refusal = jsonError status (codeRefusalTag refusal)
  where
    status = case refusal of
      CRInvalid -> status400
      _ -> status409
