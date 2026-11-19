{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Web.Names.Codes (codeCheckHandler) where

import Control.Logger.Simple (logNote)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, ResponseReceived)
import Simplex.Chat.Badges.Code (parseNameCode)
import Simplex.Messaging.Agent.Store.Common (DBStore)
import Simplex.Messaging.Encoding.String (textEncode)
import StoreService.Names.Codes (codeState, readPrepaidCode)
import StoreService.Names.Store (PrepaidCode (..))
import StoreService.Names.View (NameRefusal (..), refusalResponse)
import StoreService.Store.Invoices (currentSecond)
import StoreService.Web.Http (Respond, jsonResponse, maxBodyBytes, readBoundedBody)

-- | It names no registration, even for a reserved code: holding a code proves nothing about an order.
codeCheckHandler :: DBStore -> Request -> Respond -> IO ResponseReceived
codeCheckHandler st req respond =
  readBoundedBody maxBodyBytes req >>= \case
    Just body -> case codeField body of
      Right text -> case parseNameCode text of
        Just code -> do
          now <- currentSecond
          readPrepaidCode st code >>= respond . maybe (refusalResponse NRCodeInvalid) (codeAnswer now)
        Nothing -> respond $ refusalResponse NRCodeInvalid
      Left e -> logNote ("POST /api/code/check refused: a malformed code check: " <> T.pack e) >> respond (refusalResponse NRBadRequest)
    Nothing -> respond $ refusalResponse NRBadRequest

codeField :: LB.ByteString -> Either String Text
codeField body = J.eitherDecode body >>= JT.parseEither (J.withObject "code check" (.: "code"))

codeAnswer :: UTCTime -> PrepaidCode -> Response
codeAnswer now code@PrepaidCode {pcMinLength, pcYears} =
  jsonResponse status200 $ J.object ["minLength" .= pcMinLength, "years" .= pcYears, "state" .= (textEncode (codeState now code) :: Text)]
