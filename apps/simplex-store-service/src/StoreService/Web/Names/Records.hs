{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Web.Names.Records (EditInvalid (..), recordsHandler, editRequest, editsLeftJSON) where

import Control.Monad (unless)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types (status200, status409, status429, status504)
import Network.Wai (Request, Response, ResponseReceived)
import Simplex.Messaging.SimplexName (LabelHash (..))
import StoreService.Names.Abi (bytes32FromHex, hex)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Links (LinkKind (..), linkKindOf)
import StoreService.Names.Records (EditAnswer (..), EditRequest (..), EditsLeft (..), Records, relayEdit)
import StoreService.Names.Registration (channelRecordKey, contactRecordKey)
import StoreService.Names.Signatures (parseSignature)
import StoreService.Names.Store (RecordChange (..))
import StoreService.Web.Http (Respond, badRequest, invalidLink, jsonError, jsonResponse, maxBodyBytes, providerUnavailable, rateLimited, readBoundedBody)

data EditInvalid = EIBadRequest | EIInvalidLink
  deriving (Eq, Show)

recordsHandler :: Records -> Request -> Respond -> IO ResponseReceived
recordsHandler rs req respond =
  readBoundedBody maxBodyBytes req >>= \case
    Just body | Just o <- J.decode body -> case editRequest o of
      Right edit -> relayEdit rs edit >>= respond . editResponse
      Left EIInvalidLink -> respond invalidLink
      Left EIBadRequest -> respond badRequest
    _ -> respond badRequest

-- | The formats and the key, then the link, then the signature's format and low s, as the page's checks run.
editRequest :: J.Object -> Either EditInvalid EditRequest
editRequest o = do
  (labelHash, change, erNonce, erDeadline, signature) <- maybe (Left EIBadRequest) Right $ JT.parseMaybe fields o
  erLabelHash <- maybe (Left EIBadRequest) (Right . LabelHash) $ bytes32FromHex labelHash
  erChange <- case change of
    Nothing -> Right RCClear
    Just (key, value) -> do
      kind <- maybe (Left EIBadRequest) Right $ lookup key recordKinds
      unless (value == "" || linkKindOf value == Just kind) $ Left EIInvalidLink
      Right $ RCSetText key value
  erSignature <- either (const $ Left EIBadRequest) Right $ parseSignature signature
  pure EditRequest {erLabelHash, erChange, erNonce, erDeadline, erSignature}
  where
    fields v = do
      clear <- v .:? "clear"
      change <- if clear == Just True then pure Nothing else Just <$> ((,) <$> v .: "key" <*> v .: "value")
      (,,,,) <$> v .: "labelHash" <*> pure change <*> v .: "nonce" <*> v .: "deadline" <*> v .: "signature"

recordKinds :: [(Text, LinkKind)]
recordKinds = [(decodeLatin1 contactRecordKey, LKContact), (decodeLatin1 channelRecordKey, LKChannel)]

editResponse :: EditAnswer -> Response
editResponse = \case
  EAApplied tx -> jsonResponse status200 $ J.object ["tx" .= ((\(TxHash h) -> hex h) <$> tx)]
  EAReverted revert -> jsonResponse status409 $ J.object ["error" .= ("record_reverted" :: Text), "revert" .= revert]
  EAPending -> jsonError status504 "edit_pending"
  EARefused -> badRequest
  EAUnavailable -> providerUnavailable
  EACapped seconds -> rateLimited seconds
  EALimited nextAt -> jsonResponse status429 $ J.object ["error" .= ("edit_limit" :: Text), "nextAt" .= unixSeconds nextAt]

-- | The search answer's recordEdits.
editsLeftJSON :: EditsLeft -> J.Value
editsLeftJSON EditsLeft {elLeft, elNextAt} = J.object ["left" .= elLeft, "nextAt" .= (unixSeconds <$> elNextAt)]

unixSeconds :: UTCTime -> Integer
unixSeconds = floor . utcTimeToPOSIXSeconds
