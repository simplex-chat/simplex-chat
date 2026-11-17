{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Web.Names.Records (EditInvalid (..), recordsHandler, editRequest, editResponse) where

import Control.Logger.Simple (logNote)
import Control.Monad (forM_, unless)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Data.List (nub)
import qualified Data.List.NonEmpty as L
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Network.HTTP.Types (status200, status409, status429, status504)
import Network.Wai (Request, Response, ResponseReceived)
import Simplex.Messaging.Encoding.String (strDecode, textDecode)
import Simplex.Messaging.SimplexName (LabelHash (..))
import StoreService.Names.Chain (Hex32 (..))
import StoreService.Names.Records (EditAnswer (..), EditRequest (..), Records, relayEdit)
import StoreService.Names.Registration (LinkKind, nameLink, parseSignature)
import StoreService.Names.Store (EditOutcome (..), RecordChange (..), SignedChange (..))
import StoreService.Store.Invoices (unixSeconds)
import StoreService.Web.Http (Respond, badRequest, invalidLink, jsonError, jsonResponse, maxBodyBytes, providerUnavailable, rateLimited, readBoundedBody)

data EditInvalid = EIBadRequest | EIInvalidLink
  deriving (Eq, Show)

recordsHandler :: Records -> Request -> Respond -> IO ResponseReceived
recordsHandler rs req respond =
  readBoundedBody maxBodyBytes req >>= \case
    Just body | Just o <- J.decode body -> case editRequest o of
      Right edit -> relayEdit rs edit >>= \answer -> logged answer >> respond (editResponse answer)
      Left EIInvalidLink -> respond invalidLink
      Left EIBadRequest -> respond badRequest
    _ -> respond badRequest
  where
    logged = \case
      EARefused reason -> logNote $ "POST /api/name/records refused: " <> reason
      _ -> pure ()

-- | An edit as sent, before its link and signature are checked.
data SentEdit = SentEdit {seChange :: Maybe (LinkKind, Text), seNonce :: Word64, seDeadline :: Word64, seSignature :: Text}

-- | One link of each kind, so a Save of both links is the most one request carries.
maxEdits :: Int
maxEdits = 2

-- | Every edit's format and key, then every link, then every signature's format and low s, as the page's checks run.
editRequest :: J.Object -> Either EditInvalid EditRequest
editRequest o = do
  (labelHash, edits) <- maybe (Left EIBadRequest) Right $ JT.parseMaybe fields o
  erLabelHash <- either (const $ Left EIBadRequest) (Right . LabelHash . unHex32) $ strDecode $ encodeUtf8 labelHash
  sent <- maybe (Left EIBadRequest) Right $ L.nonEmpty edits
  unless (length edits <= maxEdits && keysOnce edits && clearAlone edits && consecutive edits) $ Left EIBadRequest
  forM_ (mapMaybe seChange edits) $ \(kind, value) -> unless (value == "" || (fst <$> nameLink value) == Just kind) $ Left EIInvalidLink
  erChanges <- mapM signedChange sent
  pure EditRequest {erLabelHash, erChanges}
  where
    fields v = (,) <$> v .: "labelHash" <*> (v .: "edits" >>= mapM (J.withObject "edit" sentEdit))
    sentEdit v = do
      clear <- v .:? "clear"
      seChange <- if clear == Just True then pure Nothing else Just <$> ((,) <$> (v .: "key" >>= recordKind) <*> v .: "value")
      SentEdit seChange <$> v .: "nonce" <*> v .: "deadline" <*> v .: "signature"
    recordKind = maybe (fail "an unknown record key") pure . textDecode
    keysOnce edits = let keys = [k | SentEdit {seChange = Just (k, _)} <- edits] in length keys == length (nub keys)
    clearAlone edits = length edits == 1 || all (isJust . seChange) edits
    consecutive edits = and $ zipWith (\a b -> seNonce b == seNonce a + 1) edits (drop 1 edits)
    signedChange SentEdit {seChange, seNonce, seDeadline, seSignature} = do
      scSignature <- either (const $ Left EIBadRequest) Right $ parseSignature seSignature
      pure SignedChange {scChange = maybe RCClear (uncurry RCSetText) seChange, scNonce = seNonce, scDeadline = seDeadline, scSignature}

editResponse :: EditAnswer -> Response
editResponse = \case
  EASettled (EOApplied tx) -> jsonResponse status200 $ J.object ["tx" .= tx]
  EASettled (EOReverted revert) -> jsonResponse status409 $ J.object ["error" .= ("record_reverted" :: Text), "revert" .= revert]
  EAPending -> jsonError status504 "edit_pending"
  EARefused _ -> badRequest
  EAStaleNonce -> jsonError status409 "stale_nonce"
  EAUnavailable -> providerUnavailable
  EACapped seconds -> rateLimited seconds
  EALimited nextAt -> jsonResponse status429 $ J.object ["error" .= ("edit_limit" :: Text), "nextAt" .= unixSeconds nextAt]

