{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module StoreService.Web.Names.Records (EditInvalid (..), recordsHandler, editRequest, editResponse) where

import Control.Logger.Simple (logNote)
import Control.Monad (forM_, unless)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import Data.List (nub)
import qualified Data.List.NonEmpty as L
import Data.Text (Text)
import Data.Word (Word64)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, ResponseReceived)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.SystemTime (SystemSeconds)
import StoreService.Names.Chain (Hex32 (..), revertText)
import StoreService.Names.Records (EditAnswer (..), EditRequest (..), Records, relayEdit)
import StoreService.Names.Registration (LinkKind, nameLink, parseSignature)
import StoreService.Names.Store (EditOutcome (..), RecordChange (..), SignedChange (..))
import StoreService.Names.View (NameRefusal (..))
import StoreService.Web.Http (Respond, jsonResponse, maxBodyBytes, readBoundedBody)
import StoreService.Web.Names.Orders (refusalResponse)

data EditInvalid = EIBadRequest | EIInvalidLink
  deriving (Eq, Show)

-- | An edit as sent, before its link and signature are checked.
data SentEdit = SentEdit {seKey :: LinkKind, seValue :: Text, seNonce :: Word64, seDeadline :: SystemSeconds, seSignature :: Text}

data SentRequest = SentRequest {srLabelHash :: Hex32, srEdits :: [SentEdit]}

$(JQ.deriveFromJSON defaultJSON {J.fieldLabelModifier = dropPrefix "se"} ''SentEdit)

$(JQ.deriveFromJSON defaultJSON {J.fieldLabelModifier = dropPrefix "sr"} ''SentRequest)

recordsHandler :: Records -> Request -> Respond -> IO ResponseReceived
recordsHandler rs req respond =
  readBoundedBody maxBodyBytes req >>= \case
    Just body | Just o <- J.decode body -> case editRequest o of
      Right edit -> relayEdit rs edit >>= \answer -> logged answer >> respond (editResponse answer)
      Left EIInvalidLink -> respond $ refusalResponse $ NRInvalidLink Nothing
      Left EIBadRequest -> respond $ refusalResponse NRBadRequest
    _ -> respond $ refusalResponse NRBadRequest
  where
    logged = \case
      EARefused reason -> logNote $ "POST /api/name/records refused: " <> reason
      EASettled (EOReverted revert) -> logNote $ "POST /api/name/records reverted on chain" <> maybe "" ((": " <>) . revertText) revert
      _ -> pure ()

-- | One link of each kind, so a Save of both links is the most one request carries.
maxEdits :: Int
maxEdits = 2

-- | Every edit's format and key, then every link, then every signature's format and low s, as the page's checks run.
editRequest :: J.Object -> Either EditInvalid EditRequest
editRequest o = do
  SentRequest {srLabelHash = Hex32 lh, srEdits = edits} <- maybe (Left EIBadRequest) Right $ JT.parseMaybe J.parseJSON $ J.Object o
  sent <- maybe (Left EIBadRequest) Right $ L.nonEmpty edits
  unless (length edits <= maxEdits && keysOnce edits && consecutive edits) $ Left EIBadRequest
  forM_ edits $ \SentEdit {seKey, seValue} -> unless (seValue == "" || (fst <$> nameLink seValue) == Just seKey) $ Left EIInvalidLink
  erChanges <- mapM signedChange sent
  pure EditRequest {erLabelHash = LabelHash lh, erChanges}
  where
    keysOnce edits = let keys = map seKey edits in length keys == length (nub keys)
    consecutive edits = and $ zipWith (\a b -> seNonce b == seNonce a + 1) edits (drop 1 edits)
    signedChange SentEdit {seKey, seValue, seNonce, seDeadline, seSignature} = do
      scSignature <- either (const $ Left EIBadRequest) Right $ parseSignature seSignature
      pure SignedChange {scChange = RCSetText seKey seValue, scNonce = seNonce, scDeadline = seDeadline, scSignature}

editResponse :: EditAnswer -> Response
editResponse = \case
  EASettled (EOApplied tx) -> jsonResponse status200 $ J.object ["tx" .= tx]
  EASettled (EOReverted _) -> refusalResponse NRRecordReverted
  EAPending -> refusalResponse NREditPending
  EARefused _ -> refusalResponse NRBadRequest
  EAStaleNonce -> refusalResponse NRStaleNonce
  EAUnavailable -> refusalResponse NRProviderUnavailable
  EACapped seconds -> refusalResponse $ NRRateLimited seconds
  EALimited nextAt -> refusalResponse $ NREditLimit nextAt

