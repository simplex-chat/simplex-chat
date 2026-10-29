{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Web.Names.Records (EditInvalid (..), recordsHandler, editRequest, editsLeftJSON) where

import Control.Monad (forM_, unless)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Data.List (nub)
import qualified Data.List.NonEmpty as L
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Network.HTTP.Types (status200, status409, status429, status504)
import Network.Wai (Request, Response, ResponseReceived)
import Simplex.Messaging.SimplexName (LabelHash (..))
import StoreService.Names.Abi (bytes32FromHex, hex)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Links (LinkKind (..), linkKindOf)
import StoreService.Names.Records (EditAnswer (..), EditRequest (..), EditsLeft (..), Records, relayEdit)
import StoreService.Names.Registration (channelRecordKey, contactRecordKey)
import StoreService.Names.Signatures (parseSignature)
import StoreService.Names.Store (RecordChange (..), SignedChange (..))
import StoreService.Web.Http (Respond, badRequest, invalidLink, jsonError, jsonResponse, nameBodyBytes, providerUnavailable, rateLimited, readBoundedBody)

data EditInvalid = EIBadRequest | EIInvalidLink
  deriving (Eq, Show)

recordsHandler :: Records -> Request -> Respond -> IO ResponseReceived
recordsHandler rs req respond =
  readBoundedBody nameBodyBytes req >>= \case
    Just body | Just o <- J.decode body -> case editRequest o of
      Right edit -> relayEdit rs edit >>= respond . editResponse
      Left EIInvalidLink -> respond invalidLink
      Left EIBadRequest -> respond badRequest
    _ -> respond badRequest

-- | An edit as sent, before its key, link and signature are checked.
data SentEdit = SentEdit {seChange :: Maybe (Text, Text), seNonce :: Word64, seDeadline :: Word64, seSignature :: Text}

-- | One link of each kind, so a Save of both links is the most one request carries.
maxEdits :: Int
maxEdits = 2

-- | Every edit's format and key, then every link, then every signature's format and low s, as the page's checks run.
editRequest :: J.Object -> Either EditInvalid EditRequest
editRequest o = do
  (labelHash, edits) <- maybe (Left EIBadRequest) Right $ JT.parseMaybe fields o
  erLabelHash <- maybe (Left EIBadRequest) (Right . LabelHash) $ bytes32FromHex labelHash
  sent <- maybe (Left EIBadRequest) Right $ L.nonEmpty edits
  unless (length edits <= maxEdits && keysOnce edits && clearAlone edits && consecutive edits) $ Left EIBadRequest
  links <- mapM (\SentEdit {seChange} -> mapM linkOf seChange) sent
  forM_ links $ mapM_ $ \(kind, value) -> unless (value == "" || linkKindOf value == Just kind) $ Left EIInvalidLink
  erChanges <- mapM signedChange sent
  pure EditRequest {erLabelHash, erChanges}
  where
    fields v = (,) <$> v .: "labelHash" <*> (v .: "edits" >>= mapM (J.withObject "edit" sentEdit))
    sentEdit v = do
      clear <- v .:? "clear"
      seChange <- if clear == Just True then pure Nothing else Just <$> ((,) <$> v .: "key" <*> v .: "value")
      SentEdit seChange <$> v .: "nonce" <*> v .: "deadline" <*> v .: "signature"
    keysOnce edits = let keys = [k | SentEdit {seChange = Just (k, _)} <- edits] in length keys == length (nub keys)
    clearAlone edits = length edits == 1 || all (isJust . seChange) edits
    consecutive edits = and $ zipWith (\a b -> seNonce b == seNonce a + 1) edits (drop 1 edits)
    linkOf (key, value) = maybe (Left EIBadRequest) (Right . (,value)) $ lookup key recordKinds
    signedChange SentEdit {seChange, seNonce, seDeadline, seSignature} = do
      scSignature <- either (const $ Left EIBadRequest) Right $ parseSignature seSignature
      pure SignedChange {scChange = maybe RCClear (uncurry RCSetText) seChange, scNonce = seNonce, scDeadline = seDeadline, scSignature}

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
