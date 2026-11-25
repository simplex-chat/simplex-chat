{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Web.Names.Registration
  ( registrationHandler,
    revealHandler,
    retryHandler,
    registrationChanged,
    registrationRefusal,
    heldFor,
    versionParam,
    OrderSignature,
    querySignature,
    bodySignature,
    outsideWindow,
    signedRegistration,
  )
where

import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Logger.Simple (logNote)
import Control.Monad (join)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Lazy as LB
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Word (Word64)
import Network.HTTP.Types (status200)
import Network.Wai (Request, ResponseReceived, queryString)
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.SystemTime (roundedToUTCTime)
import Simplex.Messaging.Util (eitherToMaybe, tshow)
import StoreService.Config (boundedDecimal)
import StoreService.Names.Orders (OrderRefusal (..), OrdersEnv (..), WriteRefusal (..), checkSigner, checkTimestamp, noteRead, orderFieldsOf, versionWritten)
import StoreService.Names.Phases (RegistrationId (..), awaitChange, publish)
import StoreService.Names.Reveal (checkReveal, isRepeat, revealSigned)
import StoreService.Names.Signatures (OrderAction (..), OrderFields (..), orderRequestMessage)
import StoreService.Names.Store (BrokenInvariant (..), Commit (..), FailReason (..), NameRegistration (..), OrderPhase (..), getRegistration, noHistory, updateRegistration)
import StoreService.Names.Turns (withRegistrationTurn)
import StoreService.Names.View (NameRefusal (..), RegistrationView, lapseMoment, lapsedAt, registrationVersion, registrationView)
import StoreService.Names.Worker (Retried (..), RetryRefusal (..), retryRegistration)
import StoreService.Web.Http (HttpEnv, Respond, holdsFullSeconds, jsonResponse, maxBodyBytes, readBoundedBody, withHoldSlot)
import StoreService.Web.Names.Orders (orderRefusal, refusalResponse)

-- | GET /api/registration/:id, held while its version is seenVersion.
registrationHandler :: OrdersEnv -> HttpEnv -> Int -> RegistrationId -> Request -> Respond -> IO ResponseReceived
registrationHandler env@OrdersEnv {oeStore, oeConfig, oeContracts, oePhases, oeNow} http hold rid@(RegistrationId ridText) req respond =
  outsideWindow env signature >>= \case
    True -> respond $ refusalResponse NRBadRequest
    False ->
      readNow >>= \case
        Nothing -> respond $ refusalResponse NRNotFound
        Just (now, r@NameRegistration {nrVersion})
          | versionParam req == Just nrVersion ->
              withHoldSlot http (awaitChange oePhases rid (registrationVersion oeStore rid) nrVersion (heldFor hold now $ maybeToList $ lapseMoment r))
                >>= maybe (respond $ refusalResponse $ NRRateLimited holdsFullSeconds) (const $ readNow >>= maybe (respond $ refusalResponse NRNotFound) answer)
          | otherwise -> answer (now, r)
  where
    signature = querySignature req
    readNow = oeNow >>= \now -> signedRegistration env rid ridText OARead signature >>= traverse (\r -> (now, r) <$ noteRead oeStore now r)
    answer (now, r) = respond . jsonResponse status200 . J.toJSON =<< registrationView oeStore oeConfig oeContracts now r

-- | What a reveal decided in the name's turn, with the version it wrote; the publish and the answer come after the turn.
data RevealDecision
  = RDAccepted (Maybe Int64)
  | RDRefused OrderRefusal
  | RDChanged
  | RDGone

-- | POST /api/registration/:id/reveal, its checks in the contract's order, from the stored commitment in the name's turn.
revealHandler :: OrdersEnv -> RegistrationId -> Request -> Respond -> IO ResponseReceived
revealHandler env@OrdersEnv {oeStore, oeConfig, oeContracts, oeTurns, oePhases, oeNow} rid@(RegistrationId ridText) req respond =
  readBoundedBody maxBodyBytes req >>= \case
    Just body -> case J.eitherDecode body of
      Right request ->
        withTransaction oeStore (`getRegistration` rid) >>= \case
          Just r@NameRegistration {nrOwner} ->
            revealSigned oeConfig nrOwner (ofDuration $ orderFieldsOf r) (nrDisputeDays r) request >>= \case
              True -> withRegistrationTurn oeStore oeTurns r (decide request) >>= answer . maybe RDGone (either (const RDChanged) id)
              False -> respond $ refusalResponse NRNotFound
          Nothing -> respond $ refusalResponse NRNotFound
      Left e -> logNote ("POST /api/registration/:id/reveal refused: a malformed reveal: " <> T.pack e) >> respond (refusalResponse NRBadRequest)
    Nothing -> answer $ RDRefused (ORBadRequest "the body is over its size limit")
  where
    decide request r@NameRegistration {nrReveal} = do
      now <- oeNow
      case nrReveal of
        Just stored -> pure $ if isRepeat request stored then RDAccepted Nothing else RDRefused (ORBadRequest "another reveal is stored")
        Nothing -> case nrPhase r of
          OCommitted Commit {cmRevealBefore}
            | lapsedAt now r -> pure $ RDRefused (ORBadRequest "the commitment lapsed")
            | otherwise -> signed now (roundedToUTCTime cmRevealBefore) $ \rv -> r {nrReveal = Just rv}
          OFailed _ FRUnread -> signed now now $ \rv -> r {nrPhase = OQueued, nrHistory = noHistory, nrReveal = Just rv}
          _ -> pure $ RDRefused (ORBadRequest "the order is not committed")
      where
        signed now earliest accept =
          checkReveal oeConfig oeContracts now (nrOwner r) (orderFieldsOf r) (nrDisputeDays r) earliest request >>= \case
            Left refusal -> pure $ RDRefused refusal
            Right rv -> write now (RDAccepted . Just) (accept rv)
    write now decided r =
      withTransaction oeStore (\db -> versionWritten <$> updateRegistration db r now) >>= \case
        Right v -> pure $ decided v
        Left WRChanged -> pure RDChanged
        Left (WRRefused refusal) -> E.throwIO $ BrokenInvariant $ "registration " <> ridText <> " is live while it waits for its owner, yet its reveal was refused: " <> tshow refusal
    answer = \case
      RDAccepted v -> published v >> respond (jsonResponse status200 $ J.object [])
      RDRefused refusal -> do
        logNote ("POST /api/registration/:id/reveal refused: " <> tshow refusal)
        respond $ refusalResponse $ case refusal of
          ORNameCheckUnavailable -> NRProviderUnavailable
          _ -> orderRefusal refusal
      RDChanged -> respond . refusalResponse =<< registrationChanged env rid
      RDGone -> respond $ refusalResponse NRNotFound
    published = mapM_ (atomically . publish oePhases rid)

-- | POST /api/registration/:id/retry, a lapsed order committed again, in the name's turn; the publish and the answer come after the turn.
retryHandler :: OrdersEnv -> RegistrationId -> Request -> Respond -> IO ResponseReceived
retryHandler env@OrdersEnv {oeStore, oeTurns, oePhases} rid@(RegistrationId ridText) req respond = do
  signature <- bodySignature <$> readBoundedBody maxBodyBytes req
  outsideWindow env signature >>= \case
    True -> respond $ refusalResponse NRBadRequest
    False ->
      signedRegistration env rid ridText OARetry signature >>= \case
        Just r -> withRegistrationTurn oeStore oeTurns r (retryRegistration env) >>= maybe (respond $ refusalResponse NRNotFound) (answer . either (const RTChanged) id)
        Nothing -> respond $ refusalResponse NRNotFound
  where
    answer = \case
      RTWritten v -> do
        atomically $ publish oePhases rid v
        respond . maybe (refusalResponse NRNotFound) (jsonResponse status200 . J.toJSON) =<< currentView env rid
      RTChanged -> respond . refusalResponse =<< registrationChanged env rid
      RTRefused refusal -> do
        logNote ("POST /api/registration/:id/retry refused: " <> tshow refusal)
        respond $ refusalResponse $ retryRefusal refusal

retryRefusal :: RetryRefusal -> NameRefusal
retryRefusal = \case
  RFNotLapsed -> NRBadRequest

-- | The registration's current view, for a request that lost the race to change it.
registrationChanged :: OrdersEnv -> RegistrationId -> IO NameRefusal
registrationChanged env = registrationRefusal env NRRegistrationChanged

-- | A refusal with the registration's current view, so the page follows its state.
registrationRefusal :: OrdersEnv -> (RegistrationView -> NameRefusal) -> RegistrationId -> IO NameRefusal
registrationRefusal env refusal rid = maybe NRNotFound refusal <$> currentView env rid

-- | The registration's view as it reads now.
currentView :: OrdersEnv -> RegistrationId -> IO (Maybe RegistrationView)
currentView OrdersEnv {oeStore, oeConfig, oeContracts, oeNow} rid =
  withTransaction oeStore (`getRegistration` rid) >>= traverse (\r -> oeNow >>= \now -> registrationView oeStore oeConfig oeContracts now r)

-- | The hold, cut to end just past the next moment at which the answer changes with time alone.
heldFor :: Int -> UTCTime -> [UTCTime] -> Int
heldFor hold now moments = fromInteger $ minimum $ toInteger hold : [ceiling (diffUTCTime moment now * microsPerSecond) + 1 | moment <- moments, moment >= now]
  where
    microsPerSecond = 1000000

-- | Nothing unless seenVersion is a decimal version, so any other value answers at once.
versionParam :: Request -> Maybe Int64
versionParam req = join (lookup "seenVersion" $ queryString req) >>= eitherToMaybe . parseAll (boundedDecimal 0 maxBound)

data OrderSignature = OrderSignature
  { osTimestamp :: Maybe Word64,
    osSignature :: Maybe RecoverableSignature
  }

querySignature :: Request -> OrderSignature
querySignature req =
  OrderSignature
    { osTimestamp = param "timestamp" >>= eitherToMaybe . parseAll (boundedDecimal 0 maxBound),
      osSignature = param "signature" >>= eitherToMaybe . strDecode
    }
  where
    param k = join $ lookup k $ queryString req

bodySignature :: Maybe LB.ByteString -> OrderSignature
bodySignature body = OrderSignature {osTimestamp = field "timestamp", osSignature = field "signature"}
  where
    object = body >>= J.decode :: Maybe J.Object
    field :: J.FromJSON a => J.Key -> Maybe a
    field k = object >>= KM.lookup k >>= JT.parseMaybe J.parseJSON

outsideWindow :: OrdersEnv -> OrderSignature -> IO Bool
outsideWindow OrdersEnv {oeNow} OrderSignature {osTimestamp} = (\now -> maybe False (isLeft . checkTimestamp now) osTimestamp) <$> oeNow

signedRegistration :: OrdersEnv -> RegistrationId -> Text -> OrderAction -> OrderSignature -> IO (Maybe NameRegistration)
signedRegistration OrdersEnv {oeStore} rid oid action OrderSignature {osTimestamp, osSignature} =
  withTransaction oeStore (`getRegistration` rid) >>= \case
    Just r@NameRegistration {nrOwner}
      | Just timestamp <- osTimestamp,
        Just signature <- osSignature ->
          (\signed -> r <$ eitherToMaybe signed) <$> checkSigner nrOwner (orderRequestMessage oid action timestamp) signature
    _ -> pure Nothing
