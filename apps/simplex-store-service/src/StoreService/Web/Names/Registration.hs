{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Web.Names.Registration
  ( registrationHandler,
    revealHandler,
    retryHandler,
    registrationChangedResponse,
    registrationRefusalResponse,
    retryRefusalResponse,
    heldFor,
    versionParam,
  )
where

import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Logger.Simple (logNote)
import Control.Monad (join)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Network.HTTP.Types (status200, status409)
import Network.Wai (Request, Response, ResponseReceived, queryString)
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Util (tshow)
import StoreService.Names.Orders (OrdersEnv (..), WriteRefusal (..), versionWritten)
import StoreService.Names.Phases (RegistrationId (..), awaitChange, publish)
import StoreService.Names.Reveal (RevealRefusal (..), checkReveal, isRepeat, linksValid)
import StoreService.Names.Store (BrokenInvariant (..), NameRegistration (..), OrderPhase (..), getRegistration, updateRegistration)
import StoreService.Names.Turns (withRegistrationTurn)
import StoreService.Names.View (lapseMoment, registrationAt, registrationVersion, registrationView)
import StoreService.Names.Worker (Retried (..), RetryRefusal (..), retryRegistration)
import StoreService.Web.Http (HttpEnv, Respond, badRequest, holdsFull, invalidLink, jsonResponse, nameBodyBytes, notFound, providerUnavailable, readBoundedBody, withHoldSlot)
import StoreService.Web.Names.Codes (codeRefusalResponse)
import Text.Read (readMaybe)

-- | GET /api/registration/:id, held while its version is seenVersion.
registrationHandler :: OrdersEnv -> HttpEnv -> Int -> Text -> Request -> Respond -> IO ResponseReceived
registrationHandler OrdersEnv {oeStore, oeConfig, oeContracts, oePhases, oeNow} http hold ridText req respond =
  readNow >>= \case
    Nothing -> respond notFound
    Just (now, r@NameRegistration {nrVersion})
      | versionParam req == Just nrVersion ->
          withHoldSlot http (awaitChange oePhases rid (registrationVersion oeStore rid) nrVersion (heldFor hold now $ maybeToList $ lapseMoment r))
            >>= maybe (respond holdsFull) (const $ readNow >>= maybe (respond notFound) answer)
      | otherwise -> answer (now, r)
  where
    rid = RegistrationId ridText
    readNow = oeNow >>= \now -> fmap (now,) <$> withTransaction oeStore (`getRegistration` rid)
    answer (now, r) = respond . jsonResponse status200 =<< registrationView oeStore oeConfig oeContracts now (registrationAt now r)

-- | What a reveal decided in the name's turn, with the version it wrote; the publish and the answer come after the turn.
data RevealDecision
  = RDAccepted (Maybe Int64)
  | RDRefused RevealRefusal (Maybe Int64)
  | RDChanged
  | RDGone

-- | POST /api/registration/:id/reveal, its checks in the contract's order, from the stored commitment in the name's turn.
revealHandler :: OrdersEnv -> Text -> Request -> Respond -> IO ResponseReceived
revealHandler env@OrdersEnv {oeStore, oeConfig, oeContracts, oeTurns, oePhases, oeNow} ridText req respond =
  withTransaction oeStore (`getRegistration` rid) >>= \case
    Nothing -> respond notFound
    Just r ->
      readBoundedBody nameBodyBytes req >>= \case
        Just body | Just request <- J.decode body ->
          withRegistrationTurn oeStore oeTurns r (decide request) >>= answer . maybe RDGone (either (const RDChanged) id)
        _ -> answer $ RDRefused RVBadRequest Nothing
  where
    rid = RegistrationId ridText
    decide request r@NameRegistration {nrReveal} = do
      now <- oeNow
      case nrReveal of
        Just stored -> pure $ if isRepeat request stored then RDAccepted Nothing else RDRefused RVBadRequest Nothing
        Nothing
          | nrPhase (registrationAt now r) /= OCommitted -> pure $ RDRefused RVBadRequest Nothing
          | otherwise ->
              checkReveal oeConfig oeContracts now r request >>= \case
                Left refusal -> pure $ RDRefused refusal Nothing
                Right rv
                  | linksValid rv -> write now (RDAccepted . Just) r {nrReveal = Just rv}
                  | isJust (nrLinksRefusedAt r) -> pure $ RDRefused RVInvalidLink Nothing
                  | otherwise -> write now (RDRefused RVInvalidLink . Just) r {nrLinksRefusedAt = Just now}
    write now decided r =
      withTransaction oeStore (\db -> versionWritten <$> updateRegistration db r now) >>= \case
        Right v -> pure $ decided v
        Left WRChanged -> pure RDChanged
        Left (WRRefused refusal) -> E.throwIO $ BrokenInvariant $ "registration " <> ridText <> " is live while committed, yet its reveal was refused: " <> tshow refusal
    answer = \case
      RDAccepted v -> published v >> respond (jsonResponse status200 $ J.object [])
      RDRefused refusal v -> do
        published v
        logNote ("POST /api/registration/:id/reveal refused: " <> tshow refusal)
        respond $ case refusal of
          RVBadRequest -> badRequest
          RVInvalidLink -> invalidLink
          RVChainUnavailable -> providerUnavailable
      RDChanged -> respond =<< registrationChangedResponse env rid
      RDGone -> respond notFound
    published = mapM_ (atomically . publish oePhases rid)

-- | POST /api/registration/:id/retry, Try again in the name's turn; the publish and the answer come after the turn.
retryHandler :: OrdersEnv -> Text -> Request -> Respond -> IO ResponseReceived
retryHandler env@OrdersEnv {oeStore, oeTurns, oePhases} ridText _ respond =
  withTransaction oeStore (`getRegistration` rid) >>= \case
    Nothing -> respond notFound
    Just r -> withRegistrationTurn oeStore oeTurns r (retryRegistration env) >>= maybe (respond notFound) (answer . either (const RTChanged) id)
  where
    rid = RegistrationId ridText
    answer = \case
      RTWritten v -> do
        atomically $ publish oePhases rid v
        respond . maybe notFound (jsonResponse status200) =<< currentView env rid
      RTChanged -> respond =<< registrationChangedResponse env rid
      RTRefused refusal -> do
        logNote ("POST /api/registration/:id/retry refused: " <> tshow refusal)
        respond $ retryRefusalResponse refusal

retryRefusalResponse :: RetryRefusal -> Response
retryRefusalResponse = \case
  RFNotFailed -> badRequest
  RFLinksRefused -> badRequest
  RFSendsUsedUp -> badRequest
  RFCode code -> codeRefusalResponse code
  RFUnread -> providerUnavailable

-- | The registration's current view, for a request that lost the race to change it.
registrationChangedResponse :: OrdersEnv -> RegistrationId -> IO Response
registrationChangedResponse env = registrationRefusalResponse env "registration_changed"

-- | A refusal with the registration's current view, so the page follows its state.
registrationRefusalResponse :: OrdersEnv -> Text -> RegistrationId -> IO Response
registrationRefusalResponse env tag rid = maybe notFound refusal <$> currentView env rid
  where
    refusal view = jsonResponse status409 $ J.object ["error" .= tag, "registration" .= view]

-- | The registration's view as it reads now.
currentView :: OrdersEnv -> RegistrationId -> IO (Maybe J.Value)
currentView OrdersEnv {oeStore, oeConfig, oeContracts, oeNow} rid =
  withTransaction oeStore (`getRegistration` rid) >>= traverse (\r -> oeNow >>= \now -> registrationView oeStore oeConfig oeContracts now (registrationAt now r))

-- | The hold, cut to end just past the next moment at which the answer changes with time alone.
heldFor :: Int -> UTCTime -> [UTCTime] -> Int
heldFor hold now moments = fromInteger $ minimum $ toInteger hold : [ceiling (diffUTCTime moment now * microsPerSecond) + 1 | moment <- moments, moment >= now]
  where
    microsPerSecond = 1000000

-- | Nothing unless seenVersion is a decimal version, so any other value answers at once.
versionParam :: Request -> Maybe Int64
versionParam req = case join $ lookup "seenVersion" $ queryString req of
  Just s | not (B.null s) && B.all isDigit s -> readMaybe (B.unpack s) >>= inRange
  _ -> Nothing
  where
    inRange :: Integer -> Maybe Int64
    inRange v = if v <= toInteger (maxBound :: Int64) then Just (fromInteger v) else Nothing
