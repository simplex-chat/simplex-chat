{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Web.Names.Registration
  ( registrationHandler,
    revealHandler,
    registrationChangedResponse,
    registrationAt,
    registrationVersion,
    lapseMoment,
    heldFor,
    versionParam,
  )
where

import Control.Concurrent.STM (atomically)
import Control.Monad (join)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Types (status200, status409)
import Network.Wai (Request, Response, ResponseReceived, queryString)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Util (tshow)
import StoreService.Log (logInfo)
import StoreService.Names.Orders (OrdersEnv (..))
import StoreService.Names.Phases (RegistrationId (..), awaitChange, publish)
import StoreService.Names.Registration (RegisterIntent (..))
import StoreService.Names.Reveal (RevealFields (..), RevealRefusal (..), SignedLabel (..), checkIntent, committedRegistration, isRepeat, linksValid, parseRevealFields, parseSignedLabel, revealOf)
import StoreService.Names.Store (NamePhase (..), NameRegistration (..), VersionWrite (..), getRegistration, updateRegistration)
import StoreService.Names.Turns (withLabelTurn)
import StoreService.Names.View (registrationView)
import StoreService.Web.Http (Respond, badRequest, invalidLink, jsonResponse, nameBodyBytes, notFound, providerUnavailable, readBoundedBody)
import Text.Read (readMaybe)

-- | GET /api/registration/:id, held while its version is seenVersion.
registrationHandler :: OrdersEnv -> Int -> Text -> Request -> Respond -> IO ResponseReceived
registrationHandler OrdersEnv {oeStore, oeConfig, oeContracts, oePhases, oeNow} hold ridText req respond =
  readNow >>= \case
    Nothing -> respond notFound
    Just (now, r@NameRegistration {nrVersion})
      | versionParam req == Just nrVersion -> do
          _ <- awaitChange oePhases rid (registrationVersion oeStore rid) nrVersion (heldFor hold now $ maybeToList $ lapseMoment r)
          readNow >>= maybe (respond notFound) answer
      | otherwise -> answer (now, r)
  where
    rid = RegistrationId ridText
    readNow = oeNow >>= \now -> fmap (now,) <$> withTransaction oeStore (`getRegistration` rid)
    answer (now, r) = respond . jsonResponse status200 =<< registrationView oeStore oeConfig oeContracts (registrationAt now r)

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
    Just NameRegistration {nrLabelHash} ->
      readBoundedBody nameBodyBytes req >>= \case
        Just body | Just (J.Object o) <- J.decode body, Just signed <- parseSignedLabel o -> withLabelTurn oeTurns nrLabelHash (decide nrLabelHash o signed) >>= answer
        _ -> answer $ RDRefused RVBadRequest Nothing
  where
    rid = RegistrationId ridText
    decide turnHash o signed = do
      now <- oeNow
      withTransaction oeStore (`getRegistration` rid) >>= \case
        Nothing -> pure RDGone
        Just r@NameRegistration {nrLabelHash, nrReveal}
          -- moved to another name while this request waited for the turn
          | nrLabelHash /= turnHash -> pure RDChanged
          | Just stored <- nrReveal -> pure $ if isRepeat signed stored then RDAccepted Nothing else RDRefused RVBadRequest Nothing
          | nrPhase (registrationAt now r) /= PCommitted -> pure $ RDRefused RVBadRequest Nothing
          | otherwise -> case parseRevealFields o of
              Nothing -> pure $ RDRefused RVBadRequest Nothing
              Just fields -> either (\refusal -> pure $ RDRefused refusal Nothing) (signedReveal now r signed fields) $ committedRegistration oeConfig r signed fields
    signedReveal now r SignedLabel {slSignature} RevealFields {rfNonce, rfDeadline} registration = do
      let intent = RegisterIntent {riRegistration = registration, riNonce = rfNonce, riDeadline = rfDeadline, riSignature = slSignature}
      checkIntent oeConfig oeContracts now intent >>= \case
        Left refusal -> pure $ RDRefused refusal Nothing
        Right ()
          | linksValid registration -> write now (RDAccepted . Just) r {nrReveal = Just $ revealOf intent}
          | isJust (nrLinksRefusedAt r) -> pure $ RDRefused RVInvalidLink Nothing
          | otherwise -> write now (RDRefused RVInvalidLink . Just) r {nrLinksRefusedAt = Just now}
    write now decided r =
      withTransaction oeStore (\db -> updateRegistration db r now) >>= \case
        Written v -> pure $ decided v
        _ -> pure RDChanged
    answer = \case
      RDAccepted v -> published v >> respond (jsonResponse status200 $ J.object [])
      RDRefused refusal v -> do
        published v
        logInfo ("POST /api/registration/:id/reveal refused: " <> tshow refusal)
        respond $ case refusal of
          RVBadRequest -> badRequest
          RVInvalidLink -> invalidLink
          RVChainUnavailable -> providerUnavailable
      RDChanged -> respond =<< registrationChangedResponse env rid
      RDGone -> respond notFound
    published = mapM_ (atomically . publish oePhases rid)

-- | The registration's current view, for a request that lost the race to change it.
registrationChangedResponse :: OrdersEnv -> RegistrationId -> IO Response
registrationChangedResponse OrdersEnv {oeStore, oeConfig, oeContracts, oeNow} rid =
  withTransaction oeStore (`getRegistration` rid) >>= \case
    Nothing -> pure notFound
    Just r -> do
      now <- oeNow
      view <- registrationView oeStore oeConfig oeContracts (registrationAt now r)
      pure $ jsonResponse status409 $ J.object ["error" .= ("registration_changed" :: Text), "registration" .= view]

-- | Versions start at 1, so a registration deleted during a hold reads as changed.
registrationVersion :: DBStore -> RegistrationId -> IO Int64
registrationVersion st rid = maybe 0 nrVersion <$> withTransaction st (`getRegistration` rid)

notSignedInTime :: Text
notSignedInTime = "The name was not signed in time"

-- | Past revealBefore with no reveal, the commitment can no longer register, so it reads failed.
registrationAt :: UTCTime -> NameRegistration -> NameRegistration
registrationAt now r = case lapseMoment r of
  Just lapse | now > lapse -> r {nrPhase = PFailed, nrError = Just notSignedInTime}
  _ -> r

lapseMoment :: NameRegistration -> Maybe UTCTime
lapseMoment = \case
  NameRegistration {nrPhase = PCommitted, nrReveal = Nothing, nrRevealBefore = Just before} -> Just $ posixSecondsToUTCTime $ fromIntegral before
  _ -> Nothing

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
