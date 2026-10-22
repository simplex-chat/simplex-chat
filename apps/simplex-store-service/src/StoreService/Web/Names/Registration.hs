{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Web.Names.Registration
  ( registrationHandler,
    registrationChangedResponse,
    registrationAt,
    registrationVersion,
    lapseMoment,
    heldFor,
    versionParam,
  )
where

import Control.Monad (join)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Types (status200, status409)
import Network.Wai (Request, Response, ResponseReceived, queryString)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import StoreService.Names.Orders (OrdersEnv (..))
import StoreService.Names.Phases (RegistrationId (..), awaitChange)
import StoreService.Names.Store (NamePhase (..), NameRegistration (..), getRegistration)
import StoreService.Names.View (registrationView)
import StoreService.Web.Http (Respond, jsonResponse, notFound)
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
