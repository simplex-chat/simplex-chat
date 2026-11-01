{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Web.Names.Registration
  ( registrationHandler,
    registrationChangedResponse,
    registrationRefusalResponse,
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
import Network.HTTP.Types (status200, status409)
import Network.Wai (Request, Response, ResponseReceived, queryString)
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import StoreService.Names.Orders (OrdersEnv (..))
import StoreService.Names.Phases (RegistrationId (..), awaitChange)
import StoreService.Names.Store (NameRegistration (..), getRegistration)
import StoreService.Names.View (lapseMoment, registrationAt, registrationVersion, registrationView)
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
registrationChangedResponse env = registrationRefusalResponse env "registration_changed"

-- | A refusal with the registration's current view, so the page follows its state.
registrationRefusalResponse :: OrdersEnv -> Text -> RegistrationId -> IO Response
registrationRefusalResponse env tag rid = maybe notFound refusal <$> currentView env rid
  where
    refusal view = jsonResponse status409 $ J.object ["error" .= tag, "registration" .= view]

-- | The registration's view as it reads now.
currentView :: OrdersEnv -> RegistrationId -> IO (Maybe J.Value)
currentView OrdersEnv {oeStore, oeConfig, oeContracts, oeNow} rid =
  withTransaction oeStore (`getRegistration` rid) >>= traverse (\r -> oeNow >>= \now -> registrationView oeStore oeConfig oeContracts (registrationAt now r))

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
