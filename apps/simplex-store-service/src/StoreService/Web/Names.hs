{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Web.Names (Route (..), namesRoutes, namesApp) where

import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (Method)
import Network.Wai (Request, ResponseReceived, pathInfo)
import StoreService.Config (ServiceConfig)
import StoreService.Names.Env (NamesEnv (..))
import StoreService.Names.Orders (ordersEnv)
import StoreService.Web.Http (HttpEnv, Limit, Respond, codeLimit, createLimit, findLimit, holdMicros, limited, only, previewLimit, readLimit, recordsLimit, searchLimit)
import StoreService.Web.Names.Codes (codeCheckHandler)
import StoreService.Web.Names.Orders (codeOrderHandler, spendHandler)
import StoreService.Web.Names.Owner (findHandler, retargetHandler)
import StoreService.Web.Names.Records (recordsHandler)
import StoreService.Web.Names.Registration (registrationHandler, retryHandler, revealHandler)
import StoreService.Web.Names.Search (previewHandler, searchHandler)

data Route = Route
  { rMethod :: Method,
    rLimit :: Limit,
    rHandler :: Request -> Respond -> IO ResponseReceived
  }

namesRoutes :: ServiceConfig -> NamesEnv -> [Text] -> Maybe Route
namesRoutes cfg env@NamesEnv {neConfig, neStore, neContracts, neProfiles, neRecords, neWaiters, neProviders} = \case
  ["api", "name", "records"] -> Just Route {rMethod = "POST", rLimit = recordsLimit, rHandler = recordsHandler neRecords}
  ["api", "name", "register"] -> Just Route {rMethod = "POST", rLimit = codeLimit, rHandler = codeOrderHandler (ordersEnv env) neProviders neWaiters}
  ["api", "code", "check"] -> Just Route {rMethod = "POST", rLimit = codeLimit, rHandler = codeCheckHandler neStore}
  ["api", "link", "preview"] -> Just Route {rMethod = "POST", rLimit = previewLimit, rHandler = previewHandler neProfiles}
  -- below every fixed /api/name/<word> route, which it would otherwise match
  ["api", "name", hash] -> Just Route {rMethod = "GET", rLimit = searchLimit, rHandler = searchHandler neConfig neStore neContracts neProfiles getCurrentTime hash}
  -- above /api/registration/:id, which it would otherwise match
  ["api", "registration", "find"] -> Just Route {rMethod = "POST", rLimit = findLimit, rHandler = findHandler (ordersEnv env)}
  ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler (ordersEnv env) holdMicros rid}
  ["api", "registration", rid, "name"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = retargetHandler (ordersEnv env) rid}
  ["api", "registration", rid, "reveal"] -> Just Route {rMethod = "POST", rLimit = readLimit, rHandler = revealHandler (ordersEnv env) rid}
  ["api", "registration", rid, "retry"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = retryHandler (ordersEnv env) rid}
  ["api", "registration", rid, "spend"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = spendHandler (ordersEnv env) cfg neProviders neWaiters rid}
  _ -> Nothing

-- | Nothing for a path no route knows. The limit is taken only once the route and the method match.
namesApp :: HttpEnv -> ([Text] -> Maybe Route) -> Request -> Respond -> Maybe (IO ResponseReceived)
namesApp http routes req respond = serve <$> routes (pathInfo req)
  where
    serve Route {rMethod, rLimit, rHandler} =
      only rMethod req respond $ limited http rLimit req respond $ rHandler req respond
