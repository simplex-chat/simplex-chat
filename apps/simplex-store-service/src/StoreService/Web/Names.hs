{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Web.Names (Route (..), namesRoutes, namesApp) where

import Data.Text (Text)
import Network.HTTP.Types (Method)
import Network.Wai (Request, ResponseReceived, pathInfo)
import StoreService.Names.Env (NamesEnv (..))
import StoreService.Names.Orders (ordersEnv)
import StoreService.Web.Http (HttpEnv, Limit, Respond, codeLimit, holdMicros, limited, only, previewLimit, readLimit, searchLimit)
import StoreService.Web.Names.Codes (codeCheckHandler)
import StoreService.Web.Names.Orders (codeOrderHandler)
import StoreService.Web.Names.Registration (registrationHandler)
import StoreService.Web.Names.Search (previewHandler, searchHandler)

data Route = Route
  { rMethod :: Method,
    rLimit :: Limit,
    rHandler :: Request -> Respond -> IO ResponseReceived
  }

namesRoutes :: NamesEnv -> [Text] -> Maybe Route
namesRoutes env@NamesEnv {neConfig, neStore, neContracts, neProfiles, neWaiters, neProviders} = \case
  ["api", "name", "register"] -> Just Route {rMethod = "POST", rLimit = codeLimit, rHandler = codeOrderHandler (ordersEnv env) neProviders neWaiters}
  ["api", "code", "check"] -> Just Route {rMethod = "POST", rLimit = codeLimit, rHandler = codeCheckHandler neStore}
  ["api", "link", "preview"] -> Just Route {rMethod = "POST", rLimit = previewLimit, rHandler = previewHandler neProfiles}
  -- below every fixed /api/name/<word> route, which it would otherwise match
  ["api", "name", hash] -> Just Route {rMethod = "GET", rLimit = searchLimit, rHandler = searchHandler neConfig neStore neContracts neProfiles hash}
  ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler (ordersEnv env) holdMicros rid}
  _ -> Nothing

-- | Nothing for a path no route knows. The limit is taken only once the route and the method match.
namesApp :: HttpEnv -> ([Text] -> Maybe Route) -> Request -> Respond -> Maybe (IO ResponseReceived)
namesApp http routes req respond = serve <$> routes (pathInfo req)
  where
    serve Route {rMethod, rLimit, rHandler} =
      only rMethod req respond $ limited http rLimit req respond $ rHandler req respond
