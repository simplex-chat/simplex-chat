{-# LANGUAGE NamedFieldPuns #-}

module StoreService.Web.Names (Route (..), namesRoutes, namesApp) where

import Data.Text (Text)
import Network.HTTP.Types (Method)
import Network.Wai (Request, ResponseReceived, pathInfo)
import StoreService.Names.Env (NamesEnv)
import StoreService.Web.Http (HttpEnv, Limit, Respond, limited, only)

data Route = Route
  { rMethod :: Method,
    rLimit :: Limit,
    rHandler :: Request -> Respond -> IO ResponseReceived
  }

namesRoutes :: NamesEnv -> [Text] -> Maybe Route
namesRoutes _ _ = Nothing

-- | Nothing for a path no route knows. The limit is taken only once the route and the method match.
namesApp :: HttpEnv -> ([Text] -> Maybe Route) -> Request -> Respond -> Maybe (IO ResponseReceived)
namesApp http routes req respond = serve <$> routes (pathInfo req)
  where
    serve Route {rMethod, rLimit, rHandler} =
      only rMethod req respond $ limited http rLimit req respond $ rHandler req respond
