{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Local HTTP names resolver for chat tests, copied from simplexmq's
-- NamesResolverServer and made dynamic: it answers /resolve/<domain> from a
-- mutable name -> NameRecord registry, so a test can resolve a name to the
-- address it just created.
module NameResolver
  ( NameRegistry,
    withNameResolver,
    registerName,
    contactNameRecord,
    channelNameRecord,
    contactAndChannelNameRecord,
    resolverNamesConfig,
  )
where

import Control.Concurrent.STM
import qualified Data.Aeson as J
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Network.HTTP.Types (hContentType, notFound404, ok200)
import Network.Wai (Application, pathInfo, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Messaging.Names.Record (NameRecord (..))
import Simplex.Messaging.Server.Names (NamesConfig (..))
import Simplex.Messaging.SimplexName (SimplexNameInfo (..), fullDomainName)

type NameRegistry = TVar (Map Text NameRecord)

-- | Run an action with a local resolver on a free port and its registry (keyed
-- by full domain name, what the resolver looks the name up by).
withNameResolver :: (Int -> TVar (Map Text NameRecord) -> IO a) -> IO a
withNameResolver action = do
  reg <- newTVarIO M.empty
  Warp.withApplication (pure (app reg)) $ \port -> action port reg
  where
    app :: TVar (Map Text NameRecord) -> Application
    app reg req send = do
      (st, body) <- case pathInfo req of
        ["health"] -> pure (ok200, "{}")
        ["resolve", d] -> maybe (notFound404, "{}") (\r -> (ok200, J.encode r)) . M.lookup d <$> readTVarIO reg
        _ -> pure (notFound404, "{}")
      send $ responseLBS st [(hContentType, "application/json")] body

-- | Register a name's domain to resolve to the given record.
registerName :: TVar (Map Text NameRecord) -> SimplexNameInfo -> NameRecord -> IO ()
registerName reg SimplexNameInfo {nameDomain} r =
  atomically $ modifyTVar' reg $ M.insert (fullDomainName nameDomain) r

contactNameRecord :: Text -> Text -> NameRecord
contactNameRecord name link = (emptyRecord name) {nrSimplexContact = [link]}

channelNameRecord :: Text -> Text -> NameRecord
channelNameRecord name link = (emptyRecord name) {nrSimplexChannel = [link]}

-- | A record whose domain resolves to both a direct contact link and a channel link.
contactAndChannelNameRecord :: Text -> Text -> Text -> NameRecord
contactAndChannelNameRecord name contactLink channelLink =
  (emptyRecord name) {nrSimplexContact = [contactLink], nrSimplexChannel = [channelLink]}

emptyRecord :: Text -> NameRecord
emptyRecord name =
  NameRecord
    { nrName = name,
      nrNickname = "",
      nrWebsite = "",
      nrLocation = "",
      nrSimplexContact = [],
      nrSimplexChannel = [],
      nrEth = Nothing,
      nrBtc = Nothing,
      nrXmr = Nothing,
      nrDot = Nothing,
      nrOwner = "",
      nrResolver = ""
    }

-- | NamesConfig for a chat test SMP server pointing at this resolver.
resolverNamesConfig :: Int -> NamesConfig
resolverNamesConfig port =
  NamesConfig
    { resolverEndpoint = "http://127.0.0.1:" <> show port,
      resolverAuth = Nothing,
      resolverTimeoutMs = 1000,
      resolverMaxResponseBytes = 65536
    }
