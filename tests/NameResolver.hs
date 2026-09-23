{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Local HTTP names resolver for chat tests, copied from simplexmq's
-- NamesResolverServer and made dynamic: it answers /v2/resolve/<query> from a
-- mutable name -> NameRecord registry, so a test can resolve a name to the
-- address it just created.
module NameResolver
  ( NameRegistry,
    withNameResolver,
    registerName,
    ownedName,
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
import Data.Text.Encoding (decodeLatin1)
import Network.HTTP.Types (hContentType, notFound404, ok200)
import Network.Wai (Application, pathInfo, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Names.Record (NamePricing (..), NameRecord (..), NameRegistration (..), NameResponse (..), OwnedNames (..), USDCents (..))
import Simplex.Messaging.Server.Names (NamesConfig (..))
import Simplex.Messaging.SimplexName (SimplexDomain (..), SimplexNameInfo (..), labelHash)

type NameRegistry = TVar (Map Text NameRecord)

-- | Run an action with a local resolver on a free port and its registry (keyed
-- by the query the resolver looks the name up by).
withNameResolver :: (Int -> TVar (Map Text NameRecord) -> IO a) -> IO a
withNameResolver action = do
  reg <- newTVarIO M.empty
  Warp.withApplication (pure (app reg)) $ \port -> action port reg
  where
    app :: TVar (Map Text NameRecord) -> Application
    app reg req send = do
      (st, body) <- case pathInfo req of
        ["health"] -> pure (ok200, "{}")
        ["v2", "resolve", q] -> (\r -> (ok200, J.encode $ nameResponse r)) . M.lookup q <$> readTVarIO reg
        ["v2", "owned-by", addr] -> (\rs -> (ok200, J.encode $ ownedNames addr rs)) . M.elems <$> readTVarIO reg
        _ -> pure (notFound404, "{}")
      send $ responseLBS st [(hContentType, "application/json")] body
    nameResponse (Just nameRecord) = NameResponse {lastBlockTs = Nothing, registration = NRRegistered {expires = Nothing, graceUntil = Nothing, reservedReason_ = Nothing, nameRecord}}
    nameResponse Nothing = NameResponse {lastBlockTs = Nothing, registration = NRAvailable {pricing = NamePricing {registrationPrices = M.empty, basePrice = USDCents 1000, minLabelLength = 1}}}
    -- an account is in use when it owns a name, the only thing this resolver knows about
    ownedNames addr rs =
      let ns = [nameResponse (Just r) | r@NameRecord {nrOwner} <- rs, nrOwner == addr]
       in OwnedNames {ownNames = ns, ownInUse = not (null ns), ownNextOffset = Nothing}

-- | Register a name's domain to resolve to the given record.
registerName :: TVar (Map Text NameRecord) -> SimplexNameInfo -> NameRecord -> IO ()
registerName reg SimplexNameInfo {nameDomain = SimplexDomain {nameTLD, domain}} r =
  atomically $ modifyTVar' reg $ M.insert (decodeLatin1 $ strEncode (labelHash domain) <> strEncode nameTLD) r

-- | Register a name an address owns, for a wallet scan to find. Keyed by the name, not the resolver's lookup key, because owned-by reads the values.
ownedName :: TVar (Map Text NameRecord) -> Text -> Text -> IO ()
ownedName reg name owner = atomically $ modifyTVar' reg $ M.insert name (emptyRecord name) {nrOwner = owner}

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
