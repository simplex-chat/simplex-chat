{-# LANGUAGE LambdaCase #-}
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
    registerRegistration,
    registerExpiredName,
    registerReservedName,
    registerAvailableName,
    unregisterName,
    failNameResolution,
    testPricing,
    emptyNameRecord,
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
import Network.HTTP.Types (hContentType, internalServerError500, notFound404, ok200)
import Network.Wai (Application, pathInfo, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Names.Record (NamePricing (..), NameRecord (..), NameRegistration (..), NameReservedReason, NameResponse (..), USDCents (..))
import Simplex.Messaging.Server.Names (NamesConfig (..))
import Simplex.Messaging.SimplexName (SimplexDomain (..), SimplexNameInfo (..), labelHash)
import Simplex.Messaging.SystemTime (RoundedSystemTime (..), getSystemSeconds)

-- what the test resolver answers for a name: a registration, or a failed request (2h)
data TestNameAnswer = AnswerRegistration NameRegistration | AnswerFails

type NameRegistry = TVar (Map Text TestNameAnswer)

-- | Run an action with a local resolver on a free port and its registry (keyed
-- by the query the resolver looks the name up by).
withNameResolver :: (Int -> NameRegistry -> IO a) -> IO a
withNameResolver action = do
  reg <- newTVarIO M.empty
  Warp.withApplication (pure (app reg)) $ \port -> action port reg
  where
    app :: NameRegistry -> Application
    app reg req send = do
      (st, body) <- case pathInfo req of
        ["health"] -> pure (ok200, "{}")
        ["v2", "resolve", q] -> answer . M.lookup q <$> readTVarIO reg
        _ -> pure (notFound404, "{}")
      send $ responseLBS st [(hContentType, "application/json")] body
    answer = \case
      Just AnswerFails -> (internalServerError500, "{}")
      Just (AnswerRegistration registration) -> (ok200, J.encode NameResponse {lastBlockTs = Nothing, registration})
      Nothing -> (ok200, J.encode NameResponse {lastBlockTs = Nothing, registration = NRAvailable {pricing = testPricing 1}})

-- | Register a name's domain to resolve to the given record, as a live registration.
registerName :: NameRegistry -> SimplexNameInfo -> NameRecord -> IO ()
registerName reg ni nameRecord =
  registerRegistration reg ni NRRegistered {expires = Nothing, graceUntil = Nothing, reservedReason_ = Nothing, nameRecord}

-- | Register any registration the registry could answer with.
registerRegistration :: NameRegistry -> SimplexNameInfo -> NameRegistration -> IO ()
registerRegistration reg ni r = atomically $ modifyTVar' reg $ M.insert (registryKey ni) (AnswerRegistration r)

-- | A name that expired a day ago, renewable by its owner for another 30 days.
registerExpiredName :: NameRegistry -> SimplexNameInfo -> NameRecord -> IO ()
registerExpiredName reg ni nameRecord = do
  RoundedSystemTime now <- getSystemSeconds
  let expires = Just $ RoundedSystemTime (now - 86400)
      graceUntil = Just $ RoundedSystemTime (now + 30 * 86400)
  registerRegistration reg ni NRRegistered {expires, graceUntil, reservedReason_ = Nothing, nameRecord}

-- | A name the registry holds back.
registerReservedName :: NameRegistry -> SimplexNameInfo -> NameReservedReason -> IO ()
registerReservedName reg ni reservedReason = registerRegistration reg ni NRReserved {reservedReason}

-- | A name that is free, priced with the given minimum label length.
registerAvailableName :: NameRegistry -> SimplexNameInfo -> Int -> IO ()
registerAvailableName reg ni minLen = registerRegistration reg ni NRAvailable {pricing = testPricing minLen}

-- | Make the resolver fail for this name, as a registry that is down or erroring would.
failNameResolution :: NameRegistry -> SimplexNameInfo -> IO ()
failNameResolution reg ni = atomically $ modifyTVar' reg $ M.insert (registryKey ni) AnswerFails

-- | Drop a name, so it resolves as available again.
unregisterName :: NameRegistry -> SimplexNameInfo -> IO ()
unregisterName reg ni = atomically $ modifyTVar' reg $ M.delete (registryKey ni)

registryKey :: SimplexNameInfo -> Text
registryKey SimplexNameInfo {nameDomain = SimplexDomain {nameTLD, domain}} =
  decodeLatin1 $ strEncode (labelHash domain) <> strEncode nameTLD

testPricing :: Int -> NamePricing
testPricing minLabelLength = NamePricing {registrationPrices = M.empty, basePrice = USDCents 1000, minLabelLength}

contactNameRecord :: Text -> Text -> NameRecord
contactNameRecord name link = (emptyRecord name) {nrSimplexContact = [link]}

channelNameRecord :: Text -> Text -> NameRecord
channelNameRecord name link = (emptyRecord name) {nrSimplexChannel = [link]}

-- | A record whose domain resolves to both a direct contact link and a channel link.
contactAndChannelNameRecord :: Text -> Text -> Text -> NameRecord
contactAndChannelNameRecord name contactLink channelLink =
  (emptyRecord name) {nrSimplexContact = [contactLink], nrSimplexChannel = [channelLink]}

-- | A registered name whose record holds no usable link.
emptyNameRecord :: Text -> NameRecord
emptyNameRecord = emptyRecord

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
