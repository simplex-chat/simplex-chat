{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Web.Names.Search (searchHandler, previewHandler, termsHandler, recordLinks) where

import Control.Concurrent.Async (async, mapConcurrently, wait)
import Control.Logger.Simple (logWarn)
import Control.Monad (join)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Aeson ((.:))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import Data.List (find)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import qualified Data.Text as T
import Data.Word (Word64)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, ResponseReceived, queryString)
import Simplex.Chat.Badges.Code (NameLength, nameLength)
import Simplex.Messaging.Agent.Protocol (ConnShortLink, ConnectionMode (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, parseAll, taggedObjectJSON)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (eitherToMaybe, tshow)
import StoreService.Config (NamesConfig (..), boundedDecimal)
import StoreService.Names.Chain (ChainSeconds, Hex32 (..))
import StoreService.Names.Community (Community, TermsLink, communityOf, readableTerms, termsLink, yearsFor)
import StoreService.Names.Contracts
import StoreService.Names.Profiles (Entity, Profiles, linkEntity, linkPreview)
import StoreService.Names.Records (EditsLeft, nameEditsLeft)
import StoreService.Names.Registration (CommunityName, LinkKind (..), nameLink)
import StoreService.Names.Store (labelHolder)
import StoreService.Names.View (NameRefusal (..), refusalResponse)
import StoreService.Web.Http (HttpEnv, Respond, jsonResponse, maxBodyBytes, readBoundedBody, searchSlot)
import System.Timeout (timeout)

data Found
  = FAvailable USDCents (Maybe Terms)
  | FRegistering
  | FReserved
  | FRegistered RegisteredName (Maybe NameRecords)

data SearchAnswer = SearchAnswer {found :: NameFound, community :: Maybe Community}

data NameFound
  = NFAvailable {price :: USDCents, years :: Int, terms :: Maybe TermsLink}
  | NFRegistering
  | NFReserved
  | NFRegistered {owner :: Address, resolver :: Address, expiresAt :: ChainSeconds, gracePeriod :: Integer, entities :: [Entity], records :: ResolverRecords}

data ResolverRecords
  = RROwn {contact :: Text, channel :: Text, nonce :: Word64, edits :: EditsLeft}
  | RROther

data TermsText = TermsText {link :: Text, text :: Text}

$(JQ.deriveToJSON (taggedObjectJSON $ dropPrefix "RR") ''ResolverRecords)

$(JQ.deriveToJSON (taggedObjectJSON $ dropPrefix "NF") ''NameFound)

$(JQ.deriveToJSON defaultJSON ''SearchAnswer)

$(JQ.deriveToJSON defaultJSON ''TermsText)

-- | The chain reads of one search share one deadline, so a node that answers slowly fails the search once.
-- Only they hold a search slot: the profile reads after them have their own bound.
searchHandler :: NamesConfig -> HttpEnv -> DBStore -> Contracts -> Profiles -> IO UTCTime -> Text -> Request -> Respond -> IO ResponseReceived
searchHandler cfg@NamesConfig {nYears} http st ct ps clock hashText req respond = case LabelHash . unHex32 <$> strDecode (encodeUtf8 hashText) of
  Left _ -> respond $ refusalResponse NRBadRequest
  Right lh -> case queryLength req of
    Nothing -> respond $ refusalResponse NRInvalidName
    Just len -> do
      registering <- isJust <$> withTransaction st (`labelHolder` lh)
      searchSlot http (timeout (ctDeadline ct) (runExceptT $ found lh len registering)) >>= \case
        Nothing -> respond $ refusalResponse $ NRRateLimited 1
        Just (Just (Right (f, community))) -> respond =<< searchAnswer cfg st ps clock lh community f
        Just (Just (Left e)) -> unavailable $ tshow e
        Just Nothing -> unavailable "timed out"
  where
    found lh len registering = do
      (status, community) <- ExceptT $ together $ (,) <$> concurrent (nameStatus ct lh) <*> concurrent (communityName ct lh)
      (,community) <$> case status of
        NSAvailable
          | registering -> pure FRegistering
          | otherwise -> ExceptT $ together $ FAvailable <$> concurrent (chainPriceCents ct len (yearsFor community nYears)) <*> concurrent (readableNow community)
        NSReserved -> pure FReserved
        NSRegistered name -> FRegistered name <$> ExceptT (nameRecords ct lh name)
    readableNow community = termsLinks ct >>= traverse (\links -> eitherToMaybe <$> (wait =<< async (readableTerms ct $ kindTerms community links)))
    unavailable :: Text -> IO ResponseReceived
    unavailable why = logWarn ("GET /api/name: a chain read failed: " <> why) >> respond (refusalResponse NRProviderUnavailable)

-- | Nothing unless @length@ is a decimal integer in the label's bounds.
queryLength :: Request -> Maybe NameLength
queryLength req = join (lookup "length" $ queryString req) >>= eitherToMaybe . parseAll (boundedDecimal 0 maxBound) >>= nameLength

searchAnswer :: NamesConfig -> DBStore -> Profiles -> IO UTCTime -> LabelHash -> CommunityName -> Found -> IO Response
searchAnswer cfg@NamesConfig {nYears, nRecordEditsPerYear} st ps clock lh community f = do
  found <- case f of
    FAvailable price readable -> pure NFAvailable {price, years = yearsFor community nYears, terms = termsLink <$> readable}
    FRegistering -> pure NFRegistering
    FReserved -> pure NFReserved
    FRegistered RegisteredName {rnOwner, rnResolver, rnExpiresAt, rnGracePeriod} records_ -> do
      entities <- maybe (pure []) (recordEntities ps) records_
      -- only a name on the default resolver is edited here
      records <- case records_ of
        Just NameRecords {rdContact, rdChannel, rdNonce} -> (\edits -> RROwn {contact = rdContact, channel = rdChannel, nonce = rdNonce, edits}) <$> editsLeft
        Nothing -> pure RROther
      pure NFRegistered {owner = rnOwner, resolver = rnResolver, expiresAt = rnExpiresAt, gracePeriod = rnGracePeriod, entities, records}
  pure $ jsonResponse status200 SearchAnswer {found, community = communityOf cfg lh community}
  where
    editsLeft = clock >>= \now -> withTransaction st $ \db -> nameEditsLeft db nRecordEditsPerYear lh now

-- | Contact entities first. A record may hold several links joined as the app joins them; a value that is not
-- a short link of its record's kind makes no entity, and neither does a link past the first few of its kind.
recordEntities :: Profiles -> NameRecords -> IO [Entity]
recordEntities ps NameRecords {rdContact, rdChannel} =
  mapConcurrently (uncurry $ linkEntity ps) $ recordLinks LKContact rdContact <> recordLinks LKChannel rdChannel

recordLinks :: LinkKind -> Text -> [(LinkKind, ConnShortLink 'CMContact)]
recordLinks kind = take maxEntitiesPerKey . filter ((kind ==) . fst) . mapMaybe (nameLink . T.strip) . T.splitOn linkSeparator

linkSeparator :: Text
linkSeparator = ";"

-- | The links of a record beyond these are not fetched, so a record cannot grow a search's fetches and answer.
maxEntitiesPerKey :: Int
maxEntitiesPerKey = 5

-- | A link whose profile cannot be read is still a valid link: only its kind decides.
previewHandler :: Profiles -> Request -> Respond -> IO ResponseReceived
previewHandler ps req respond =
  readBoundedBody maxBodyBytes req >>= \case
    Just body | Just (J.Object o) <- J.decode body -> case JT.parseEither (.: "link") o of
      Right link | Just (kind, sl) <- nameLink link -> respond . jsonResponse status200 . J.toJSON =<< linkPreview ps kind sl
      _ -> respond $ refusalResponse $ NRInvalidLink Nothing
    _ -> respond $ refusalResponse NRBadRequest

-- | The text of either current set of terms, by its hash, with its link; any other hash answers not_found.
termsHandler :: Contracts -> Text -> Request -> Respond -> IO ResponseReceived
termsHandler ct hashText _ respond = case strDecode $ encodeUtf8 hashText of
  Left _ -> respond $ refusalResponse NRBadRequest
  Right h ->
    termsLinks ct >>= \case
      Left e -> logWarn ("GET /api/terms: a chain read failed: " <> tshow e) >> respond (refusalResponse NRProviderUnavailable)
      Right (general, community) -> case find ((h ==) . tmHash) $ catMaybes [general, community] of
        Nothing -> respond $ refusalResponse NRNotFound
        Just t@Terms {tmLink} ->
          termsText ct t >>= \case
            Right text -> respond $ jsonResponse status200 TermsText {link = tmLink, text}
            Left (TRWait seconds) -> respond $ refusalResponse $ NRTermsUnavailable $ Just seconds
