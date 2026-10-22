{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Web.Names.Search (searchHandler, previewHandler) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (join)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, ResponseReceived, queryString)
import Simplex.Chat.Badges.Code (maxNameLength, minNameLength)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Log (logWarn)
import StoreService.Names.Abi (bytes32FromHex)
import StoreService.Names.Contracts
import StoreService.Names.Links (LinkKind (..), linkKindOf)
import StoreService.Names.Profiles (Entity, Profiles, linkEntity, linkPreview)
import StoreService.Names.Store (isLabelRegistering)
import StoreService.Names.View (tldText)
import StoreService.Web.Http (Respond, badRequest, invalidLink, invalidName, jsonResponse, maxBodyBytes, providerUnavailable, readBoundedBody)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data Found
  = FAvailable Int
  | -- | the chain says available while a registration here has its register sent, not mined
    FRegistering
  | FReserved
  | FRegistered RegisteredName (Maybe NameRecords)

-- | The chain reads of one search share one deadline, so a node that answers slowly fails the search once.
searchHandler :: NamesConfig -> DBStore -> Contracts -> Profiles -> Text -> Request -> Respond -> IO ResponseReceived
searchHandler cfg@NamesConfig {nYears} st ct ps hashText req respond = case LabelHash <$> bytes32FromHex hashText of
  Nothing -> respond badRequest
  Just lh -> case queryLength req of
    Nothing -> respond invalidName
    Just len -> do
      registering <- withTransaction st (`isLabelRegistering` lh)
      timeout (ctDeadline ct) (runExceptT $ found lh len registering) >>= \case
        Just (Right f) -> respond =<< searchAnswer cfg ps f
        Just (Left e) -> unavailable $ tshow e
        Nothing -> unavailable "timed out"
  where
    found lh len registering =
      ExceptT (nameStatus ct lh) >>= \case
        NSAvailable
          | registering -> pure FRegistering
          | otherwise -> FAvailable <$> ExceptT (chainPriceCents ct len nYears)
        NSReserved -> pure FReserved
        NSRegistered name -> FRegistered name <$> ExceptT (nameRecords ct lh name)
    unavailable :: Text -> IO ResponseReceived
    unavailable why = logWarn ("GET /api/name: a chain read failed: " <> why) >> respond providerUnavailable

-- | Nothing unless @length@ is a decimal integer in the label's bounds.
queryLength :: Request -> Maybe Int
queryLength req = case join $ lookup "length" $ queryString req of
  Just s | not (B.null s) && B.all isDigit s -> readMaybe (B.unpack s) >>= inBounds
  _ -> Nothing
  where
    inBounds :: Integer -> Maybe Int
    inBounds n
      | n >= toInteger minNameLength && n <= toInteger maxNameLength = Just $ fromInteger n
      | otherwise = Nothing

searchAnswer :: NamesConfig -> Profiles -> Found -> IO Response
searchAnswer NamesConfig {nTLD, nYears, nTermStart, nChain = ChainConfig {cChainId, cResolver, cSponsor}} ps = \case
  FAvailable price ->
    ok $
      ["status" .= ("available" :: Text), "price" .= price, "currency" .= ("usd" :: Text), "years" .= nYears, "termStart" .= termStart]
        <> ["resolver" .= addressText cResolver, "sponsor" .= addressText cSponsor]
        <> chain
  FRegistering -> ok ["status" .= ("registered" :: Text), "entities" .= ([] :: [Entity]), "tld" .= tldText nTLD]
  FReserved -> ok ["status" .= ("reserved" :: Text)]
  FRegistered RegisteredName {rnOwner, rnResolver, rnExpiresAt, rnGracePeriod} records_ -> do
    entities <- maybe (pure []) (recordEntities ps) records_
    ok $
      ["status" .= ("registered" :: Text), "entities" .= entities, "records" .= (recordsJSON <$> records_)]
        <> ["owner" .= addressText rnOwner, "resolver" .= addressText rnResolver, "recordsNonce" .= (rdNonce <$> records_)]
        <> ["expiresAt" .= rnExpiresAt, "gracePeriod" .= rnGracePeriod]
        <> chain
  where
    ok :: [JT.Pair] -> IO Response
    ok = pure . jsonResponse status200 . J.object
    chain = ["chainId" .= cChainId, "tld" .= tldText nTLD]
    termStart = floor (utcTimeToPOSIXSeconds nTermStart) :: Integer
    recordsJSON NameRecords {rdContact, rdChannel} = J.object ["contact" .= rdContact, "channel" .= rdChannel]

-- | Contact entities first. A record may hold several links joined as the app joins them; a value that is not
-- a short link of its record's kind makes no entity, and neither does a link past the first few of its kind.
recordEntities :: Profiles -> NameRecords -> IO [Entity]
recordEntities ps NameRecords {rdContact, rdChannel} =
  mapConcurrently (uncurry $ linkEntity ps) $ recordLinks LKContact rdContact <> recordLinks LKChannel rdChannel
  where
    recordLinks kind = map (kind,) . take maxEntitiesPerKey . filter ((Just kind ==) . linkKindOf) . map T.strip . T.splitOn linkSeparator

linkSeparator :: Text
linkSeparator = ";"

-- | The links of a record beyond these are not fetched, so a record cannot grow a search's fetches and answer.
maxEntitiesPerKey :: Int
maxEntitiesPerKey = 5

-- | A link whose profile cannot be read is still a valid link: only its kind decides.
previewHandler :: Profiles -> Request -> Respond -> IO ResponseReceived
previewHandler ps req respond =
  readBoundedBody maxBodyBytes req >>= \case
    Just body | Just (J.Object o) <- J.decode body -> case KM.lookup "link" o of
      Just (J.String link) | Just kind <- linkKindOf link -> respond . jsonResponse status200 . J.toJSON =<< linkPreview ps kind link
      _ -> respond invalidLink
    _ -> respond badRequest

addressText :: Address -> Text
addressText = decodeLatin1 . strEncode
