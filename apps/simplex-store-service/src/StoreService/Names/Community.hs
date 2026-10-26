{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StoreService.Names.Community
  ( communityFor,
    termsFor,
    agreedTermsHash,
    communityJSON,
    termsJSON,
    TermsError (..),
    termsErrorText,
    currentCommunityTerms,
    nameCommunityTerms,
    Acceptance (..),
    currentAcceptance,
  )
where

import qualified Control.Exception as E
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.IORef (atomicWriteIORef, readIORef)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Network.HTTP.Client (HttpException, Manager, Response (..), brReadSome, parseRequest, withResponse)
import Network.HTTP.Types (statusCode)
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (tshow)
import StoreService.Config (NamesConfig (..), TermsConfig (..))
import StoreService.Names.Abi (bytes32FromHex, hex)
import StoreService.Names.Chain (ChainClient (..), httpFailureDetail)
import StoreService.Names.Contracts (Contracts (..), bonaFideName, communityTermsHash, namePolicy, policyNonce)
import System.Timeout (timeout)

communityFor :: NamesConfig -> LabelHash -> Maybe Text
communityFor NamesConfig {nCommunity} (LabelHash h) = M.lookup h nCommunity

-- | A community name's order signs the community terms' hash after the version, so the order records which text was agreed to.
termsFor :: NamesConfig -> Maybe ByteString -> Text
termsFor NamesConfig {nTerms = TermsConfig {tVersion}} = maybe tVersion (\h -> tVersion <> communityTermsSuffix <> T.drop 2 (hex h))

-- | The community terms hash an order signed, Nothing for the terms of a name not listed.
agreedTermsHash :: Text -> Maybe ByteString
agreedTermsHash terms = case T.breakOn communityTermsSuffix terms of
  (_, rest) | not (T.null rest) -> bytes32FromHex $ "0x" <> T.drop (T.length communityTermsSuffix) rest
  _ -> Nothing

communityTermsSuffix :: Text
communityTermsSuffix = "/community/"

-- | Null without community terms or without community_terms_url to link; else the name's text from the ini, if any, and
-- those terms.
communityJSON :: NamesConfig -> LabelHash -> Maybe ByteString -> J.Value
communityJSON cfg@NamesConfig {nCommunityTermsUrl} lh h = case (h, nCommunityTermsUrl) of
  (Just h', Just url) -> J.object ["for" .= communityFor cfg lh, "terms" .= J.object ["url" .= url, "hash" .= hex h']]
  _ -> J.Null

termsJSON :: NamesConfig -> J.Value
termsJSON NamesConfig {nTerms = TermsConfig {tVersion, tUrl}} = J.object ["version" .= tVersion, "url" .= tUrl]

-- | Far above a page of terms.
maxTermsBytes :: Int
maxTermsBytes = 1024 * 1024

termsTimeout :: Int
termsTimeout = 10000000

-- | TEMismatch: the page's text is not the policy's terms, which only the operator can fix.
data TermsError = TENoTerms | TENoUrl | TEUnread Text | TEMismatch ByteString
  deriving (Eq, Show)

termsErrorText :: TermsError -> Text
termsErrorText = \case
  TENoTerms -> "the name policy has no community terms"
  TENoUrl -> "names.community_terms_url is not set"
  TEUnread e -> e
  TEMismatch h -> "the text at names.community_terms_url does not hash to the name policy's terms " <> hex h

-- | Nothing for a name the policy does not list; else the community terms it is sold under, whose text is checked.
nameCommunityTerms :: NamesConfig -> Contracts -> LabelHash -> IO (Either TermsError (Maybe ByteString))
nameCommunityTerms cfg ct lh = runExceptT $
  withExceptT (TEUnread . tshow) (ExceptT $ bonaFideName ct lh) >>= \case
    False -> pure Nothing
    True -> Just <$> ExceptT (currentCommunityTerms cfg ct)

-- | What the page signs to accept the community terms of a name the policy lists.
data Acceptance = Acceptance {acPolicy :: Address, acTermsHash :: ByteString, acNonce :: Word64}
  deriving (Eq, Show)

-- | The acceptance the owner signs for a name the policy lists, Nothing for another name; the view asks for it and the
-- reveal is checked against it.
currentAcceptance :: NamesConfig -> Contracts -> LabelHash -> Address -> IO (Either Text (Maybe Acceptance))
currentAcceptance cfg ct lh owner = runExceptT $ withExceptT termsErrorText (ExceptT $ nameCommunityTerms cfg ct lh) >>= traverse acceptance
  where
    acceptance acTermsHash = do
      acPolicy <- withExceptT tshow $ ExceptT $ namePolicy ct
      acNonce <- withExceptT tshow $ ExceptT $ policyNonce ct owner
      pure Acceptance {acPolicy, acTermsHash, acNonce}

-- | The policy's community terms hash, once the text at community_terms_url hashes to it. Each call reads the hash and
-- fetches the page until its text hashes to it, so terms set later are checked when a call first reads them.
currentCommunityTerms :: NamesConfig -> Contracts -> IO (Either TermsError ByteString)
currentCommunityTerms NamesConfig {nCommunityTermsUrl} ct = runExceptT $ do
  h <- withExceptT (TEUnread . tshow) $ ExceptT $ communityTermsHash ct
  when (h == B.replicate 32 0) $ throwError TENoTerms
  verified <- liftIO $ readIORef $ ctVerifiedTerms ct
  unless (verified == Just h) $ do
    url <- maybe (throwError TENoUrl) pure nCommunityTermsUrl
    text <- withExceptT TEUnread $ ExceptT $ fetchTerms (ccManager $ ctChain ct) url
    unless (keccak256 text == h) $ throwError $ TEMismatch h
    liftIO $ atomicWriteIORef (ctVerifiedTerms ct) $ Just h
  pure h

fetchTerms :: Manager -> Text -> IO (Either Text ByteString)
fetchTerms manager url =
  timeout termsTimeout (E.try get) >>= \case
    Nothing -> pure $ Left "names.community_terms_url did not answer in time"
    Just (Left (e :: HttpException)) -> pure $ Left $ "names.community_terms_url could not be read: " <> httpFailureDetail (T.unpack url) e
    Just (Right r) -> pure r
  where
    get = do
      req <- parseRequest $ T.unpack url
      withResponse req manager $ \resp -> do
        body <- LB.toStrict <$> brReadSome (responseBody resp) (maxTermsBytes + 1)
        pure $ case statusCode (responseStatus resp) of
          200 | B.length body <= maxTermsBytes -> Right body
          200 -> Left "names.community_terms_url answered more than 1 MiB"
          code -> Left $ "names.community_terms_url answered HTTP " <> tshow code
