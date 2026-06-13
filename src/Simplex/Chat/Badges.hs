{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Badges
  ( BadgeType (..),
    BadgeStatus (..),
    BadgeInfo (..),
    BadgeCrypto (..),
    Badge (..),
    LocalBadge (..),
    JSONBadge (..),
    JBadge (..),
    BBSPublicKeyStr (..),
    localBadgeInfo,
    localBadgeStatus,
    badgeKeyIndex,
    BadgePresHeader (..),
    badgePresHeaderBytes,
    toBadgePresHeader,
    BadgePurchase (..),
    BadgeMasterKey (..),
    BadgeRequest (..),
    VerifiedBadgeRequest (..),
    bbsBadgeHeader,
    generateMasterKey,
    verifyPayment,
    issueBadge,
    verifyCredential,
    generateBadgeProof,
    badgeProof,
    verifyBadge,
    verifyBadge_,
    mkBadgeStatus,
    BadgeRow,
    badgeToRow,
    localBadgeToRow,
    rowToBadge,
  ) where

import Control.Concurrent.STM
import Crypto.Random (ChaChaDRG)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.TH as JQ
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as U
import qualified Data.ByteString.Char8 as BC
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, nominalDay)
import Data.Word (Word8)
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..), fromTextField_)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, sumTypeJSON)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
#else
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
#endif

-- Badge type

data BadgeType
  = BTSupporter
  | BTLegend
  | BTInvestor
  | BTUnknown Text
  deriving (Eq, Show)

instance TextEncoding BadgeType where
  textEncode = \case
    BTSupporter -> "supporter"
    BTLegend -> "legend"
    BTInvestor -> "investor"
    BTUnknown tag -> tag
  textDecode s = Just $ case s of
    "supporter" -> BTSupporter
    "legend" -> BTLegend
    "investor" -> BTInvestor
    tag -> BTUnknown tag

instance ToJSON BadgeType where
  toJSON = textToJSON
  toEncoding = textToEncoding

instance FromJSON BadgeType where
  parseJSON = textParseJSON "BadgeType"

-- Badge status

data BadgeStatus = BSActive | BSExpired | BSExpiredOld | BSFailed | BSUnknownKey
  deriving (Eq, Show)

-- Disclosed badge content (BBS messages 1, 2, 3)

data BadgeInfo = BadgeInfo
  { badgeType :: BadgeType,
    badgeExpiry :: Maybe UTCTime,
    badgeExtra :: Text
  }
  deriving (Eq, Show)

-- a badge expired longer than this ago is BSExpiredOld and is not shown in the UI
badgeOldInterval :: NominalDiffTime
badgeOldInterval = 31 * nominalDay

-- the verification outcome of a received proof: Just True = verified, Just False = failed,
-- Nothing = the proof's key index is not among this app version's configured keys (BSUnknownKey).
mkBadgeStatus :: UTCTime -> Maybe Bool -> BadgeInfo -> BadgeStatus
mkBadgeStatus now verified BadgeInfo {badgeExpiry} = case verified of
  Nothing -> BSUnknownKey
  Just False -> BSFailed
  Just True -> case badgeExpiry of
    Just e
      | addUTCTime badgeOldInterval e < now -> BSExpiredOld
      | e < now -> BSExpired
    _ -> BSActive

-- Badge crypto: a credential (own, secret) or a proof (wire, a presentation).
-- Positional GADT - a record field cannot be shared across constructors with different result types.
-- The leading Int is the issuer key index: it tells verifiers which configured key to use (see BadgeKey).

data BadgeCrypto = BCCredential | BCProof

data Badge (b :: BadgeCrypto) where
  BadgeCredential :: Int -> BadgeMasterKey -> BBSSignature -> BadgeInfo -> Badge 'BCCredential
  BadgeProof :: Int -> BBSPresHeader -> BBSProof -> BadgeInfo -> Badge 'BCProof

deriving instance Show (Badge b)

deriving instance Eq (Badge 'BCCredential)

deriving instance Eq (Badge 'BCProof)

badgeKeyIndex :: Badge b -> Int
badgeKeyIndex = \case
  BadgeCredential idx _ _ _ -> idx
  BadgeProof idx _ _ _ -> idx

-- Local badge: a stored badge plus its display status.
--   OwnBadge   - the user's own credential (loaded from the DB).
--   PeerBadge  - a verified peer proof (from the DB, or received over the wire).
--   ShownBadge - decoded from a crypto-free profile JSON for display only: no crypto, so it cannot be sent.
data LocalBadge
  = OwnBadge (Badge 'BCCredential) BadgeStatus
  | PeerBadge (Badge 'BCProof) BadgeStatus
  | ShownBadge BadgeInfo BadgeStatus
  deriving (Eq, Show)

localBadgeInfo :: LocalBadge -> BadgeInfo
localBadgeInfo = \case
  OwnBadge (BadgeCredential _ _ _ i) _ -> i
  PeerBadge (BadgeProof _ _ _ i) _ -> i
  ShownBadge i _ -> i

localBadgeStatus :: LocalBadge -> BadgeStatus
localBadgeStatus = \case
  OwnBadge _ st -> st
  PeerBadge _ st -> st
  ShownBadge _ st -> st

-- Presentation header: unbound test marker (stable) or forward-compat catch-all (master variants)

data BadgePresHeader
  = PHTest
  | PHUnknown Word8 ByteString

badgePresHeaderBytes :: BadgePresHeader -> ByteString
badgePresHeaderBytes = \case
  PHTest -> B.singleton 0
  PHUnknown t b -> B.cons t b

toBadgePresHeader :: ByteString -> BadgePresHeader
toBadgePresHeader bs = case B.uncons bs of
  Just (0, _) -> PHTest
  Just (t, b) -> PHUnknown t b
  Nothing -> PHUnknown 0 ""

-- stable accepts both; master rejects PHTest
badgePresHeaderAccepted :: BadgePresHeader -> Bool
badgePresHeaderAccepted = \case
  PHTest -> True
  PHUnknown _ _ -> True

-- Payment proof

data BadgePurchase
  = BPAppleReceipt Text
  | BPGoogleReceipt Text
  | BPStripeSession
  | BPRedeemCode Text
  deriving (Eq, Show)

-- Master key

newtype BadgeMasterKey = BadgeMasterKey ByteString
  deriving newtype (Eq, Show, StrEncoding)

instance ToJSON BadgeMasterKey where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromJSON BadgeMasterKey where
  parseJSON = strParseJSON "BadgeMasterKey"

generateMasterKey :: TVar ChaChaDRG -> IO BadgeMasterKey
generateMasterKey drg = BadgeMasterKey <$> atomically (C.randomBytes 32 drg)

-- Workflow types

data BadgeRequest = BadgeRequest
  { masterKey :: BadgeMasterKey,
    badgeInfo :: BadgeInfo
  }
  deriving (Show)

newtype VerifiedBadgeRequest = VerifiedBadgeRequest BadgeRequest
  deriving (Show)

-- Constants

bbsBadgeHeader :: BBSHeader
bbsBadgeHeader = BBSHeader "SimpleX badges v1"

bbsBadgeMessageCount :: Int
bbsBadgeMessageCount = 4

bbsBadgeDisclosedIndexes :: [Int]
bbsBadgeDisclosedIndexes = [1, 2, 3]

-- Message encoding

encodeExpiry :: Maybe UTCTime -> ByteString
encodeExpiry = maybe "lifetime" strEncode

badgeMessages :: BadgeMasterKey -> BadgeInfo -> [ByteString]
badgeMessages (BadgeMasterKey ms) info = ms : badgeInfoMessages info

badgeInfoMessages :: BadgeInfo -> [ByteString]
badgeInfoMessages BadgeInfo {badgeType, badgeExpiry, badgeExtra} =
  [encodeExpiry badgeExpiry, encodeUtf8 (textEncode badgeType), encodeUtf8 badgeExtra]

-- Payment verification (stub - always passes)

verifyPayment :: BadgePurchase -> BadgeRequest -> IO (Maybe VerifiedBadgeRequest)
verifyPayment _payment req = pure $ Just (VerifiedBadgeRequest req)

-- Server-side: issue a badge credential, recording which issuer key signed it

issueBadge :: Int -> BBSSecretKey -> VerifiedBadgeRequest -> IO (Either String (Badge 'BCCredential))
issueBadge keyIdx sk (VerifiedBadgeRequest BadgeRequest {masterKey, badgeInfo})
  | badgeExtra badgeInfo /= "" = pure $ Left "badgeExtra must be empty (reserved)"
  | otherwise = fmap (\sig -> BadgeCredential keyIdx masterKey sig badgeInfo) <$> bbsSign sk bbsBadgeHeader (badgeMessages masterKey badgeInfo)

-- Client-side: verify the credential received from server

verifyCredential :: BBSPublicKey -> Badge 'BCCredential -> IO Bool
verifyCredential pk (BadgeCredential _ masterKey signature badgeInfo) =
  bbsVerify pk signature bbsBadgeHeader (badgeMessages masterKey badgeInfo)

-- Client-side: generate a proof for a contact/group; the proof carries the credential's key index

generateBadgeProof :: BBSPublicKey -> Badge 'BCCredential -> BBSPresHeader -> IO (Either String (Badge 'BCProof))
generateBadgeProof pk (BadgeCredential keyIdx masterKey signature badgeInfo) ph =
  fmap (\p -> BadgeProof keyIdx ph p badgeInfo) <$> bbsProofGen pk signature bbsBadgeHeader ph bbsBadgeDisclosedIndexes (badgeMessages masterKey badgeInfo)

-- application-level proof generation with a semantic presentation header
badgeProof :: BBSPublicKey -> Badge 'BCCredential -> BadgePresHeader -> IO (Either String (Badge 'BCProof))
badgeProof pk cred ph = generateBadgeProof pk cred (BBSPresHeader $ badgePresHeaderBytes ph)

-- Recipient-side: verify a badge proof with the configured key its index points to.
-- Nothing means the key index is not in the configured keys (this app version can't verify it).

verifyBadge :: Map Int BBSPublicKey -> Badge 'BCProof -> IO (Maybe Bool)
verifyBadge keys b@(BadgeProof keyIdx _ _ _) = case M.lookup keyIdx keys of
  Nothing -> pure Nothing
  Just pk -> Just <$> verifyBadgeWith pk b

verifyBadgeWith :: BBSPublicKey -> Badge 'BCProof -> IO Bool
verifyBadgeWith pk (BadgeProof _ ph@(BBSPresHeader phBytes) proof badgeInfo)
  | badgePresHeaderAccepted (toBadgePresHeader phBytes) =
      bbsProofVerify pk proof bbsBadgeHeader ph bbsBadgeDisclosedIndexes bbsBadgeMessageCount (badgeInfoMessages badgeInfo)
  | otherwise = pure False

verifyBadge_ :: Map Int BBSPublicKey -> Maybe (Badge 'BCProof) -> IO (Maybe Bool)
verifyBadge_ keys = maybe (pure (Just False)) (verifyBadge keys)

-- DB

instance FromField BadgeType where fromField = fromTextField_ textDecode

instance ToField BadgeType where toField = toField . textEncode

-- (proof, pres_header, expiry, type, verified, extra, master_key, signature, key_idx) - binary columns wrapped in Binary (BLOB/bytea)
type BadgeRow = (Maybe (Binary ByteString), Maybe (Binary ByteString), Maybe UTCTime, Maybe Text, Maybe BoolInt, Maybe Text, Maybe (Binary ByteString), Maybe (Binary ByteString), Maybe Int)

-- receive/store sites have a wire proof + a computed verification outcome;
-- the status here only drives the stored verified flag, the display status is recomputed on load
badgeToRow :: Maybe (Badge 'BCProof) -> Maybe Bool -> BadgeRow
badgeToRow badge verified = localBadgeToRow $ (`PeerBadge` st) <$> badge
  where
    st = case verified of
      Just True -> BSActive
      Just False -> BSFailed
      Nothing -> BSUnknownKey

localBadgeToRow :: Maybe LocalBadge -> BadgeRow
localBadgeToRow (Just lb) = case lb of
  OwnBadge (BadgeCredential idx (BadgeMasterKey mk) (BBSSignature sg) BadgeInfo {badgeType, badgeExpiry, badgeExtra}) st ->
    (Nothing, Nothing, badgeExpiry, Just (textEncode badgeType), verifiedField st, Just badgeExtra, Just (Binary mk), Just (Binary sg), Just idx)
  PeerBadge (BadgeProof idx (BBSPresHeader ph) (BBSProof p) BadgeInfo {badgeType, badgeExpiry, badgeExtra}) st ->
    (Just (Binary p), Just (Binary ph), badgeExpiry, Just (textEncode badgeType), verifiedField st, Just badgeExtra, Nothing, Nothing, Just idx)
  ShownBadge BadgeInfo {badgeType, badgeExpiry, badgeExtra} st ->
    (Nothing, Nothing, badgeExpiry, Just (textEncode badgeType), verifiedField st, Just badgeExtra, Nothing, Nothing, Nothing)
  where
    verifiedField st = case st of
      BSFailed -> Just (BI False)
      BSUnknownKey -> Nothing
      _ -> Just (BI True)
localBadgeToRow Nothing = (Nothing, Nothing, Nothing, Nothing, Just (BI False), Nothing, Nothing, Nothing, Nothing)

rowToBadge :: UTCTime -> BadgeRow -> Maybe LocalBadge
rowToBadge now (p_, ph_, badgeExpiry, type_, verified_, extra_, mk_, sg_, idx_) = do
  btText <- type_
  bt <- textDecode btText
  let info = BadgeInfo {badgeType = bt, badgeExpiry, badgeExtra = maybe "" id extra_}
      -- NULL badge_verified means the key index was unknown when stored (Nothing)
      st = mkBadgeStatus now (unBI <$> verified_) info
  case (mk_, sg_, p_, ph_, idx_) of
    (Just (Binary mk), Just (Binary sg), _, _, Just idx) -> Just $ OwnBadge (BadgeCredential idx (BadgeMasterKey mk) (BBSSignature sg) info) st
    (_, _, Just (Binary p), Just (Binary ph), Just idx) -> Just $ PeerBadge (BadgeProof idx (BBSPresHeader ph) (BBSProof p) info) st
    _ -> Just $ ShownBadge info st

-- JSON

$(JQ.deriveJSON (enumJSON $ dropPrefix "BS") ''BadgeStatus)

$(JQ.deriveJSON defaultJSON ''BadgeInfo)

$(JQ.deriveJSON defaultJSON ''BadgeRequest)

-- The Badge GADT (multi-constructor, different result types) is JSON-encoded via a plain mirror,
-- the codebase pattern for GADTs (see Messages/CIContent CIDeleted/CIStatus). deriveJSON does the work.

data JBadge
  = JBadgeCredential {badgeKeyIdx :: Int, masterKey :: BadgeMasterKey, signature :: BBSSignature, badgeInfo :: BadgeInfo}
  | JBadgeProof {badgeKeyIdx :: Int, presHeader :: BBSPresHeader, proof :: BBSProof, badgeInfo :: BadgeInfo}

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "JBadge") ''JBadge)

jBadge :: Badge b -> JBadge
jBadge = \case
  BadgeCredential idx mk sg i -> JBadgeCredential idx mk sg i
  BadgeProof idx ph p i -> JBadgeProof idx ph p i

instance ToJSON (Badge b) where
  toJSON = toJSON . jBadge
  toEncoding = toEncoding . jBadge

instance FromJSON (Badge 'BCProof) where
  parseJSON v =
    parseJSON v >>= \case
      JBadgeProof idx ph p i -> pure (BadgeProof idx ph p i)
      _ -> fail "expected badge proof"

instance FromJSON (Badge 'BCCredential) where
  parseJSON v =
    parseJSON v >>= \case
      JBadgeCredential idx mk sg i -> pure (BadgeCredential idx mk sg i)
      _ -> fail "expected badge credential"

-- LocalBadge is sent to the UI/clients WITHOUT crypto - only disclosed info + status. The credential/proof
-- bytes stay core-side. FromJSON reconstructs a display-only badge (empty proof) for read-only consumers
-- (remote host, UI echoes); the authoritative badge is loaded from the DB (rowToBadge), never from this JSON.
data JSONBadge = JSONBadge {badge :: BadgeInfo, status :: BadgeStatus}

$(JQ.deriveJSON defaultJSON ''JSONBadge)

instance ToJSON LocalBadge where
  toJSON lb = toJSON $ JSONBadge (localBadgeInfo lb) (localBadgeStatus lb)
  toEncoding lb = toEncoding $ JSONBadge (localBadgeInfo lb) (localBadgeStatus lb)

instance FromJSON LocalBadge where
  parseJSON v = do
    JSONBadge info st <- parseJSON v
    pure $ ShownBadge info st

newtype BBSPublicKeyStr = BBSPublicKeyStr {toBBSPublicKey :: BBSPublicKey}

instance IsString BBSPublicKeyStr where
  fromString = BBSPublicKeyStr . BBSPublicKey . fromRight (error "bad base64 in BBSPublicKey") . U.decode . BC.pack
