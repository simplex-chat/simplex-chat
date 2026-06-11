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
    localBadgeInfo,
    localBadgeStatus,
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
    localBadgeVerified,
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
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
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
  | BTBusiness
  | BTLegend
  | BTInvestor
  | BTUnknown Text
  deriving (Eq, Show)

instance TextEncoding BadgeType where
  textEncode = \case
    BTSupporter -> "supporter"
    BTBusiness -> "business"
    BTLegend -> "legend"
    BTInvestor -> "investor"
    BTUnknown tag -> tag
  textDecode s = Just $ case s of
    "supporter" -> BTSupporter
    "business" -> BTBusiness
    "legend" -> BTLegend
    "investor" -> BTInvestor
    tag -> BTUnknown tag

instance ToJSON BadgeType where
  toJSON = textToJSON
  toEncoding = textToEncoding

instance FromJSON BadgeType where
  parseJSON = textParseJSON "BadgeType"

-- Badge status

data BadgeStatus = BSActive | BSExpired | BSFailed
  deriving (Eq, Show)

-- Disclosed badge content (BBS messages 1, 2, 3)

data BadgeInfo = BadgeInfo
  { badgeType :: BadgeType,
    badgeExpiry :: Maybe UTCTime,
    badgeExtra :: Text
  }
  deriving (Eq, Show)

mkBadgeStatus :: UTCTime -> Bool -> BadgeInfo -> BadgeStatus
mkBadgeStatus now verified BadgeInfo {badgeExpiry}
  | not verified = BSFailed
  | maybe False (now >) badgeExpiry = BSExpired
  | otherwise = BSActive

-- Badge crypto: a credential (own, secret) or a proof (wire, a presentation).
-- Positional GADT - a record field cannot be shared across constructors with different result types.

data BadgeCrypto = BCCredential | BCProof

data Badge (b :: BadgeCrypto) where
  BadgeCredential :: BadgeMasterKey -> BBSSignature -> BadgeInfo -> Badge 'BCCredential
  BadgeProof :: BBSPresHeader -> BBSProof -> BadgeInfo -> Badge 'BCProof

deriving instance Show (Badge b)

deriving instance Eq (Badge 'BCCredential)

deriving instance Eq (Badge 'BCProof)

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
  OwnBadge (BadgeCredential _ _ i) _ -> i
  PeerBadge (BadgeProof _ _ i) _ -> i
  ShownBadge i _ -> i

localBadgeStatus :: LocalBadge -> BadgeStatus
localBadgeStatus = \case
  OwnBadge _ st -> st
  PeerBadge _ st -> st
  ShownBadge _ st -> st

localBadgeVerified :: Maybe LocalBadge -> Maybe Bool
localBadgeVerified = fmap $ \lb -> localBadgeStatus lb /= BSFailed

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

-- Server-side: issue a badge credential

issueBadge :: BBSSecretKey -> BBSPublicKey -> VerifiedBadgeRequest -> IO (Either String (Badge 'BCCredential))
issueBadge sk pk (VerifiedBadgeRequest BadgeRequest {masterKey, badgeInfo})
  | badgeExtra badgeInfo /= "" = pure $ Left "badgeExtra must be empty (reserved)"
  | otherwise = fmap (\sig -> BadgeCredential masterKey sig badgeInfo) <$> bbsSign sk pk bbsBadgeHeader (badgeMessages masterKey badgeInfo)

-- Client-side: verify the credential received from server

verifyCredential :: BBSPublicKey -> Badge 'BCCredential -> IO Bool
verifyCredential pk (BadgeCredential masterKey signature badgeInfo) =
  bbsVerify pk signature bbsBadgeHeader (badgeMessages masterKey badgeInfo)

-- Client-side: generate a proof for a contact/group

generateBadgeProof :: BBSPublicKey -> Badge 'BCCredential -> BBSPresHeader -> IO (Either String (Badge 'BCProof))
generateBadgeProof pk (BadgeCredential masterKey signature badgeInfo) ph =
  fmap (\p -> BadgeProof ph p badgeInfo) <$> bbsProofGen pk signature bbsBadgeHeader ph bbsBadgeDisclosedIndexes (badgeMessages masterKey badgeInfo)

-- application-level proof generation with a semantic presentation header
badgeProof :: BBSPublicKey -> Badge 'BCCredential -> BadgePresHeader -> IO (Either String (Badge 'BCProof))
badgeProof pk cred ph = generateBadgeProof pk cred (BBSPresHeader $ badgePresHeaderBytes ph)

-- Recipient-side: verify a badge proof

verifyBadge :: BBSPublicKey -> Badge 'BCProof -> IO Bool
verifyBadge pk (BadgeProof ph@(BBSPresHeader phBytes) proof badgeInfo)
  | badgePresHeaderAccepted (toBadgePresHeader phBytes) =
      bbsProofVerify pk proof bbsBadgeHeader ph bbsBadgeDisclosedIndexes bbsBadgeMessageCount (badgeInfoMessages badgeInfo)
  | otherwise = pure False

verifyBadge_ :: BBSPublicKey -> Maybe (Badge 'BCProof) -> IO Bool
verifyBadge_ = maybe (pure False) . verifyBadge

-- DB

instance FromField BadgeType where fromField = fromTextField_ textDecode

instance ToField BadgeType where toField = toField . textEncode

-- (proof, pres_header, expiry, type, verified, extra, master_key, signature) - binary columns wrapped in Binary (BLOB/bytea)
type BadgeRow = (Maybe (Binary ByteString), Maybe (Binary ByteString), Maybe UTCTime, Maybe Text, Maybe BoolInt, Maybe Text, Maybe (Binary ByteString), Maybe (Binary ByteString))

-- receive/store sites have a wire proof + a computed verified flag
badgeToRow :: Maybe (Badge 'BCProof) -> Bool -> BadgeRow
badgeToRow badge verified = localBadgeToRow $ (\b -> PeerBadge b (if verified then BSActive else BSFailed)) <$> badge

localBadgeToRow :: Maybe LocalBadge -> BadgeRow
localBadgeToRow (Just lb) = case lb of
  OwnBadge (BadgeCredential (BadgeMasterKey mk) (BBSSignature sg) BadgeInfo {badgeType, badgeExpiry, badgeExtra}) st ->
    (Nothing, Nothing, badgeExpiry, Just (textEncode badgeType), Just (BI (active st)), Just badgeExtra, Just (Binary mk), Just (Binary sg))
  PeerBadge (BadgeProof (BBSPresHeader ph) (BBSProof p) BadgeInfo {badgeType, badgeExpiry, badgeExtra}) st ->
    (Just (Binary p), Just (Binary ph), badgeExpiry, Just (textEncode badgeType), Just (BI (active st)), Just badgeExtra, Nothing, Nothing)
  ShownBadge BadgeInfo {badgeType, badgeExpiry, badgeExtra} st ->
    (Nothing, Nothing, badgeExpiry, Just (textEncode badgeType), Just (BI (active st)), Just badgeExtra, Nothing, Nothing)
  where
    active st = st /= BSFailed
localBadgeToRow Nothing = (Nothing, Nothing, Nothing, Nothing, Just (BI False), Nothing, Nothing, Nothing)

rowToBadge :: UTCTime -> BadgeRow -> Maybe LocalBadge
rowToBadge now (p_, ph_, badgeExpiry, type_, verified_, extra_, mk_, sg_) = do
  btText <- type_
  bt <- textDecode btText
  let info = BadgeInfo {badgeType = bt, badgeExpiry, badgeExtra = maybe "" id extra_}
      verified = maybe False unBI verified_
      st = mkBadgeStatus now verified info
  case (mk_, sg_, p_, ph_) of
    (Just (Binary mk), Just (Binary sg), _, _) -> Just $ OwnBadge (BadgeCredential (BadgeMasterKey mk) (BBSSignature sg) info) st
    (_, _, Just (Binary p), Just (Binary ph)) -> Just $ PeerBadge (BadgeProof (BBSPresHeader ph) (BBSProof p) info) st
    _ -> Just $ ShownBadge info st

-- JSON

$(JQ.deriveJSON (enumJSON $ dropPrefix "BS") ''BadgeStatus)

$(JQ.deriveJSON defaultJSON ''BadgeInfo)

$(JQ.deriveJSON defaultJSON ''BadgeRequest)

-- The Badge GADT (multi-constructor, different result types) is JSON-encoded via a plain mirror,
-- the codebase pattern for GADTs (see Messages/CIContent CIDeleted/CIStatus). deriveJSON does the work.

data JBadge
  = JBadgeCredential {masterKey :: BadgeMasterKey, signature :: BBSSignature, badgeInfo :: BadgeInfo}
  | JBadgeProof {presHeader :: BBSPresHeader, proof :: BBSProof, badgeInfo :: BadgeInfo}

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "JBadge") ''JBadge)

jBadge :: Badge b -> JBadge
jBadge = \case
  BadgeCredential mk sg i -> JBadgeCredential mk sg i
  BadgeProof ph p i -> JBadgeProof ph p i

instance ToJSON (Badge b) where
  toJSON = toJSON . jBadge
  toEncoding = toEncoding . jBadge

instance FromJSON (Badge 'BCProof) where
  parseJSON v =
    parseJSON v >>= \case
      JBadgeProof ph p i -> pure (BadgeProof ph p i)
      _ -> fail "expected badge proof"

instance FromJSON (Badge 'BCCredential) where
  parseJSON v =
    parseJSON v >>= \case
      JBadgeCredential mk sg i -> pure (BadgeCredential mk sg i)
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
