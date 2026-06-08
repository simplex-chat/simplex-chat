{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Badges
  ( BadgeType (..),
    BadgeStatus (..),
    LocalBadge (..),
    BadgePurchase (..),
    BadgeMasterKey (..),
    BadgeRequest (..),
    VerifiedBadgeRequest (..),
    BadgeCredential (..),
    SupporterBadge (..),
    bbsBadgeHeader,
    generateMasterKey,
    verifyPayment,
    issueBadge,
    verifyBadgeSignature,
    generateBadgeProof,
    verifyBadge,
    verifyBadge_,
    mkBadgeStatus,
    localBadgeVerified,
    srvBadgePublicKey,
    BadgeRow,
    badgeToRow,
    rowToBadge,
  ) where

import Control.Concurrent.STM
import Crypto.Random (ChaChaDRG)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.TH as JQ
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Simplex.Messaging.Agent.Store.DB (BoolInt (..), fromTextField_)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON)
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

-- Badge status and local badge

data BadgeStatus = BSActive | BSExpired | BSFailed
  deriving (Eq, Show)

data LocalBadge = LocalBadge
  { badgeStatus :: BadgeStatus,
    badge :: SupporterBadge
  }
  deriving (Eq, Show)

mkBadgeStatus :: UTCTime -> Bool -> SupporterBadge -> BadgeStatus
mkBadgeStatus now verified SupporterBadge {badgeExpiry}
  | not verified = BSFailed
  | maybe False (now >) badgeExpiry = BSExpired
  | otherwise = BSActive

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
    badgeType :: BadgeType,
    expiry :: Maybe UTCTime
  }
  deriving (Show)

newtype VerifiedBadgeRequest = VerifiedBadgeRequest BadgeRequest
  deriving (Show)

data BadgeCredential = BadgeCredential
  { masterKey :: BadgeMasterKey,
    signature :: BBSSignature,
    badgeExpiry :: Maybe UTCTime,
    badgeType :: BadgeType,
    badgeExtra :: Text
  }
  deriving (Eq, Show)

data SupporterBadge = SupporterBadge
  { proof :: BBSProof,
    presHeader :: BBSPresHeader,
    badgeExpiry :: Maybe UTCTime,
    badgeType :: BadgeType,
    badgeExtra :: Text
  }
  deriving (Eq, Show)

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

badgeMessages :: BadgeMasterKey -> Maybe UTCTime -> BadgeType -> Text -> [ByteString]
badgeMessages (BadgeMasterKey ms) expiry bt extra = [ms, encodeExpiry expiry, encodeUtf8 (textEncode bt), encodeUtf8 extra]

badgeDisclosedMessages :: Maybe UTCTime -> BadgeType -> Text -> [ByteString]
badgeDisclosedMessages expiry bt extra = [encodeExpiry expiry, encodeUtf8 (textEncode bt), encodeUtf8 extra]

-- Payment verification (stub - always passes)

verifyPayment :: BadgePurchase -> BadgeRequest -> IO (Maybe VerifiedBadgeRequest)
verifyPayment _payment req = pure $ Just (VerifiedBadgeRequest req)

-- Server-side: issue a badge credential

issueBadge :: BBSSecretKey -> BBSPublicKey -> VerifiedBadgeRequest -> IO (Either String BadgeCredential)
issueBadge sk pk (VerifiedBadgeRequest BadgeRequest {masterKey, badgeType, expiry}) =
  fmap mkCred <$> bbsSign sk pk bbsBadgeHeader (badgeMessages masterKey expiry badgeType badgeExtra)
  where
    badgeExtra = "" -- reserved, empty for now
    mkCred sig = BadgeCredential {masterKey, signature = sig, badgeExpiry = expiry, badgeType, badgeExtra}

-- Client-side: verify the credential received from server

verifyBadgeSignature :: BBSPublicKey -> BadgeCredential -> IO Bool
verifyBadgeSignature pk BadgeCredential {masterKey, signature, badgeExpiry, badgeType, badgeExtra} =
  bbsVerify pk signature bbsBadgeHeader (badgeMessages masterKey badgeExpiry badgeType badgeExtra)

-- Client-side: generate a proof for a contact/group

generateBadgeProof :: BBSPublicKey -> BadgeCredential -> BBSPresHeader -> IO (Either String SupporterBadge)
generateBadgeProof pk BadgeCredential {masterKey, signature, badgeExpiry, badgeType, badgeExtra} ph =
  fmap mkBadge <$> bbsProofGen pk signature bbsBadgeHeader ph bbsBadgeDisclosedIndexes (badgeMessages masterKey badgeExpiry badgeType badgeExtra)
  where
    mkBadge p = SupporterBadge {proof = p, presHeader = ph, badgeExpiry, badgeType, badgeExtra}

-- Recipient-side: verify a badge proof

verifyBadge :: BBSPublicKey -> SupporterBadge -> IO Bool
verifyBadge pk SupporterBadge {proof, presHeader, badgeExpiry, badgeType, badgeExtra} =
  bbsProofVerify pk proof bbsBadgeHeader presHeader bbsBadgeDisclosedIndexes bbsBadgeMessageCount (badgeDisclosedMessages badgeExpiry badgeType badgeExtra)

verifyBadge_ :: BBSPublicKey -> Maybe SupporterBadge -> IO Bool
verifyBadge_ = maybe (pure False) . verifyBadge

localBadgeVerified :: Maybe LocalBadge -> Maybe Bool
localBadgeVerified = fmap $ \LocalBadge {badgeStatus} -> badgeStatus /= BSFailed

-- Server public key (test key - replace with real key when badge service is deployed)

srvBadgePublicKey :: BBSPublicKey
srvBadgePublicKey = BBSPublicKey "" -- TODO generate real keypair

-- DB

instance FromField BadgeType where fromField = fromTextField_ textDecode

instance ToField BadgeType where toField = toField . textEncode

type BadgeRow = (Maybe ByteString, Maybe ByteString, Maybe UTCTime, Maybe Text, Maybe BoolInt, Maybe Text)

badgeToRow :: Maybe SupporterBadge -> Bool -> BadgeRow
badgeToRow (Just SupporterBadge {proof = BBSProof p, presHeader = BBSPresHeader ph, badgeExpiry, badgeType, badgeExtra}) verified =
  (Just p, Just ph, badgeExpiry, Just (textEncode badgeType), Just (BI verified), Just badgeExtra)
badgeToRow _ _ = (Nothing, Nothing, Nothing, Nothing, Just (BI False), Nothing)

rowToBadge :: UTCTime -> BadgeRow -> Maybe LocalBadge
rowToBadge now (Just p, Just ph, badgeExpiry, Just btText, verified_, extra_) = do
  bt <- textDecode btText
  let b = SupporterBadge {proof = BBSProof p, presHeader = BBSPresHeader ph, badgeExpiry, badgeType = bt, badgeExtra = maybe "" id extra_}
      verified = maybe False unBI verified_
  Just LocalBadge {badgeStatus = mkBadgeStatus now verified b, badge = b}
rowToBadge _ _ = Nothing

-- JSON

$(JQ.deriveJSON (enumJSON $ dropPrefix "BS") ''BadgeStatus)

$(JQ.deriveJSON defaultJSON ''SupporterBadge)

$(JQ.deriveJSON defaultJSON ''LocalBadge)

$(JQ.deriveJSON defaultJSON ''BadgeRequest)

$(JQ.deriveJSON defaultJSON ''BadgeCredential)
