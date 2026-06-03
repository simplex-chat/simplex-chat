{-# LANGUAGE DuplicateRecordFields #-}
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
    mkBadgeStatus,
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.TH as JQ
import Control.Concurrent.STM
import Crypto.Random (ChaChaDRG)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON)

-- Badge type

data BadgeType
  = BTSupporter
  | BTBusiness
  | BTLegend
  | BTCFInvestor
  | BTUnknown Text
  deriving (Eq, Show)

instance TextEncoding BadgeType where
  textEncode = \case
    BTSupporter -> "supporter"
    BTBusiness -> "business"
    BTLegend -> "legend"
    BTCFInvestor -> "cf_investor"
    BTUnknown tag -> tag
  textDecode s = Just $ case s of
    "supporter" -> BTSupporter
    "business" -> BTBusiness
    "legend" -> BTLegend
    "cf_investor" -> BTCFInvestor
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
  deriving (Eq, Show)

generateMasterKey :: TVar ChaChaDRG -> IO BadgeMasterKey
generateMasterKey drg = BadgeMasterKey <$> atomically (C.randomBytes 32 drg)

-- Workflow types

data BadgeRequest = BadgeRequest
  { masterKey :: BadgeMasterKey,
    badgeType :: BadgeType,
    payment :: BadgePurchase
  }
  deriving (Show)

data VerifiedBadgeRequest = VerifiedBadgeRequest
  { masterKey :: BadgeMasterKey,
    badgeType :: BadgeType
  }
  deriving (Show)

data BadgeCredential = BadgeCredential
  { masterKey :: BadgeMasterKey,
    signature :: BBSSignature,
    badgeExpiry :: Maybe UTCTime,
    badgeType :: BadgeType
  }
  deriving (Eq, Show)

data SupporterBadge = SupporterBadge
  { proof :: BBSProof,
    presHeader :: BBSPresHeader,
    badgeExpiry :: Maybe UTCTime,
    badgeType :: BadgeType
  }
  deriving (Eq, Show)

-- Constants

bbsBadgeHeader :: BBSHeader
bbsBadgeHeader = BBSHeader "SimpleX badges v1"

bbsBadgeMessageCount :: Int
bbsBadgeMessageCount = 3

bbsBadgeDisclosedIndexes :: [Int]
bbsBadgeDisclosedIndexes = [1, 2]

-- Message encoding

encodeExpiry :: Maybe UTCTime -> ByteString
encodeExpiry = maybe "lifetime" strEncode

badgeMessages :: BadgeMasterKey -> Maybe UTCTime -> BadgeType -> [ByteString]
badgeMessages (BadgeMasterKey ms) expiry bt = [ms, encodeExpiry expiry, encodeUtf8 (textEncode bt)]

badgeDisclosedMessages :: Maybe UTCTime -> BadgeType -> [ByteString]
badgeDisclosedMessages expiry bt = [encodeExpiry expiry, encodeUtf8 (textEncode bt)]

-- Payment verification (stub - always passes)

verifyPayment :: BadgeRequest -> IO (Maybe VerifiedBadgeRequest)
verifyPayment BadgeRequest {masterKey, badgeType} =
  pure $ Just VerifiedBadgeRequest {masterKey, badgeType}

-- Server-side: issue a badge credential

issueBadge :: BBSSecretKey -> BBSPublicKey -> Maybe UTCTime -> VerifiedBadgeRequest -> IO (Either String BadgeCredential)
issueBadge sk pk expiry VerifiedBadgeRequest {masterKey, badgeType} =
  fmap mkCred <$> bbsSign sk pk bbsBadgeHeader (badgeMessages masterKey expiry badgeType)
  where
    mkCred sig = BadgeCredential {masterKey, signature = sig, badgeExpiry = expiry, badgeType}

-- Client-side: verify the credential received from server

verifyBadgeSignature :: BBSPublicKey -> BadgeCredential -> IO Bool
verifyBadgeSignature pk BadgeCredential {masterKey, signature, badgeExpiry, badgeType} =
  bbsVerify pk signature bbsBadgeHeader (badgeMessages masterKey badgeExpiry badgeType)

-- Client-side: generate a proof for a contact/group

generateBadgeProof :: BBSPublicKey -> BadgeCredential -> BBSPresHeader -> IO (Either String SupporterBadge)
generateBadgeProof pk BadgeCredential {masterKey, signature, badgeExpiry, badgeType} ph =
  fmap mkBadge <$> bbsProofGen pk signature bbsBadgeHeader ph bbsBadgeDisclosedIndexes (badgeMessages masterKey badgeExpiry badgeType)
  where
    mkBadge p = SupporterBadge {proof = p, presHeader = ph, badgeExpiry, badgeType}

-- Recipient-side: verify a badge proof

verifyBadge :: BBSPublicKey -> SupporterBadge -> IO Bool
verifyBadge pk SupporterBadge {proof, presHeader, badgeExpiry, badgeType} =
  bbsProofVerify pk proof bbsBadgeHeader presHeader bbsBadgeDisclosedIndexes bbsBadgeMessageCount (badgeDisclosedMessages badgeExpiry badgeType)

-- JSON

$(JQ.deriveJSON (enumJSON $ dropPrefix "BS") ''BadgeStatus)

$(JQ.deriveJSON defaultJSON ''SupporterBadge)

$(JQ.deriveJSON defaultJSON ''LocalBadge)
