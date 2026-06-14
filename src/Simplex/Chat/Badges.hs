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
    BadgeCredential (..),
    BadgeProof (..),
    LocalBadge (..),
    JSONBadge (..),
    BBSPublicKeyStr (..),
    localBadgeInfo,
    localBadgeStatus,
    maxXFTPFileSize,
    maxFileSizeSupporter,
    maxFileSizeLegend,
    BadgePresHeaderTag (..),
    BadgePresHeader (..),
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
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Base64.URL as U
import qualified Data.ByteString.Char8 as B
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, nominalDay)
import Simplex.FileTransfer.Description (gb, maxFileSize)
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

-- A badge credential (own, secret) and a proof (a presentation) are independent records.
-- badgeKeyIdx is the issuer key index: it tells verifiers which configured key to use.
-- Only proofs ride the wire (in a profile); credentials come from the badge service. Neither is
-- ever serialized as a sum - each travels as its own record, so the JSON carries no credential/proof tag.

data BadgeCredential = BadgeCredential
  { badgeKeyIdx :: Int,
    masterKey :: BadgeMasterKey,
    signature :: BBSSignature,
    badgeInfo :: BadgeInfo
  }
  deriving (Eq, Show)

data BadgeProof = BadgeProof
  { badgeKeyIdx :: Int,
    presHeader :: BBSPresHeader,
    proof :: BBSProof,
    badgeInfo :: BadgeInfo
  }
  deriving (Eq, Show)

-- Local badge: a stored badge plus its display status (the in-memory sum; never serialized as a sum).
--   OwnBadge   - the user's own credential (loaded from the DB).
--   PeerBadge  - a verified peer proof (from the DB, or received over the wire).
--   ShownBadge - decoded from a crypto-free profile JSON for display only: no crypto, so it cannot be sent.
data LocalBadge
  = OwnBadge BadgeCredential BadgeStatus
  | PeerBadge BadgeProof BadgeStatus
  | ShownBadge BadgeInfo BadgeStatus
  deriving (Eq, Show)

localBadgeInfo :: LocalBadge -> BadgeInfo
localBadgeInfo = \case
  OwnBadge BadgeCredential {badgeInfo} _ -> badgeInfo
  PeerBadge BadgeProof {badgeInfo} _ -> badgeInfo
  ShownBadge i _ -> i

localBadgeStatus :: LocalBadge -> BadgeStatus
localBadgeStatus = \case
  OwnBadge _ st -> st
  PeerBadge _ st -> st
  ShownBadge _ st -> st

-- XFTP file size limit raised by an active badge: a legend badge to 5GB, any other to 2GB, otherwise the default.
maxFileSizeSupporter :: Int64
maxFileSizeSupporter = gb 2

maxFileSizeLegend :: Int64
maxFileSizeLegend = gb 5

maxXFTPFileSize :: Maybe LocalBadge -> Int64
maxXFTPFileSize = \case
  Just b | localBadgeStatus b == BSActive -> case badgeType (localBadgeInfo b) of
    BTLegend -> maxFileSizeLegend
    _ -> maxFileSizeSupporter
  _ -> maxFileSize

-- Presentation header: a tag char + payload. PHTest is unbound - a fresh random nonce per
-- presentation, not bound to any context; the 'T' tag marks it so master rejects it.
-- PHUnknown is the forward-compat catch-all for tags this version does not interpret.

data BadgePresHeaderTag = PHTestTag | PHUnknownTag Char

instance StrEncoding BadgePresHeaderTag where
  strEncode = B.singleton . \case
    PHTestTag -> 'T'
    PHUnknownTag c -> c
  strP = tag <$> A.anyChar
    where
      tag = \case
        'T' -> PHTestTag
        c -> PHUnknownTag c

data BadgePresHeader
  = PHTest ByteString
  | PHUnknown Char ByteString

instance StrEncoding BadgePresHeader where
  strEncode = \case
    PHTest nonce -> strEncode PHTestTag <> nonce
    PHUnknown c b -> strEncode (PHUnknownTag c) <> b
  strP =
    strP >>= \case
      PHTestTag -> PHTest <$> A.takeByteString
      PHUnknownTag c -> PHUnknown c <$> A.takeByteString

-- stable accepts both; master rejects PHTest
badgePresHeaderAccepted :: BadgePresHeader -> Bool
badgePresHeaderAccepted = \case
  PHTest _ -> True
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

issueBadge :: Int -> BBSSecretKey -> VerifiedBadgeRequest -> IO (Either String BadgeCredential)
issueBadge keyIdx sk (VerifiedBadgeRequest BadgeRequest {masterKey, badgeInfo})
  | badgeExtra badgeInfo /= "" = pure $ Left "badgeExtra must be empty (reserved)"
  | otherwise = fmap (\sig -> BadgeCredential keyIdx masterKey sig badgeInfo) <$> bbsSign sk bbsBadgeHeader (badgeMessages masterKey badgeInfo)

-- Client-side: verify the credential received from server

verifyCredential :: BBSPublicKey -> BadgeCredential -> IO Bool
verifyCredential pk (BadgeCredential _ masterKey signature badgeInfo) =
  bbsVerify pk signature bbsBadgeHeader (badgeMessages masterKey badgeInfo)

-- Client-side: generate a proof for a contact/group; the proof carries the credential's key index

generateBadgeProof :: BBSPublicKey -> BadgeCredential -> BBSPresHeader -> IO (Either String BadgeProof)
generateBadgeProof pk (BadgeCredential keyIdx masterKey signature badgeInfo) ph =
  fmap (\p -> BadgeProof keyIdx ph p badgeInfo) <$> bbsProofGen pk signature bbsBadgeHeader ph bbsBadgeDisclosedIndexes (badgeMessages masterKey badgeInfo)

-- application-level proof generation with a semantic presentation header
badgeProof :: BBSPublicKey -> BadgeCredential -> BadgePresHeader -> IO (Either String BadgeProof)
badgeProof pk cred ph = generateBadgeProof pk cred (BBSPresHeader $ strEncode ph)

-- Recipient-side: verify a badge proof with the configured key its index points to.
-- Nothing means the key index is not in the configured keys (this app version can't verify it).

verifyBadge :: Map Int BBSPublicKey -> BadgeProof -> IO (Maybe Bool)
verifyBadge keys b@(BadgeProof keyIdx _ _ _) = case M.lookup keyIdx keys of
  Nothing -> pure Nothing
  Just pk -> Just <$> verifyBadgeWith pk b

verifyBadgeWith :: BBSPublicKey -> BadgeProof -> IO Bool
verifyBadgeWith pk (BadgeProof _ ph@(BBSPresHeader phBytes) proof badgeInfo)
  | either (const False) badgePresHeaderAccepted (strDecode phBytes) =
      bbsProofVerify pk proof bbsBadgeHeader ph bbsBadgeDisclosedIndexes bbsBadgeMessageCount (badgeInfoMessages badgeInfo)
  | otherwise = pure False

verifyBadge_ :: Map Int BBSPublicKey -> Maybe BadgeProof -> IO (Maybe Bool)
verifyBadge_ keys = maybe (pure (Just False)) (verifyBadge keys)

-- DB

instance FromField BadgeType where fromField = fromTextField_ textDecode

instance ToField BadgeType where toField = toField . textEncode

-- (proof, pres_header, expiry, type, verified, extra, master_key, signature, key_idx) - binary columns wrapped in Binary (BLOB/bytea)
type BadgeRow = (Maybe (Binary ByteString), Maybe (Binary ByteString), Maybe UTCTime, Maybe Text, Maybe BoolInt, Maybe Text, Maybe (Binary ByteString), Maybe (Binary ByteString), Maybe Int)

-- receive/store sites have a wire proof + a computed verification outcome;
-- the status here only drives the stored verified flag, the display status is recomputed on load
badgeToRow :: Maybe BadgeProof -> Maybe Bool -> BadgeRow
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

-- Each record is a plain JSON object (defaultJSON), platform-independent and with no credential/proof
-- tag - the context (a proof in a profile, a credential from the service) determines which it is.

$(JQ.deriveJSON defaultJSON ''BadgeCredential)

$(JQ.deriveJSON defaultJSON ''BadgeProof)

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
  fromString = BBSPublicKeyStr . BBSPublicKey . fromRight (error "bad base64 in BBSPublicKey") . U.decode . B.pack
