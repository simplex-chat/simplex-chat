{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module StoreService.Names.View
  ( ViewReads (..),
    ViewRead (..),
    RegistrationView (..),
    registrationView,
    readViewChain,
    toRegistrationView,
    balanceJSON,
    addressText,
    registrationAt,
    lapseMoment,
    registrationVersion,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Logger.Simple (logWarn)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Int (Int64)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String (strEncode, textEncode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Names.Chain (ChainError, ChainSeconds, TxHash (..), chainTime, hex)
import StoreService.Names.Community (AcceptanceAsked (..), communityJSON, currentAcceptance)
import StoreService.Names.Contracts (Contracts (ctDeadline, ctViewSlots), sponsorNonce)
import StoreService.Names.Profiles (deadlineIn, withSlot)
import StoreService.Names.Registration (AgreedTerms (..), RegistrationId (..), termsKindText, tldLabel)
import StoreService.Names.Store (Balance (..), BalanceHold (..), BalanceUse (..), FailReason (..), HoldState (..), LateWait, NameRegistration (..), NameReveal (..), OrderPhase (..), getBalance, getRegistration, heldBy, lateWait, namePhase, paymentBalance, paymentInvoice)

-- | The view's chain reads; a field whose read failed is null and named in unread.
data ViewReads = ViewReads
  { vrIntentNonce :: ViewRead Word64,
    vrAcceptance :: ViewRead AcceptanceAsked
  }
  deriving (Eq, Show)

data ViewRead a = VRNotAsked | VRUnread | VRRead a
  deriving (Eq, Show)

data RegistrationView = RegistrationView
  { vwId :: Text,
    vwVersion :: Int64,
    vwMoves :: Int,
    vwLinksRefused :: Bool,
    vwPhase :: Text,
    vwLabelHash :: Text,
    vwLabel :: Maybe Text,
    vwOwner :: Address,
    vwCommitment :: Text,
    vwSealed :: Text,
    vwResolver :: Address,
    vwSponsor :: Address,
    vwChainId :: Word64,
    vwTld :: Text,
    vwYears :: Int,
    vwDuration :: Word64,
    vwPrice :: USDCents,
    vwFromBalance :: USDCents,
    vwBalance :: Maybe J.Value,
    vwLatePayment :: Maybe J.Value,
    vwCommitTx :: Maybe Text,
    vwRegisterTx :: Maybe Text,
    vwRevealAfter :: Maybe ChainSeconds,
    vwRevealBefore :: Maybe ChainSeconds,
    vwIntentNonce :: Maybe Word64,
    vwAcceptance :: Maybe J.Value,
    vwUnread :: [Text],
    vwExpiresAt :: Maybe ChainSeconds,
    vwError :: Maybe Text,
    vwCommunity :: J.Value,
    vwTerms :: Text
  }
  deriving (Eq, Show)

$(JQ.deriveToJSON defaultJSON {J.omitNothingFields = False, J.fieldLabelModifier = dropPrefix "vw"} ''RegistrationView)

-- | The balance as it reads at the given time, when a hold may have become overdue, and what a closed order's late money waits on.
registrationView :: DBStore -> NamesConfig -> Contracts -> UTCTime -> NameRegistration -> IO RegistrationView
registrationView st cfg ct now r = do
  (balance, late) <- withTransaction st $ \db -> do
    balance <- getBalance db (nrId r) now
    late <- maybe (pure Nothing) (lateWait db) (paymentInvoice $ nrPayment r)
    pure (balance, late)
  toRegistrationView cfg r balance late <$> readViewChain ct r

-- | The intent nonce only in committed; the acceptance too, until a reveal is stored.
readViewChain :: Contracts -> NameRegistration -> IO ViewReads
readViewChain ct NameRegistration {nrId = RegistrationId rid, nrPhase, nrOwner, nrLabelHash, nrReveal} = case nrPhase of
  OCommitted -> do
    deadline <- deadlineIn $ ctDeadline ct
    withSlot deadline (ctViewSlots ct) (concurrently (sponsorNonce ct nrOwner) acceptanceRead) >>= \case
      Just (nonce, acceptance_) -> ViewReads <$> readOf "intent nonce" nonce <*> maybe (pure VRNotAsked) (readOf "terms acceptance") acceptance_
      Nothing -> unread <$ logWarn ("names: the view of registration " <> rid <> " read the chain in no slot within its deadline")
  _ -> pure none
  where
    none = ViewReads {vrIntentNonce = VRNotAsked, vrAcceptance = VRNotAsked}
    acceptanceRead = if isJust nrReveal then pure Nothing else Just <$> currentAcceptance ct nrLabelHash nrOwner
    readOf :: Text -> Either ChainError a -> IO (ViewRead a)
    readOf what = either (\e -> VRUnread <$ logWarn ("names: the view of registration " <> rid <> " could not read the " <> what <> ": " <> tshow e)) (pure . VRRead)
    unread = ViewReads {vrIntentNonce = VRUnread, vrAcceptance = if isNothing nrReveal then VRUnread else VRNotAsked}

toRegistrationView :: NamesConfig -> NameRegistration -> Maybe Balance -> Maybe LateWait -> ViewReads -> RegistrationView
toRegistrationView cfg@NamesConfig {nTLD, nChain = ChainConfig {cChainId, cResolver, cSponsor}} r balance late ViewReads {vrIntentNonce, vrAcceptance} =
  RegistrationView
    { vwId = rid,
      vwVersion = nrVersion,
      vwMoves = nrMoves,
      vwLinksRefused = isJust nrLinksRefusedAt,
      vwPhase = textEncode (namePhase nrPhase),
      vwLabelHash = hex lh,
      vwLabel = rvLabel <$> nrReveal,
      vwOwner = nrOwner,
      vwCommitment = hex nrCommitment,
      vwSealed = nrSealed,
      vwResolver = cResolver,
      vwSponsor = cSponsor,
      vwChainId = cChainId,
      vwTld = tldLabel nTLD,
      vwYears = nrYears,
      vwDuration = nrDuration,
      vwPrice = nrPrice,
      vwFromBalance = maybe 0 buUsed (paymentBalance nrPayment),
      vwBalance = balanceJSON <$> balance,
      vwLatePayment = (\w -> J.object ["waiting" .= w]) <$> late,
      vwCommitTx = txText <$> nrCommitTx,
      vwRegisterTx = txText <$> nrRegisterTx,
      vwRevealAfter = nrRevealAfter,
      vwRevealBefore = nrRevealBefore,
      vwIntentNonce = readValue vrIntentNonce,
      vwAcceptance = acceptanceJSON <$> readValue vrAcceptance,
      vwUnread = ["intentNonce" | isUnread vrIntentNonce] <> ["acceptance" | isUnread vrAcceptance],
      vwExpiresAt = expiresAt,
      vwError = textEncode <$> failReason,
      vwCommunity = communityJSON cfg (nrLabelHash r) (atCommunityName nrAgreedTerms),
      vwTerms = textEncode nrAgreedTerms
    }
  where
    NameRegistration {nrId = RegistrationId rid, nrVersion, nrMoves, nrLinksRefusedAt, nrPhase, nrLabelHash = LabelHash lh, nrReveal, nrOwner, nrCommitment, nrSealed} = r
    NameRegistration {nrYears, nrDuration, nrPrice, nrPayment, nrCommitTx, nrRegisterTx, nrRevealAfter, nrRevealBefore, nrAgreedTerms} = r
    (expiresAt, failReason) = case nrPhase of
      ORegistered expiry _ -> (Just expiry, Nothing)
      OFailed _ reason -> (Nothing, Just reason)
      _ -> (Nothing, Nothing)
    txText (TxHash h) = hex h
    readValue = \case
      VRRead a -> Just a
      _ -> Nothing
    isUnread = \case
      VRUnread -> True
      _ -> False
    acceptanceJSON AcceptanceAsked {acCommunity, acPolicy, acTermsHash, acNonce} =
      J.object ["kind" .= termsKindText acCommunity, "policy" .= acPolicy, "termsHash" .= hex acTermsHash, "nonce" .= acNonce]

-- | The holder is not named, so an order's link discloses no other order. A funded hold has no end time.
balanceJSON :: Balance -> J.Value
balanceJSON b@Balance {blAmount, blRefunded} =
  J.object ["amount" .= blAmount, "held" .= isJust hold, "heldUntil" .= (heldUntil . bhState =<< hold), "refunded" .= blRefunded]
  where
    hold = heldBy b
    heldUntil = \case
      HSOpen t -> Just t
      HSFunded -> Nothing
      HSOverdue _ -> Nothing

-- | EIP-55, as the page compares owners.
addressText :: Address -> Text
addressText = safeDecodeUtf8 . strEncode

-- | Versions start at 1, so a registration deleted during a hold reads as changed.
registrationVersion :: DBStore -> RegistrationId -> IO Int64
registrationVersion st rid = maybe 0 nrVersion <$> withTransaction st (`getRegistration` rid)

-- | Past revealBefore with no reveal, the commitment can no longer register, so it reads failed.
registrationAt :: UTCTime -> NameRegistration -> NameRegistration
registrationAt now r = case lapseMoment r of
  Just lapse | now > lapse -> r {nrPhase = OFailed lapse FRNotSignedInTime}
  _ -> r

lapseMoment :: NameRegistration -> Maybe UTCTime
lapseMoment = \case
  NameRegistration {nrPhase = OCommitted, nrReveal = Nothing, nrRevealBefore = Just before} -> Just $ chainTime before
  _ -> Nothing
