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
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Chain (ChainError, ChainSeconds, Hex32 (..), TxHash (..), chainTime)
import StoreService.Names.Community (AcceptanceAsked (..), Community, communityOf, currentAcceptance)
import StoreService.Names.Contracts (Contracts (ctDeadline, ctViewSlots), sponsorNonce)
import StoreService.Names.Profiles (deadlineIn, withSlot)
import StoreService.Names.Registration (AgreedTerms (..), RegistrationId (..), termsKindText)
import StoreService.Names.Store (Balance (..), BalanceHold (..), BalanceUse (..), Commit (..), CommitStep (..), FailReason (..), HoldState (..), LateWait, NameRegistration (..), NameReveal (..), OrderPhase (..), commitTxOf, failedFrom, getBalance, getRegistration, heldBy, lateWait, namePhase, paymentBalance, paymentInvoice, phaseCommit, phaseRegisterTx)

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
    vwLabelHash :: Hex32,
    vwLabel :: Maybe Text,
    vwOwner :: Address,
    vwCommitment :: Hex32,
    vwSealed :: Text,
    vwYears :: Int,
    vwDuration :: Word64,
    vwPrice :: USDCents,
    vwFromBalance :: USDCents,
    vwBalance :: Maybe J.Value,
    vwLatePayment :: Maybe J.Value,
    vwCommitTx :: Maybe TxHash,
    vwRegisterTx :: Maybe TxHash,
    vwRevealAfter :: Maybe ChainSeconds,
    vwRevealBefore :: Maybe ChainSeconds,
    vwIntentNonce :: Maybe Word64,
    vwAcceptance :: Maybe J.Value,
    vwUnread :: [Text],
    vwExpiresAt :: Maybe ChainSeconds,
    vwError :: Maybe Text,
    vwCommunity :: Maybe Community,
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
  OCommitted _ -> do
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
toRegistrationView cfg r balance late ViewReads {vrIntentNonce, vrAcceptance} =
  RegistrationView
    { vwId = rid,
      vwVersion = nrVersion,
      vwMoves = nrMoves,
      vwLinksRefused = isJust (cmLinksRefusedAt =<< commit),
      vwPhase = textEncode (namePhase nrPhase),
      vwLabelHash = Hex32 lh,
      vwLabel = rvLabel <$> nrReveal,
      vwOwner = nrOwner,
      vwCommitment = nrCommitment,
      vwSealed = nrSealed,
      vwYears = nrYears,
      vwDuration = nrDuration,
      vwPrice = nrPrice,
      vwFromBalance = maybe 0 buUsed (paymentBalance nrPayment),
      vwBalance = balanceJSON <$> balance,
      vwLatePayment = (\w -> J.object ["waiting" .= w]) <$> late,
      vwCommitTx = commitTxOf =<< commitStep,
      vwRegisterTx = phaseRegisterTx nrPhase,
      vwRevealAfter = cmRevealAfter <$> commit,
      vwRevealBefore = cmRevealBefore <$> commit,
      vwIntentNonce = readValue vrIntentNonce,
      vwAcceptance = acceptanceJSON <$> readValue vrAcceptance,
      vwUnread = ["intentNonce" | isUnread vrIntentNonce] <> ["acceptance" | isUnread vrAcceptance],
      vwExpiresAt = expiresAt,
      vwError = textEncode <$> failReason,
      vwCommunity = communityOf cfg (nrLabelHash r) (atCommunityName nrAgreedTerms),
      vwTerms = textEncode nrAgreedTerms
    }
  where
    NameRegistration {nrId = RegistrationId rid, nrVersion, nrMoves, nrPhase, nrLabelHash = LabelHash lh, nrReveal, nrOwner, nrCommitment, nrSealed} = r
    NameRegistration {nrYears, nrDuration, nrPrice, nrPayment, nrAgreedTerms} = r
    commitStep = phaseCommit nrPhase
    commit = case commitStep of
      Just (CSCommitted c) -> Just c
      _ -> Nothing
    (expiresAt, failReason) = case nrPhase of
      ORegistered _ _ expiry _ -> (Just expiry, Nothing)
      OFailed _ _ _ reason -> (Nothing, Just reason)
      _ -> (Nothing, Nothing)
    readValue = \case
      VRRead a -> Just a
      _ -> Nothing
    isUnread = \case
      VRUnread -> True
      _ -> False
    acceptanceJSON AcceptanceAsked {acCommunity, acPolicy, acTermsHash, acNonce} =
      J.object ["kind" .= termsKindText acCommunity, "policy" .= acPolicy, "termsHash" .= acTermsHash, "nonce" .= acNonce]

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
  Just lapse | now > lapse -> r {nrPhase = failedFrom lapse FRNotSignedInTime $ nrPhase r}
  _ -> r

lapseMoment :: NameRegistration -> Maybe UTCTime
lapseMoment = \case
  NameRegistration {nrPhase = OCommitted Commit {cmRevealBefore}, nrReveal = Nothing} -> Just $ chainTime cmRevealBefore
  _ -> Nothing
