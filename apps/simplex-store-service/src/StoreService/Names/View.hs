{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.View
  ( ViewReads (..),
    ViewRead (..),
    registrationView,
    readViewChain,
    viewJSON,
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
import Data.Int (Int64)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String (strEncode, textEncode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Names.Abi (hex)
import StoreService.Names.Chain (ChainError, TxHash (..), chainTime)
import StoreService.Names.Community (AcceptanceAsked (..), communityJSON, currentAcceptance)
import StoreService.Names.Contracts (Contracts (ctDeadline, ctViewSlots), sponsorNonce)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Profiles (deadlineIn, withSlot)
import StoreService.Names.Registration (termsKindText, tldLabel)
import StoreService.Names.Signatures (AgreedTerms (..))
import StoreService.Names.Store (Balance (..), BalanceHold (..), BalanceUse (..), FailReason (..), HoldState (..), LateWait, NameRegistration (..), NameReveal (..), OrderPhase (..), getBalance, getRegistration, heldBy, lateWait, namePhase, paymentBalance, paymentInvoice)

-- | The view's chain reads; a field whose read failed is null and named in unread.
data ViewReads = ViewReads
  { vrIntentNonce :: ViewRead Word64,
    vrAcceptance :: ViewRead AcceptanceAsked
  }
  deriving (Eq, Show)

data ViewRead a = VRNotAsked | VRUnread | VRRead a
  deriving (Eq, Show)

-- | The balance as it reads at the given time, when a hold may have become overdue, and what a closed order's late money waits on.
registrationView :: DBStore -> NamesConfig -> Contracts -> UTCTime -> NameRegistration -> IO J.Value
registrationView st cfg ct now r = do
  (balance, late) <- withTransaction st $ \db -> do
    balance <- getBalance db (nrId r) now
    late <- maybe (pure Nothing) (lateWait db) (paymentInvoice $ nrPayment r)
    pure (balance, late)
  viewJSON cfg r balance late <$> readViewChain ct r

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

viewJSON :: NamesConfig -> NameRegistration -> Maybe Balance -> Maybe LateWait -> ViewReads -> J.Value
viewJSON cfg@NamesConfig {nTLD, nChain = ChainConfig {cChainId, cResolver, cSponsor}} r balance late ViewReads {vrIntentNonce, vrAcceptance} =
  J.object
    [ "id" .= rid,
      "version" .= nrVersion,
      "moves" .= nrMoves,
      "linksRefused" .= isJust nrLinksRefusedAt,
      "phase" .= (textEncode (namePhase nrPhase) :: Text),
      "labelHash" .= hex lh,
      "label" .= (rvLabel <$> nrReveal),
      "owner" .= nrOwner,
      "commitment" .= hex nrCommitment,
      "sealed" .= nrSealed,
      "resolver" .= cResolver,
      "sponsor" .= cSponsor,
      "chainId" .= cChainId,
      "tld" .= tldLabel nTLD,
      "years" .= nrYears,
      "duration" .= nrDuration,
      "price" .= nrPrice,
      "fromBalance" .= maybe 0 buUsed (paymentBalance nrPayment),
      "balance" .= (balanceJSON <$> balance),
      "latePayment" .= ((\w -> J.object ["waiting" .= (textEncode w :: Text)]) <$> late),
      "commitTx" .= (txText <$> nrCommitTx),
      "registerTx" .= (txText <$> nrRegisterTx),
      "revealAfter" .= nrRevealAfter,
      "revealBefore" .= nrRevealBefore,
      "intentNonce" .= readValue vrIntentNonce,
      "acceptance" .= (acceptanceJSON <$> readValue vrAcceptance),
      "unread" .= (["intentNonce" | isUnread vrIntentNonce] <> ["acceptance" | isUnread vrAcceptance] :: [Text]),
      "expiresAt" .= expiresAt,
      "error" .= (textEncode <$> failReason :: Maybe Text),
      "community" .= communityJSON cfg (nrLabelHash r) (atCommunityName nrAgreedTerms),
      "terms" .= (textEncode nrAgreedTerms :: Text)
    ]
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
