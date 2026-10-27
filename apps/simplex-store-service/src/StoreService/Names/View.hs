{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.View
  ( ViewReads (..),
    registrationView,
    readViewChain,
    viewJSON,
    balanceJSON,
    addressText,
    tldText,
    registrationAt,
    lapseMoment,
    notSignedInTime,
    registrationVersion,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as J
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String (strEncode, textEncode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexTLD)
import Simplex.Messaging.Util (safeDecodeUtf8)
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Names.Abi (hex)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Community (AcceptanceAsked (..), agreedTermsHash, communityJSON, currentAcceptance)
import StoreService.Names.Contracts (Contracts, sponsorNonce)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Store (Balance (..), BalanceHold (..), LateWait (..), NamePhase (..), NameRegistration (..), NameReveal (..), RegistrationPayment (..), getBalance, getRegistration, heldBy, lateWait)

-- | The view's chain reads; a field whose read failed is null and named in vrUnread.
data ViewReads = ViewReads
  { vrIntentNonce :: Maybe Word64,
    vrUnread :: [Text],
    vrAcceptance :: Maybe AcceptanceAsked
  }
  deriving (Eq, Show)

-- | The balance as it reads at the given time, when a hold may have become overdue, and what a closed order's late money waits on.
registrationView :: DBStore -> NamesConfig -> Contracts -> UTCTime -> NameRegistration -> IO J.Value
registrationView st cfg ct now r = do
  (balance, late) <- withTransaction st $ \db -> do
    balance <- getBalance db (nrId r) now
    late <- case nrPayment r of
      RPInvoice _ invId -> lateWait db invId
      _ -> pure Nothing
    pure (balance, late)
  viewJSON cfg r balance late <$> readViewChain ct r

-- | The intent nonce only in committed; the acceptance too, until a reveal is stored, for a name the policy lists.
readViewChain :: Contracts -> NameRegistration -> IO ViewReads
readViewChain ct NameRegistration {nrPhase, nrOwner, nrLabelHash, nrReveal} = case nrPhase of
  PCommitted -> do
    vr <- sponsorNonce ct nrOwner >>= \case
      Right nonce -> pure none {vrIntentNonce = Just nonce}
      Left _ -> pure none {vrUnread = ["intentNonce"]}
    if isJust nrReveal
      then pure vr
      else currentAcceptance ct nrLabelHash nrOwner <&> \case
        Right vrAcceptance -> vr {vrAcceptance}
        Left _ -> vr {vrUnread = vrUnread vr <> ["acceptance"]}
  _ -> pure none
  where
    none = ViewReads {vrIntentNonce = Nothing, vrUnread = [], vrAcceptance = Nothing}

viewJSON :: NamesConfig -> NameRegistration -> Maybe Balance -> Maybe LateWait -> ViewReads -> J.Value
viewJSON cfg@NamesConfig {nTLD, nChain = ChainConfig {cChainId, cResolver, cSponsor}} r balance late ViewReads {vrIntentNonce, vrUnread, vrAcceptance} =
  J.object
    [ "id" .= rid,
      "version" .= nrVersion,
      "moves" .= nrMoves,
      "linksRefused" .= isJust nrLinksRefusedAt,
      "phase" .= (textEncode nrPhase :: Text),
      "labelHash" .= hex lh,
      "label" .= (rvLabel <$> nrReveal),
      "owner" .= addressText nrOwner,
      "commitment" .= hex nrCommitment,
      "sealed" .= nrSealed,
      "resolver" .= addressText cResolver,
      "sponsor" .= addressText cSponsor,
      "chainId" .= cChainId,
      "tld" .= tldText nTLD,
      "years" .= nrYears,
      "duration" .= nrDuration,
      "price" .= nrPrice,
      "fromBalance" .= nrBalanceUsed,
      "balance" .= (balanceJSON <$> balance),
      "latePayment" .= ((\w -> J.object ["waiting" .= waitingText w]) <$> late),
      "commitTx" .= (txText <$> nrCommitTx),
      "registerTx" .= (txText <$> nrRegisterTx),
      "revealAfter" .= nrRevealAfter,
      "revealBefore" .= nrRevealBefore,
      "intentNonce" .= vrIntentNonce,
      "acceptance" .= (acceptanceJSON <$> vrAcceptance),
      "unread" .= vrUnread,
      "expiresAt" .= nrExpiresAt,
      "error" .= nrError,
      "community" .= communityJSON cfg (nrLabelHash r) (agreedTermsHash nrAgreedTerms),
      "terms" .= (if T.null nrAgreedTerms then Nothing else Just nrAgreedTerms)
    ]
  where
    NameRegistration {nrId = RegistrationId rid, nrVersion, nrMoves, nrLinksRefusedAt, nrPhase, nrLabelHash = LabelHash lh, nrReveal, nrOwner, nrCommitment, nrSealed} = r
    NameRegistration {nrYears, nrDuration, nrPrice, nrBalanceUsed, nrCommitTx, nrRegisterTx, nrRevealAfter, nrRevealBefore, nrExpiresAt, nrError, nrAgreedTerms} = r
    txText (TxHash h) = hex h
    acceptanceJSON AcceptanceAsked {acPolicy, acTermsHash, acNonce} =
      J.object ["policy" .= addressText acPolicy, "termsHash" .= hex acTermsHash, "nonce" .= acNonce]
    waitingText :: LateWait -> Text
    waitingText = \case
      LWConfirming -> "confirming"
      LWCrediting -> "crediting"
      LWSupport -> "support"

-- | The holder is not named, so an order's link discloses no other order. A funded hold has no end time.
balanceJSON :: Balance -> J.Value
balanceJSON b@Balance {blAmount, blRefunded} =
  J.object ["amount" .= blAmount, "held" .= isJust hold, "heldUntil" .= (bhUntil <$> (hold >>= unfunded)), "refunded" .= blRefunded]
  where
    hold = heldBy b
    unfunded h@BalanceHold {bhFunded} = if bhFunded then Nothing else Just h

-- | EIP-55, as the page compares owners.
addressText :: Address -> Text
addressText = safeDecodeUtf8 . strEncode

-- | The TLD as the page shows it after the name's dot.
tldText :: SimplexTLD -> Text
tldText = T.dropWhile (== '.') . safeDecodeUtf8 . strEncode

-- | Versions start at 1, so a registration deleted during a hold reads as changed.
registrationVersion :: DBStore -> RegistrationId -> IO Int64
registrationVersion st rid = maybe 0 nrVersion <$> withTransaction st (`getRegistration` rid)

notSignedInTime :: Text
notSignedInTime = "The name was not signed in time"

-- | Past revealBefore with no reveal, the commitment can no longer register, so it reads failed.
registrationAt :: UTCTime -> NameRegistration -> NameRegistration
registrationAt now r = case lapseMoment r of
  Just lapse | now > lapse -> r {nrPhase = PFailed, nrError = Just notSignedInTime}
  _ -> r

lapseMoment :: NameRegistration -> Maybe UTCTime
lapseMoment = \case
  NameRegistration {nrPhase = PCommitted, nrReveal = Nothing, nrRevealBefore = Just before} -> Just $ posixSecondsToUTCTime $ fromIntegral before
  _ -> Nothing
