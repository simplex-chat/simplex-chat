{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.View
  ( ViewReads (..),
    registrationView,
    readViewChain,
    viewJSON,
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
import StoreService.Names.Community (communityJSON)
import StoreService.Names.Contracts (Contracts, sponsorNonce)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Store (NamePhase (..), NameRegistration (..), NameReveal (..), getRegistration)

-- | The view's chain reads; a field whose read failed is null and named in vrUnread.
data ViewReads = ViewReads
  { vrIntentNonce :: Maybe Word64,
    vrUnread :: [Text]
  }
  deriving (Eq, Show)

registrationView :: DBStore -> NamesConfig -> Contracts -> NameRegistration -> IO J.Value
registrationView _ cfg ct r = viewJSON cfg r <$> readViewChain ct r

-- | The intent nonce only in committed.
readViewChain :: Contracts -> NameRegistration -> IO ViewReads
readViewChain ct NameRegistration {nrPhase, nrOwner} = case nrPhase of
  PCommitted -> sponsorNonce ct nrOwner >>= \case
    Right nonce -> pure none {vrIntentNonce = Just nonce}
    Left _ -> pure none {vrUnread = ["intentNonce"]}
  _ -> pure none
  where
    none = ViewReads {vrIntentNonce = Nothing, vrUnread = []}

viewJSON :: NamesConfig -> NameRegistration -> ViewReads -> J.Value
viewJSON cfg@NamesConfig {nTLD, nChain = ChainConfig {cChainId, cResolver, cSponsor}} r ViewReads {vrIntentNonce, vrUnread} =
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
      "commitTx" .= (txText <$> nrCommitTx),
      "registerTx" .= (txText <$> nrRegisterTx),
      "revealAfter" .= nrRevealAfter,
      "revealBefore" .= nrRevealBefore,
      "intentNonce" .= vrIntentNonce,
      "unread" .= vrUnread,
      "expiresAt" .= nrExpiresAt,
      "error" .= nrError,
      "community" .= communityJSON cfg (nrLabelHash r),
      "terms" .= (if T.null nrAgreedTerms then Nothing else Just nrAgreedTerms)
    ]
  where
    NameRegistration {nrId = RegistrationId rid, nrVersion, nrMoves, nrLinksRefusedAt, nrPhase, nrLabelHash = LabelHash lh, nrReveal, nrOwner, nrCommitment, nrSealed} = r
    NameRegistration {nrYears, nrDuration, nrPrice, nrCommitTx, nrRegisterTx, nrRevealAfter, nrRevealBefore, nrExpiresAt, nrError, nrAgreedTerms} = r
    txText (TxHash h) = hex h

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
