{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.View (ViewReads (..), registrationView, readViewChain, viewJSON, tldText) where

import Data.Aeson ((.=))
import qualified Data.Aeson as J
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String (strEncode, textEncode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexTLD)
import Simplex.Messaging.Util (safeDecodeUtf8)
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Names.Abi (hex)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Contracts (Contracts, codeLengthFor, sponsorNonce)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Store (NamePhase (..), NameRegistration (..), NameReveal (..), PrepaidCode (..), RegistrationPayment (..), getPrepaidCodeById)

-- | The view's chain reads; a field whose read failed is null and named in vrUnread.
data ViewReads = ViewReads
  { vrIntentNonce :: Maybe Word64,
    vrCodeLength :: Maybe Int,
    vrUnread :: [Text]
  }
  deriving (Eq, Show)

registrationView :: DBStore -> NamesConfig -> Contracts -> NameRegistration -> IO J.Value
registrationView st cfg ct r = viewJSON cfg r <$> readViewChain st ct r

-- | The intent nonce only in committed; a code length for a paid order in taken, and the issued code's in code_issued.
readViewChain :: DBStore -> Contracts -> NameRegistration -> IO ViewReads
readViewChain st ct NameRegistration {nrPhase, nrOwner, nrPayment, nrPrice, nrYears, nrIssuedCodeId} = case nrPhase of
  PCommitted -> sponsorNonce ct nrOwner >>= \case
    Right nonce -> pure none {vrIntentNonce = Just nonce}
    Left _ -> pure none {vrUnread = ["intentNonce"]}
  PTaken | RPInvoice {} <- nrPayment -> codeLengthFor ct nrPrice nrYears >>= \case
    Right len -> pure none {vrCodeLength = len}
    Left _ -> pure none {vrUnread = ["codeLength"]}
  PCodeIssued -> do
    code <- maybe (pure Nothing) (\codeId -> withTransaction st (`getPrepaidCodeById` codeId)) nrIssuedCodeId
    pure none {vrCodeLength = pcMinLength <$> code}
  _ -> pure none
  where
    none = ViewReads {vrIntentNonce = Nothing, vrCodeLength = Nothing, vrUnread = []}

viewJSON :: NamesConfig -> NameRegistration -> ViewReads -> J.Value
viewJSON NamesConfig {nTLD, nChain = ChainConfig {cChainId, cResolver, cSponsor}} r ViewReads {vrIntentNonce, vrCodeLength, vrUnread} =
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
      "codeLength" .= vrCodeLength,
      "commitTx" .= (txText <$> nrCommitTx),
      "registerTx" .= (txText <$> nrRegisterTx),
      "revealAfter" .= nrRevealAfter,
      "revealBefore" .= nrRevealBefore,
      "intentNonce" .= vrIntentNonce,
      "unread" .= vrUnread,
      "expiresAt" .= nrExpiresAt,
      "error" .= nrError
    ]
  where
    NameRegistration {nrId = RegistrationId rid, nrVersion, nrMoves, nrLinksRefusedAt, nrPhase, nrLabelHash = LabelHash lh, nrReveal, nrOwner, nrCommitment, nrSealed} = r
    NameRegistration {nrYears, nrDuration, nrPrice, nrCommitTx, nrRegisterTx, nrRevealAfter, nrRevealBefore, nrExpiresAt, nrError} = r
    txText (TxHash h) = hex h

-- | EIP-55, as the page compares owners.
addressText :: Address -> Text
addressText = safeDecodeUtf8 . strEncode

-- | The TLD as the page shows it after the name's dot.
tldText :: SimplexTLD -> Text
tldText = T.dropWhile (== '.') . safeDecodeUtf8 . strEncode
