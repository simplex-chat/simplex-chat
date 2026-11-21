{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | The requests only the owner code can sign: find and another name.
module StoreService.Web.Names.Owner (findHandler, retargetHandler) where

import Control.Concurrent.STM (atomically)
import Control.Logger.Simple (logError, logNote)
import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Bifunctor (first)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Network.HTTP.Types (status200)
import Network.Wai (Request, ResponseReceived)
import Simplex.Chat.PaymentService.Types (InvoiceId)
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (tshow)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Chain (Hex32 (..))
import StoreService.Names.Orders
import StoreService.Names.Phases (publish)
import StoreService.Names.Registration (OrderFields (..), RegistrationId (..), findMessage, parseAnyCaseAddress, retargetMessage)
import StoreService.Names.Reveal (RevealRequest, orderReveal)
import StoreService.Names.Store (BalanceUse (..), FailReason (..), NamePhase (..), NameRegistration (..), OrderPhase (..), findRegistrations, getRegistration, namePhase, paymentBalance, paymentInvoice)
import StoreService.Names.Turns (withRegistrationTurns)
import StoreService.Names.View (NameRefusal (..), registrationView)
import StoreService.Web.Http (Respond, jsonResponse, maxBodyBytes, nameBodyBytes, readBoundedBody)
import StoreService.Names.Worker (Retried (..), checkFailedOrder)
import StoreService.Orders (cancelNameInvoices)
import StoreService.Providers (Provider)
import StoreService.Waiters (Waiters)
import StoreService.Web.Names.Orders (Answer (..), answerNow, refusalAnswer, refused, sendAnswer)
import StoreService.Web.Names.Registration (registrationChanged, registrationRefusal, retryRefusal)

data FindRequest = FindRequest
  { frOwner :: Address,
    frTimestamp :: Word64,
    frSignature :: RecoverableSignature
  }

instance J.FromJSON FindRequest where
  parseJSON = J.withObject "find request" $ \o -> FindRequest <$> jsonField o "owner" (textWith $ first show . parseAnyCaseAddress) <*> o J..: "timestamp" <*> o J..: "signature"

newtype FindAnswer = FindAnswer {registrations :: [FoundOrder]}

data FoundOrder = FoundOrder
  { registrationId :: Text,
    invoiceId :: Maybe InvoiceId,
    phase :: NamePhase,
    createdAt :: UTCTime,
    labelHash :: Hex32,
    sealed :: Text,
    balanceSource :: Maybe Text,
    heldBy :: Maybe Text
  }

$(JQ.deriveToJSON defaultJSON {J.omitNothingFields = False} ''FoundOrder)

$(JQ.deriveToJSON defaultJSON ''FindAnswer)

data MoveRequest = MoveRequest {mvSigned :: SignedFields, mvReveal :: RevealRequest}

instance J.FromJSON MoveRequest where
  parseJSON = J.withObject "move" $ \o -> MoveRequest <$> J.parseJSON (J.Object o) <*> o J..: "reveal"

-- | POST /api/registration/find, the owner's orders newest first.
findHandler :: OrdersEnv -> Request -> Respond -> IO ResponseReceived
findHandler OrdersEnv {oeStore, oeNow} req respond =
  readBoundedBody maxBodyBytes req >>= requested >>= sendAnswer respond
  where
    requested = \case
      Just body -> either (refuse . ORBadRequest . ("a malformed find request: " <>) . T.pack) found $ J.eitherDecode body
      Nothing -> refuse $ ORBadRequest "a find request over the body cap"
    found FindRequest {frOwner, frTimestamp, frSignature} = do
      now <- oeNow
      runExceptT (mapM_ ExceptT [checkSigner frOwner (findMessage frOwner frTimestamp) frSignature, pure $ checkTimestamp now frTimestamp]) >>= \case
        Left refusal -> refuse refusal
        Right () -> do
          rs <- withTransaction oeStore (\db -> findRegistrations db frOwner now)
          pure $ answerNow $ jsonResponse status200 $ J.toJSON FindAnswer {registrations = map (uncurry foundOrder) rs}
    refuse = refused "POST /api/registration/find"

-- | balanceSource: the order whose balance paid this one; heldBy: the order whose invoice holds this one's balance.
foundOrder :: NameRegistration -> Maybe RegistrationId -> FoundOrder
foundOrder NameRegistration {nrId = RegistrationId rid, nrPayment, nrPhase, nrCreatedAt, nrLabelHash = LabelHash lh, nrSealed} holder =
  FoundOrder
    { registrationId = rid,
      invoiceId = paymentInvoice nrPayment,
      phase = namePhase nrPhase,
      createdAt = nrCreatedAt,
      labelHash = Hex32 lh,
      sealed = nrSealed,
      balanceSource = idText . buSource <$> paymentBalance nrPayment,
      heldBy = idText <$> holder
    }
  where
    idText (RegistrationId t) = t

-- | POST /api/registration/:id/name, another name or other links, under the turns of both names.
retargetHandler :: OrdersEnv -> [Provider] -> Waiters -> RegistrationId -> Request -> Respond -> IO ResponseReceived
retargetHandler env@OrdersEnv {oeStore, oeConfig, oeContracts, oeTurns, oeNow, oePhases} providers waiters rid@(RegistrationId ridText) req respond =
  readBoundedBody nameBodyBytes req >>= requested >>= sendAnswer respond
  where
    requested = \case
      Just body | Just v <- J.decode body -> either refuse signed (parseBody "move" v >>= \MoveRequest {mvSigned, mvReveal} -> (,mvReveal) <$> signedFields mvSigned)
      _ -> refuse $ ORBadRequest "a malformed move"
    signed ((fields, timestamp, signature), reveal) =
      withTransaction oeStore (`getRegistration` rid) >>= \case
        Nothing -> pure $ refusalAnswer NRNotFound
        Just r@NameRegistration {nrOwner} ->
          checkSigner nrOwner (retargetMessage rid fields timestamp) signature >>= \case
            Left refusal -> refuse refusal
            Right () ->
              withRegistrationTurns oeStore oeTurns r (ofLabelHash fields) (inTurns fields timestamp reveal)
                >>= maybe (pure $ refusalAnswer NRNotFound) (either (const changed) pure)
    inTurns fields timestamp reveal stored = do
      now <- oeNow
      moveTo now stored fields timestamp reveal
    moveTo now r@NameRegistration {nrPhase} fields timestamp reveal
      | OFailed _ FRSignatureRefused <- nrPhase = refuse $ ORBadRequest "the order failed with its signature refused, so support settles it"
      | orderFieldsOf r == fields = answerNow . jsonResponse status200 . J.toJSON <$> registrationView oeStore oeConfig oeContracts now r
      | Left refusal <- checkTimestamp now timestamp = refuse refusal
      | not (movable now r) = refuse $ ORBadRequest "the order is not movable"
      | namePhase nrPhase == PFailed =
          checkFailedOrder env r >>= \case
            Right () -> moveChecked now r fields reveal
            Left (RTWritten v) -> written env rid v . refusalAnswer <$> registrationChanged env rid
            Left RTChanged -> changed
            Left (RTRefused refusal) -> refusalAnswer (retryRefusal refusal) <$ logNote (what <> " refused: " <> tshow refusal)
      | otherwise = moveChecked now r fields reveal
    moveChecked now r@NameRegistration {nrOwner, nrMoves, nrYears} fields@OrderFields {ofLabelHash, ofDuration} reveal
      | nrMoves >= nMaxOrderMoves oeConfig = refusalAnswer <$> registrationRefusal env NRMoveLimit rid
      | otherwise =
          runExceptT (mapM_ ExceptT [checkMoveMeets oeStore r fields now, pure $ checkDuration oeConfig now years ofDuration, checkTerms oeConfig oeContracts fields, checkNameFree oeContracts oeStore (Just rid) ofLabelHash] >> ExceptT (orderReveal oeConfig oeContracts now nrOwner fields reveal)) >>= \case
            Left refusal -> refuse refusal
            Right rv -> write now r fields rv
      where
        years = orderYears fields nrYears
    write now r fields rv =
      retargetOrder env r fields rv >>= \case
        Right v -> movedAnswer now v
        Left WRChanged -> changed
        Left (WRRefused refusal) -> refuse refusal
    movedAnswer now v =
      withTransaction oeStore (`getRegistration` rid) >>= \case
        Just r -> heldAfter . written env rid v . answerNow . jsonResponse status200 . J.toJSON <$> registrationView oeStore oeConfig oeContracts now r
        Nothing -> refusalAnswer NRInternal <$ logError ("POST /api/registration/:id/name: order " <> ridText <> " is not readable after its move")
    changed = refusalAnswer <$> registrationChanged env rid
    heldAfter a@Answer {aAfter} = a {aAfter = aAfter >> cancelNameInvoices oeStore waiters (Just oePhases) providers rid}
    what = "POST /api/registration/:id/name"
    refuse = refused what

-- | Published after the turn, as the answer is sent.
written :: OrdersEnv -> RegistrationId -> Int64 -> Answer -> Answer
written OrdersEnv {oePhases} rid v a = a {aAfter = atomically $ publish oePhases rid v}

