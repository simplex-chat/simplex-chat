{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | The requests only the owner code can sign: find and the selection of a name.
module StoreService.Web.Names.Owner (findHandler, selectNameHandler) where

import Control.Concurrent.STM (atomically)
import Control.Logger.Simple (logError)
import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
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
import StoreService.Names.Chain (Hex32 (..))
import StoreService.Names.Orders
import StoreService.Names.Phases (RegistrationId (..), publish)
import StoreService.Names.Reveal (RevealRequest)
import StoreService.Names.Signatures (OrderFields (..), findMessage, parseAnyCaseAddress, selectionMessage)
import StoreService.Names.Store (NamePhase (..), NameRegistrationOrder (..), OrderPhase (..), findRegistrations, getRegistration, namePhase, paymentInvoice)
import StoreService.Names.Turns (withRegistrationTurns)
import StoreService.Names.View (NameRefusal (..), registrationView)
import StoreService.Web.Http (Respond, jsonResponse, maxBodyBytes, nameBodyBytes, readBoundedBody)
import StoreService.Providers (Provider)
import StoreService.Waiters (Waiters)
import StoreService.Web.Names.Orders (Answer (..), announceHeld, answerNow, orderChecked, refusalAnswer, refused, sendAnswer)
import StoreService.Web.Names.Registration (registrationChanged)

data FindRequest = FindRequest
  { frOwner :: Address,
    frTimestamp :: Word64,
    frSignature :: RecoverableSignature
  }

instance J.FromJSON FindRequest where
  parseJSON = J.withObject "find request" $ \o -> FindRequest <$> JT.explicitParseField (textWith $ first show . parseAnyCaseAddress) o "owner" <*> o J..: "timestamp" <*> o J..: "signature"

newtype FindAnswer = FindAnswer {registrations :: [FoundOrder]}

data FoundOrder = FoundOrder
  { registrationId :: RegistrationId,
    invoiceId :: Maybe InvoiceId,
    phase :: NamePhase,
    createdAt :: UTCTime,
    labelHash :: Hex32,
    sealed :: Text
  }

$(JQ.deriveToJSON defaultJSON {J.omitNothingFields = False} ''FoundOrder)

$(JQ.deriveToJSON defaultJSON ''FindAnswer)

data SelectionRequest = SelectionRequest {srSigned :: SignedFields, srReveal :: RevealRequest}

instance J.FromJSON SelectionRequest where
  parseJSON = J.withObject "selection" $ \o -> SelectionRequest <$> J.parseJSON (J.Object o) <*> o J..: "reveal"

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
          rs <- withTransaction oeStore (`findRegistrations` frOwner)
          pure $ answerNow $ jsonResponse status200 $ J.toJSON FindAnswer {registrations = map foundOrder rs}
    refuse = refused "POST /api/registration/find"

foundOrder :: NameRegistrationOrder -> FoundOrder
foundOrder NameRegistrationOrder {nrId, nrPayment, nrPhase, nrCreatedAt, nrLabelHash = LabelHash lh, nrSealed} =
  FoundOrder
    { registrationId = nrId,
      invoiceId = paymentInvoice nrPayment,
      phase = namePhase nrPhase,
      createdAt = nrCreatedAt,
      labelHash = Hex32 lh,
      sealed = nrSealed
    }

-- | POST /api/registration/:id/name, a name selected for a taken order, under the turns of both names, with a new order's checks.
selectNameHandler :: OrdersEnv -> [Provider] -> Waiters -> RegistrationId -> Request -> Respond -> IO ResponseReceived
selectNameHandler env@OrdersEnv {oeStore, oeConfig, oeContracts, oeTurns, oeNow} providers waiters rid@(RegistrationId ridText) req respond =
  readBoundedBody nameBodyBytes req >>= requested >>= sendAnswer respond
  where
    requested = \case
      Just body | Just v <- J.decode body -> either refuse signed (parseBody "selection" v >>= \SelectionRequest {srSigned, srReveal} -> (,srReveal) <$> signedFields srSigned)
      _ -> refuse $ ORBadRequest "a malformed selection"
    signed ((fields, timestamp, signature), reveal) =
      withTransaction oeStore (`getRegistration` rid) >>= \case
        Nothing -> pure $ refusalAnswer NRNotFound
        Just r@NameRegistrationOrder {nrOwner} ->
          checkSigner nrOwner (selectionMessage rid fields timestamp) signature >>= \case
            Left refusal -> refuse refusal
            Right () ->
              withRegistrationTurns oeStore oeTurns r (ofLabelHash fields) (inTurns OrderRequest {orOwner = nrOwner, orFields = fields, orTimestamp = timestamp, orSignature = signature} reveal)
                >>= maybe (pure $ refusalAnswer NRNotFound) (either (const changed) pure)
    inTurns order reveal stored = do
      now <- oeNow
      select now stored order reveal
    select now r@NameRegistrationOrder {nrYears, nrPrice, nrPayment, nrDisputeDays, nrReveal} order@OrderRequest {orFields} reveal
      | orderFieldsOf r == orFields = answerNow . jsonResponse status200 . J.toJSON <$> registrationView oeStore oeConfig oeContracts now r
      | OTaken takenAt <- nrPhase r =
          withTransaction oeStore (\db -> overdueOrdersMet db order now) >>= \case
            Nothing -> refuse OROrderExists
            Just _ ->
              orderChecked env now order (orderYears orFields nrYears) (nrPrice <$ paymentInvoice nrPayment) nrDisputeDays nrReveal reveal >>= \case
                Left refusal -> refuse refusal
                Right rv -> write now r takenAt order rv
      | otherwise = refuse $ ORBadRequest "the order's name is selected"
    write now r takenAt order rv =
      selectName env order r takenAt rv >>= \case
        Right closed -> selectedAnswer now closed
        Left WRChanged -> changed
        Left (WRRefused refusal) -> refuse refusal
    selectedAnswer now closed =
      withTransaction oeStore (`getRegistration` rid) >>= \case
        Just r@NameRegistrationOrder {nrVersion} -> heldAfter closed . written env rid nrVersion . answerNow . jsonResponse status200 . J.toJSON <$> registrationView oeStore oeConfig oeContracts now r
        Nothing -> refusalAnswer NRInternal <$ logError ("POST /api/registration/:id/name: order " <> ridText <> " is not readable after its selection")
    changed = refusalAnswer <$> registrationChanged env rid
    heldAfter closed a@Answer {aAfter} = a {aAfter = aAfter >> announceHeld env providers waiters closed rid}
    what = "POST /api/registration/:id/name"
    refuse = refused what

-- | Published after the turn, as the answer is sent.
written :: OrdersEnv -> RegistrationId -> Int64 -> Answer -> Answer
written OrdersEnv {oePhases} rid v a = a {aAfter = atomically $ publish oePhases rid v}

