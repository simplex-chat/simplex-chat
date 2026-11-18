{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The requests only the owner code can sign: find and another name.
module StoreService.Web.Names.Owner (findHandler, retargetHandler) where

import Control.Concurrent.STM (atomically)
import Control.Logger.Simple (logError, logNote)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Word (Word64)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, ResponseReceived)
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (tshow)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Chain (Hex32 (..))
import StoreService.Names.Orders
import StoreService.Names.Phases (publish)
import StoreService.Names.Registration (OrderFields (..), RegistrationId (..), findMessage, parseAnyCaseAddress, retargetMessage)
import StoreService.Names.Store (BalanceUse (..), NamePhase (..), NameRegistration (..), findRegistrations, getRegistration, namePhase, paymentBalance, paymentInvoice)
import StoreService.Names.Turns (withRegistrationTurns)
import StoreService.Names.View (phaseText, registrationAt, registrationView)
import StoreService.Web.Http (Respond, internalError, jsonResponse, maxBodyBytes, nameBodyBytes, notFound, readBoundedBody)
import StoreService.Names.Worker (Retried (..), checkFailedOrder)
import StoreService.Web.Names.Orders (Answer (..), answerNow, refused, sendAnswer)
import StoreService.Web.Names.Registration (registrationChangedResponse, registrationRefusalResponse, retryRefusalResponse)

data FindRequest = FindRequest
  { frOwner :: Address,
    frTimestamp :: Word64,
    frSignature :: RecoverableSignature
  }

instance J.FromJSON FindRequest where
  parseJSON = J.withObject "find request" $ \o -> FindRequest <$> jsonField o "owner" (textWith $ first show . parseAnyCaseAddress) <*> o J..: "timestamp" <*> o J..: "signature"

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
          pure $ answerNow $ jsonResponse status200 $ J.object ["registrations" .= map (\(r, holder) -> foundJSON (registrationAt now r) holder) rs]
    refuse = refused "POST /api/registration/find"

-- | balanceSource: the order whose balance paid this one; heldBy: the order whose invoice holds this one's balance.
foundJSON :: NameRegistration -> Maybe RegistrationId -> J.Value
foundJSON NameRegistration {nrId = RegistrationId rid, nrPayment, nrPhase, nrCreatedAt, nrLabelHash = LabelHash lh, nrSealed} holder =
  J.object
    [ "registrationId" .= rid,
      "invoiceId" .= paymentInvoice nrPayment,
      "phase" .= phaseText nrPhase,
      "createdAt" .= nrCreatedAt,
      "labelHash" .= Hex32 lh,
      "sealed" .= nrSealed,
      "balanceSource" .= (idText . buSource <$> paymentBalance nrPayment),
      "heldBy" .= (idText <$> holder)
    ]
  where
    idText (RegistrationId t) = t

-- | POST /api/registration/:id/name, another name or other links, under the turns of both names.
retargetHandler :: OrdersEnv -> RegistrationId -> Request -> Respond -> IO ResponseReceived
retargetHandler env@OrdersEnv {oeStore, oeConfig, oeContracts, oeTurns, oeNow} rid@(RegistrationId ridText) req respond =
  readBoundedBody nameBodyBytes req >>= requested >>= sendAnswer respond
  where
    requested = \case
      Just body | Just v <- J.decode body -> either refuse signed (parseBody "move" v >>= signedFields)
      _ -> refuse $ ORBadRequest "a malformed move"
    signed (fields, timestamp, signature) =
      withTransaction oeStore (`getRegistration` rid) >>= \case
        Nothing -> pure $ answerNow notFound
        Just r@NameRegistration {nrOwner} ->
          checkSigner nrOwner (retargetMessage rid fields timestamp) signature >>= \case
            Left refusal -> refuse refusal
            Right () ->
              withRegistrationTurns oeStore oeTurns r (ofLabelHash fields) (inTurns fields timestamp)
                >>= maybe (pure $ answerNow notFound) (either (const changed) pure)
    inTurns fields timestamp stored = do
      now <- oeNow
      moveTo now (registrationAt now stored) fields timestamp
    moveTo now r@NameRegistration {nrPhase} fields timestamp
      | orderFieldsOf r == fields = answerNow . jsonResponse status200 . J.toJSON <$> registrationView oeStore oeConfig oeContracts now r
      | Left refusal <- checkTimestamp now timestamp = refuse refusal
      | not (movable r) = refuse $ ORBadRequest "the order is not movable"
      | namePhase nrPhase == PFailed =
          checkFailedOrder env r >>= \case
            Right () -> moveChecked now r fields
            Left (RTWritten v) -> written env rid v <$> registrationChangedResponse env rid
            Left RTChanged -> changed
            Left (RTRefused refusal) -> answerNow (retryRefusalResponse refusal) <$ logNote (what <> " refused: " <> tshow refusal)
      | otherwise = moveChecked now r fields
    moveChecked now r@NameRegistration {nrMoves, nrYears} fields@OrderFields {ofLabelHash, ofDuration}
      | nrMoves >= nMaxOrderMoves oeConfig = answerNow <$> registrationRefusalResponse env "move_limit" rid
      | otherwise =
          runExceptT (mapM_ ExceptT [checkMoveMeets oeStore r fields now, pure $ checkDuration oeConfig now years ofDuration, checkTerms oeConfig oeContracts fields, checkNameFree oeContracts oeStore ofLabelHash]) >>= \case
            Left refusal -> refuse refusal
            Right () -> write now r fields
      where
        years = orderYears fields nrYears
    write now r fields =
      retargetOrder env r fields >>= \case
        Right v -> movedAnswer now v
        Left WRChanged -> changed
        Left (WRRefused refusal) -> refuse refusal
    movedAnswer now v =
      withTransaction oeStore (`getRegistration` rid) >>= \case
        Just r -> written env rid v . jsonResponse status200 . J.toJSON <$> registrationView oeStore oeConfig oeContracts now (registrationAt now r)
        Nothing -> answerNow internalError <$ logError ("POST /api/registration/:id/name: order " <> ridText <> " is not readable after its move")
    changed = answerNow <$> registrationChangedResponse env rid
    what = "POST /api/registration/:id/name"
    refuse = refused what

-- | Published after the turn, as the answer is sent.
written :: OrdersEnv -> RegistrationId -> Int64 -> Response -> Answer
written OrdersEnv {oePhases} rid v aResponse = Answer {aResponse, aAfter = atomically $ publish oePhases rid v}

