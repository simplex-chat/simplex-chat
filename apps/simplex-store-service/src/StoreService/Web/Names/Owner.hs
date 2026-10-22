{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The requests only the owner code can sign: find and another name.
module StoreService.Web.Names.Owner (findHandler, retargetHandler) where

import Control.Concurrent.STM (atomically)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, ResponseReceived)
import Simplex.Chat.PaymentService.Types (InvoiceId (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Encoding.String (textEncode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (eitherToMaybe, tshow)
import StoreService.Config (NamesConfig (..))
import StoreService.Log (logError, logInfo)
import StoreService.Names.Abi (hex)
import StoreService.Names.Orders
import StoreService.Names.Phases (RegistrationId (..), publish)
import StoreService.Names.Signatures (OrderFields (..), findMessage, parseAnyCaseAddress, parseSignature, retargetMessage)
import StoreService.Names.Store (NamePhase (..), NameRegistration (..), RegistrationPayment (..), findRegistrations, getRegistration)
import StoreService.Names.Turns (withLabelTurns)
import StoreService.Names.View (registrationAt, registrationView)
import StoreService.Web.Http (Respond, internalError, jsonResponse, maxBodyBytes, nameBodyBytes, notFound, readBoundedBody)
import StoreService.Web.Names.Orders (Answer (..), answerNow, firstRefusal, orderRefusalResponse, sendAnswer)
import StoreService.Web.Names.Registration (registrationChangedResponse, registrationRefusalResponse)

data FindRequest = FindRequest
  { frOwner :: Address,
    frTimestamp :: Word64,
    frSignature :: RecoverableSignature
  }

-- | POST /api/registration/find, the owner's orders newest first.
findHandler :: OrdersEnv -> Request -> Respond -> IO ResponseReceived
findHandler OrdersEnv {oeStore, oeNow} req respond =
  readBoundedBody maxBodyBytes req >>= requested >>= sendAnswer respond
  where
    requested = \case
      Just body | Just (J.Object o) <- J.decode body, Just request <- parseFind o -> found request
      _ -> refuse ORBadRequest
    found FindRequest {frOwner, frTimestamp, frSignature} = do
      now <- oeNow
      firstRefusal [checkSigner frOwner (findMessage frOwner frTimestamp) frSignature, pure $ checkTimestamp now frTimestamp] >>= \case
        Just refusal -> refuse refusal
        Nothing ->
          withTransaction oeStore (`findRegistrations` frOwner) >>= \case
            [] -> pure $ answerNow notFound
            rs -> pure $ answerNow $ jsonResponse status200 $ J.object ["registrations" .= map (foundJSON . registrationAt now) rs]
    refuse = refused "POST /api/registration/find"

parseFind :: J.Object -> Maybe FindRequest
parseFind o = do
  frOwner <- eitherToMaybe . parseAnyCaseAddress =<< textField o "owner"
  frTimestamp <- integerField =<< KM.lookup "timestamp" o
  frSignature <- eitherToMaybe . parseSignature =<< textField o "signature"
  pure FindRequest {frOwner, frTimestamp, frSignature}

foundJSON :: NameRegistration -> J.Value
foundJSON NameRegistration {nrId = RegistrationId rid, nrPayment, nrPhase, nrCreatedAt, nrLabelHash = LabelHash lh, nrSealed} =
  J.object
    [ "registrationId" .= rid,
      "invoiceId" .= invoiceId,
      "phase" .= (textEncode nrPhase :: Text),
      "createdAt" .= nrCreatedAt,
      "labelHash" .= hex lh,
      "sealed" .= nrSealed
    ]
  where
    invoiceId = case nrPayment of
      RPInvoice _ (InvoiceId t) -> Just t
      RPCode _ -> Nothing
      RPBalance -> Nothing

-- | POST /api/registration/:id/name, another name or other links, under the turns of both names.
retargetHandler :: OrdersEnv -> Text -> Request -> Respond -> IO ResponseReceived
retargetHandler env@OrdersEnv {oeStore, oeConfig, oeContracts, oeTurns, oeNow} ridText req respond =
  readBoundedBody nameBodyBytes req >>= requested >>= sendAnswer respond
  where
    rid = RegistrationId ridText
    requested = \case
      Just body | Just (J.Object o) <- J.decode body, Just request@(fields, _, _) <- parseOrderFields o -> either refuse (const $ signed request) (checkLength fields)
      _ -> refuse ORBadRequest
    signed (fields, timestamp, signature) =
      withTransaction oeStore (`getRegistration` rid) >>= \case
        Nothing -> pure $ answerNow notFound
        Just NameRegistration {nrOwner, nrLabelHash} ->
          checkSigner nrOwner (retargetMessage rid fields timestamp) signature >>= \case
            Left refusal -> refuse refusal
            Right () -> withLabelTurns oeTurns nrLabelHash (ofLabelHash fields) $ inTurns [nrLabelHash, ofLabelHash fields] fields timestamp
    -- another move can take the order to a name whose turn this request does not hold
    inTurns held fields timestamp = do
      now <- oeNow
      withTransaction oeStore (`getRegistration` rid) >>= \case
        Nothing -> pure $ answerNow notFound
        Just stored
          | nrLabelHash stored `notElem` held -> changed
          | otherwise -> moveTo now (registrationAt now stored) fields timestamp
    moveTo now r@NameRegistration {nrPhase} fields timestamp
      | orderFieldsOf r == fields = answerNow . jsonResponse status200 <$> registrationView oeStore oeConfig oeContracts now r
      | Left refusal <- checkTimestamp now timestamp = refuse refusal
      | not (movable r) = refuse ORBadRequest
      | nrPhase == PFailed =
          confirmLanded env r >>= \case
            LNotLanded -> moveChecked now r fields
            LRegistered v -> written env rid v <$> registrationChangedResponse env rid
            LChanged -> changed
            LUnread -> refuse ORNameCheckUnavailable
      | otherwise = moveChecked now r fields
    moveChecked now r@NameRegistration {nrMoves, nrYears, nrPrice, nrPayment} fields@OrderFields {ofLabelHash, ofLength, ofDuration}
      | nrMoves >= nMaxOrderMoves oeConfig = answerNow <$> registrationRefusalResponse env "move_limit" rid
      | otherwise =
          firstRefusal [checkMoveMeets oeStore r fields now, pure $ checkDuration oeConfig now nrYears ofDuration, checkNameFree oeContracts oeStore ofLabelHash] >>= \case
            Just refusal -> refuse refusal
            Nothing -> case nrPayment of
              RPInvoice {} -> checkMovePrice oeContracts r ofLength >>= either refuse (write now r fields)
              RPBalance -> checkMovePrice oeContracts r ofLength >>= either refuse (write now r fields)
              RPCode _ -> write now r fields nrPrice
    write now r fields price =
      retargetOrder env r fields price >>= \case
        Right v -> movedAnswer now v
        Left WRChanged -> changed
        Left (WRRefused refusal) -> refuse refusal
    movedAnswer now v =
      withTransaction oeStore (`getRegistration` rid) >>= \case
        Just r -> written env rid v . jsonResponse status200 <$> registrationView oeStore oeConfig oeContracts now (registrationAt now r)
        Nothing -> answerNow internalError <$ logError ("POST /api/registration/:id/name: order " <> ridText <> " is not readable after its move")
    changed = answerNow <$> registrationChangedResponse env rid
    refuse = refused "POST /api/registration/:id/name"

-- | Published after the turn, as the answer is sent.
written :: OrdersEnv -> RegistrationId -> Int64 -> Response -> Answer
written OrdersEnv {oePhases} rid v aResponse = Answer {aResponse, aAfter = atomically $ publish oePhases rid v}

refused :: Text -> OrderRefusal -> IO Answer
refused what refusal = answerNow (orderRefusalResponse refusal) <$ logInfo (what <> " refused: " <> tshow refusal)
