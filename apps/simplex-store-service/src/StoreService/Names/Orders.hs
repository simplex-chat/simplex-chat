{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The checks every order runs, by paid invoice or prepaid code, in the contract's order.
module StoreService.Names.Orders
  ( OrdersEnv (..),
    ordersEnv,
    OrderRequest (..),
    OrderRefusal (..),
    ExistingOrder (..),
    ClosedOrder (..),
    WriteRefusal (..),
    Landing (..),
    maxSealedChars,
    signedRequestSeconds,
    parseOrderRequest,
    parseOrderFields,
    textField,
    integerField,
    checkLength,
    checkSigner,
    checkOrderSigner,
    existingOrder,
    placeOrder,
    placeCodeOrder,
    codeOrderInsert,
    checkTimestamp,
    checkDuration,
    checkNameFree,
    checkPrice,
    movable,
    issuable,
    orderFieldsOf,
    checkMoveMeets,
    checkMovePrice,
    retargetOrder,
    confirmLanded,
    issuedCodeHash,
    checkTakenCodeLength,
    issueTakenCode,
  )
where

import Control.Applicative ((<|>))
import qualified Control.Exception as E
import Control.Monad (forM, unless, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Simplex.Chat.Badges.Code (maxNameLength, minNameLength)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Eth.Address (Address, recoverAddress)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (tshow)
import StoreService.Config (NamesConfig (..))
import StoreService.Log (logError, logWarn)
import StoreService.Names.Codes (CodeRefusal (..), codeRefusal, reserveCode, reserveRefusal, spendLandedCode)
import StoreService.Names.Contracts (Contracts, NameStatus (..), chainPriceCents, codeLengthFor, nameExpires, nameStatus, ownerOfToken, secondsPerYear)
import StoreService.Names.Env (NamesEnv (..))
import StoreService.Names.Phases (Phases, RegistrationId (..))
import StoreService.Names.Abi (bytes32FromHex, hex)
import StoreService.Names.Signatures (OrderFields (..), orderMessage, parseChecksumAddress, parseSignature, personalDigest)
import StoreService.Names.Store (CodeHashTaken, LiveOrder (..), NamePhase (..), NameRegistration (..), NewPrepaidCode (..), NewRegistration (..), PrepaidCode (..), RegistrationPayment (..), RegistrationRefused (..), NameTransaction (..), TxFor (..), TxStatus (..), VersionWrite (..), closeOverdue, createCodeRegistration, getLiveOrders, getPrepaidCode, getPrepaidCodeById, getRegistrationByOrderHash, insertPrepaidCode, isLabelRegistering, registrationTransactions, retargetRegistration, setIssuedCode, updateRegistration)
import StoreService.Names.Turns (Turns, withCodeTurn)

data OrdersEnv = OrdersEnv
  { oeStore :: DBStore,
    oeConfig :: NamesConfig,
    oeContracts :: Contracts,
    oeTurns :: Turns,
    oePhases :: Phases,
    oeNow :: IO UTCTime
  }

ordersEnv :: NamesEnv -> OrdersEnv
ordersEnv NamesEnv {neStore, neConfig, neContracts, neTurns, nePhases} =
  OrdersEnv {oeStore = neStore, oeConfig = neConfig, oeContracts = neContracts, oeTurns = neTurns, oePhases = nePhases, oeNow = getCurrentTime}

-- | The fields both kinds of order carry, with the owner's signature over them.
data OrderRequest = OrderRequest
  { orOwner :: Address,
    orFields :: OrderFields,
    orTimestamp :: Word64,
    orSignature :: RecoverableSignature
  }

data OrderRefusal
  = ORBadRequest
  | ORInvalidName
  | OROrderExists
  | ORNameTaken
  | ORNameCheckUnavailable
  | ORCatalogChanged Int
  | ORCode CodeRefusal
  | ORPriceTooHigh Int
  | ORCodeLengthChanged Int
  | ORCodeUnavailable
  | ORCodeConflict
  | ORProviderUnavailable
  deriving (Eq, Show)

newtype OrderRefused = OrderRefused OrderRefusal
  deriving (Show)

instance E.Exception OrderRefused

-- | An exact resend of a stored order, live or not; or the overdue orders of the same wallet and name the new order closes.
data ExistingOrder = EOResent NameRegistration | EONew [NameRegistration] | EOMeets

-- | An overdue order a new order closed, at its new version; its invoice is now expired.
data ClosedOrder = ClosedOrder {coRegistration :: NameRegistration, coVersion :: Int64}

-- | Why an owner's write changed nothing: the registration changed since it was read, or a refusal.
data WriteRefusal = WRChanged | WRRefused OrderRefusal
  deriving (Eq, Show)

-- | What /retry's first check found on the order's current name: no register through, the order registered at a new version,
-- the order changed since it was read, or a failed chain read.
data Landing = LNotLanded | LRegistered Int64 | LChanged | LUnread
  deriving (Eq, Show)

newtype TakenCodeConflict = TakenCodeConflict CodeHashTaken
  deriving (Show)

instance E.Exception TakenCodeConflict

maxSealedChars :: Int
maxSealedChars = 11095

-- | How far a signed request's timestamp may be from the service's clock.
signedRequestSeconds :: Integer
signedRequestSeconds = 600

-- | Every format check; the length's range is checkLength's, after the formats a kind of order adds.
parseOrderRequest :: J.Object -> Maybe OrderRequest
parseOrderRequest o = do
  orOwner <- either (const Nothing) Just . parseChecksumAddress =<< textField o "owner"
  (orFields, orTimestamp, orSignature) <- parseOrderFields o
  pure OrderRequest {orOwner, orFields, orTimestamp, orSignature}

-- | The signed fields of an order, or of another name for one, with their timestamp and signature.
parseOrderFields :: J.Object -> Maybe (OrderFields, Word64, RecoverableSignature)
parseOrderFields o = do
  ofLabelHash <- LabelHash <$> (bytes32FromHex =<< textField o "labelHash")
  ofLength <- integerField =<< KM.lookup "length" o
  ofDuration <- integerField =<< KM.lookup "duration" o
  ofCommitment <- bytes32FromHex =<< textField o "commitment"
  ofSealed <- textField o "sealed"
  unless (T.length ofSealed <= maxSealedChars) Nothing
  timestamp <- integerField =<< KM.lookup "timestamp" o
  signature <- either (const Nothing) Just . parseSignature =<< textField o "signature"
  pure (OrderFields {ofLabelHash, ofLength, ofDuration, ofCommitment, ofSealed}, timestamp, signature)

textField :: J.Object -> J.Key -> Maybe Text
textField o k = case KM.lookup k o of
  Just (J.String t) -> Just t
  _ -> Nothing

-- | A JSON integer within the type's range; a fraction, an exponent past it, or any other value is Nothing.
integerField :: (Integral a, Bounded a) => J.Value -> Maybe a
integerField = \case
  J.Number n -> toBoundedInteger n
  _ -> Nothing

checkLength :: OrderFields -> Either OrderRefusal ()
checkLength OrderFields {ofLength} =
  unless (ofLength >= minNameLength && ofLength <= maxNameLength) $ Left ORInvalidName

-- | The owner's EIP-191 signature over the message.
checkSigner :: Address -> ByteString -> RecoverableSignature -> IO (Either OrderRefusal ())
checkSigner owner msg signature =
  recoverAddress signature (personalDigest msg) >>= \case
    Right signer | signer == owner -> pure $ Right ()
    _ -> pure $ Left ORBadRequest

checkOrderSigner :: OrderRequest -> IO (Either OrderRefusal ())
checkOrderSigner OrderRequest {orOwner, orFields, orTimestamp, orSignature} = checkSigner orOwner (orderMessage orFields orTimestamp) orSignature

existingOrder :: DB.Connection -> OrderRequest -> ByteString -> UTCTime -> IO ExistingOrder
existingOrder db req hash now =
  getRegistrationByOrderHash db hash >>= \case
    Just r -> pure $ EOResent r
    Nothing -> maybe EOMeets EONew <$> overdueOrdersMet db req now

-- | The live orders of the request's commitment, or of its owner and label hash, when all are overdue orders of the same wallet and name;
-- Nothing when any other live order holds them.
overdueOrdersMet :: DB.Connection -> OrderRequest -> UTCTime -> IO (Maybe [NameRegistration])
overdueOrdersMet db OrderRequest {orOwner, orFields = OrderFields {ofLabelHash, ofCommitment}} now = do
  orders <- getLiveOrders db ofCommitment orOwner ofLabelHash now
  pure $ if all closable orders then Just (map loRegistration orders) else Nothing
  where
    closable LiveOrder {loRegistration = NameRegistration {nrOwner, nrLabelHash}, loOverdue} =
      loOverdue && nrOwner == orOwner && nrLabelHash == ofLabelHash

-- | In one transaction, the overdue orders the request meets are closed, then the order is inserted.
-- Refused, changing nothing, when it meets an order it cannot close, or the insert refuses.
placeOrder :: OrdersEnv -> OrderRequest -> (DB.Connection -> UTCTime -> IO (Either OrderRefusal ())) -> IO (Either OrderRefusal [ClosedOrder])
placeOrder OrdersEnv {oeStore, oeNow} req insert = do
  now <- oeNow
  first (\(OrderRefused refusal) -> refusal) <$> E.try (withTransaction oeStore $ \db -> transaction db now)
  where
    transaction db now =
      overdueOrdersMet db req now >>= \case
        Just overdue -> do
          closed <- mapM (close db now) overdue
          insert db now >>= either (E.throwIO . OrderRefused) (const $ pure closed)
        Nothing -> E.throwIO $ OrderRefused OROrderExists
    close db now r =
      closeOverdue db r now >>= \case
        Just v -> pure ClosedOrder {coRegistration = r, coVersion = v}
        Nothing -> E.throwIO $ OrderRefused OROrderExists

-- | In the code's turn, the code as it reads there must pay for the name; the order's insert then reserves it.
placeCodeOrder :: OrdersEnv -> OrderRequest -> RegistrationId -> PrepaidCode -> IO (Either OrderRefusal [ClosedOrder])
placeCodeOrder env@OrdersEnv {oeStore, oeTurns, oeNow} req@OrderRequest {orOwner, orFields = fields@OrderFields {ofLength}} rid PrepaidCode {pcCodeHash, pcYears} =
  withCodeTurn oeTurns pcCodeHash $ do
    now <- oeNow
    withTransaction oeStore (`getPrepaidCode` pcCodeHash) >>= \case
      Nothing -> pure $ Left $ ORCode CRInvalid
      Just code
        | Just refusal <- codeRefusal now rid ofLength code -> pure $ Left $ ORCode refusal
        | otherwise -> placeOrder env req $ codeOrderInsert NewRegistration {nrgId = rid, nrgOwner = orOwner, nrgOrder = fields, nrgYears = pcYears} code

-- | The code was checked in its turn, so only a writer outside this process can make its reservation fail.
codeOrderInsert :: NewRegistration -> PrepaidCode -> DB.Connection -> UTCTime -> IO (Either OrderRefusal ())
codeOrderInsert new@NewRegistration {nrgId, nrgOrder = OrderFields {ofLength}} code@PrepaidCode {pcCodeHash} db now =
  createCodeRegistration db new code now >>= \case
    Right () -> pure $ Right ()
    Left RRLiveOrder -> pure $ Left OROrderExists
    Left RRCodeNotReservable -> Left . ORCode <$> reserveRefusal db now pcCodeHash nrgId ofLength

checkTimestamp :: UTCTime -> Word64 -> Either OrderRefusal ()
checkTimestamp now timestamp =
  unless (abs (toInteger timestamp - unixSeconds now) <= signedRequestSeconds) $ Left ORBadRequest

-- | The paid years plus the free days until the term starts, by the service's clock, within the slack.
checkDuration :: NamesConfig -> UTCTime -> Int -> Word64 -> Either OrderRefusal ()
checkDuration NamesConfig {nTermStart, nTermSlackSeconds} now years duration = do
  let free = toInteger duration - toInteger years * secondsPerYear
      expected = max 0 (unixSeconds nTermStart - unixSeconds now)
  when (free < 0 || abs (free - expected) > toInteger nTermSlackSeconds) $ Left ORBadRequest

-- | A name being registered here searches as available, so it is checked here too.
checkNameFree :: Contracts -> DBStore -> LabelHash -> IO (Either OrderRefusal ())
checkNameFree ct st lh =
  nameStatus ct lh >>= \case
    Left e -> Left ORNameCheckUnavailable <$ logWarn ("names: an order's name check failed: " <> tshow e)
    Right NSAvailable -> do
      registering <- withTransaction st (`isLabelRegistering` lh)
      pure $ if registering then Left ORNameTaken else Right ()
    Right _ -> pure $ Left ORNameTaken

-- | The chain price for the length and years, which must be the price the order saw.
checkPrice :: Contracts -> Int -> Int -> Int -> IO (Either OrderRefusal ())
checkPrice ct len years price =
  chainPriceCents ct len years >>= \case
    Left e -> Left ORNameCheckUnavailable <$ logWarn ("names: an order's price read failed: " <> tshow e)
    Right chainPrice
      | chainPrice /= price -> pure $ Left $ ORCatalogChanged chainPrice
      | otherwise -> pure $ Right ()

unixSeconds :: UTCTime -> Integer
unixSeconds = floor . utcTimeToPOSIXSeconds

-- | A paid order that lost its name, any failed order, and one whose reveal was refused for its links can move to another name.
movable :: NameRegistration -> Bool
movable NameRegistration {nrPhase, nrPayment, nrLinksRefusedAt} = case nrPhase of
  PTaken | RPInvoice {} <- nrPayment -> True
  PFailed -> True
  PCommitted -> isJust nrLinksRefusedAt
  _ -> False

-- | Only a paid order that lost its name turns into a code.
issuable :: NameRegistration -> Bool
issuable NameRegistration {nrPhase, nrPayment} = case (nrPhase, nrPayment) of
  (PTaken, RPInvoice {}) -> True
  _ -> False

orderFieldsOf :: NameRegistration -> OrderFields
orderFieldsOf NameRegistration {nrLabelHash, nrLength, nrDuration, nrCommitment, nrSealed} =
  OrderFields {ofLabelHash = nrLabelHash, ofLength = nrLength, ofDuration = nrDuration, ofCommitment = nrCommitment, ofSealed = nrSealed}

-- | Any other live order of the new commitment, or of the owner and the new label hash, refuses the move.
checkMoveMeets :: DBStore -> NameRegistration -> OrderFields -> UTCTime -> IO (Either OrderRefusal ())
checkMoveMeets st NameRegistration {nrId = rid, nrOwner} OrderFields {ofLabelHash, ofCommitment} now = do
  orders <- withTransaction st $ \db -> getLiveOrders db ofCommitment nrOwner ofLabelHash now
  pure $ if any ((/= rid) . nrId . loRegistration) orders then Left OROrderExists else Right ()

-- | The new name's chain price for the order's years, at most what the order is worth, becomes its price.
checkMovePrice :: Contracts -> NameRegistration -> Int -> IO (Either OrderRefusal Int)
checkMovePrice ct NameRegistration {nrPrice, nrYears} len =
  chainPriceCents ct len nrYears >>= \case
    Left e -> Left ORNameCheckUnavailable <$ logWarn ("names: a move's price read failed: " <> tshow e)
    Right price
      | price > nrPrice -> pure $ Left $ ORPriceTooHigh price
      | otherwise -> pure $ Right price

-- | The move's write, a compare-and-set on the version read. A code order's code is checked and reserved again in the code's turn.
retargetOrder :: OrdersEnv -> NameRegistration -> OrderFields -> Int -> IO (Either WriteRefusal Int64)
retargetOrder OrdersEnv {oeStore, oeTurns, oeNow} r@NameRegistration {nrId, nrPayment} fields@OrderFields {ofLength} price = do
  now <- oeNow
  let write db = versionWritten <$> retargetRegistration db r fields price now
  case nrPayment of
    RPInvoice {} -> withTransaction oeStore write
    RPCode codeId ->
      withTransaction oeStore (`getPrepaidCodeById` codeId) >>= \case
        Nothing -> pure $ Left $ WRRefused $ ORCode CRInvalid
        Just PrepaidCode {pcCodeHash} -> either (Left . WRRefused . ORCode) id <$> reserveCode oeTurns oeStore now pcCodeHash nrId ofLength write

versionWritten :: VersionWrite -> Either WriteRefusal Int64
versionWritten = \case
  Written v -> Right v
  VersionChanged -> Left WRChanged
  LiveOrderExists -> Left $ WRRefused OROrderExists
  NotLive -> Left $ WRRefused ORBadRequest

-- | /retry's first check, on the order's current name: a register mined with status 1, or the owner holding the name, went through
-- with only its confirmation lost. The order then becomes registered by a compare-and-set on the version read, a code order spending its code.
-- The caller holds the turn of the order's label hash and passes the order as registrationAt reads it.
confirmLanded :: OrdersEnv -> NameRegistration -> IO Landing
confirmLanded OrdersEnv {oeStore, oeContracts, oeTurns, oeNow} r@NameRegistration {nrId, nrOwner, nrLabelHash, nrPayment, nrRegisterTx, nrRegisterBlock} = do
  txs <- withTransaction oeStore (`registrationTransactions` nrId)
  let mined = [(ntHash, ntMinedBlock) | NameTransaction {ntFor = TxRegister _, ntStatus = TSMined, ntHash, ntMinedBlock} <- txs]
  runExceptT (landed mined) >>= \case
    Left e -> LUnread <$ logWarn ("names: a register check's chain read failed: " <> tshow e)
    Right Nothing -> pure LNotLanded
    Right (Just (tx, expiry)) -> either (const LChanged) LRegistered <$> markRegistered tx expiry
  where
    landed mined = do
      registerTx <- case mined of
        tx : _ -> pure $ Just $ Just tx
        [] -> (\holder -> if holder == Just nrOwner then Just Nothing else Nothing) <$> ExceptT (ownerOfToken oeContracts nrLabelHash)
      forM registerTx $ \tx -> (tx,) <$> ExceptT (nameExpires oeContracts nrLabelHash)
    markRegistered tx expiry = do
      now <- oeNow
      let registered =
            r
              { nrPhase = PRegistered,
                nrRegisterTx = (fst <$> tx) <|> nrRegisterTx,
                nrRegisterBlock = (snd =<< tx) <|> nrRegisterBlock,
                nrExpiresAt = Just expiry,
                nrRegisteredAt = Just now,
                nrFailedAt = Nothing,
                nrError = Nothing
              }
          write db = versionWritten <$> updateRegistration db registered now
      case nrPayment of
        RPInvoice {} -> withTransaction oeStore write
        RPCode codeId -> spendLandedCode oeTurns oeStore now codeId nrId write >>= traverse (paidWith codeId)
    paidWith codeId (v, spent) = do
      unless spent $ do
        code <- withTransaction oeStore (`getPrepaidCodeById` codeId)
        logError $
          "names: registration " <> ridText nrId <> " turned out registered with prepaid code " <> maybe (tshow codeId) (hex . pcCodeHash) code
            <> ", which was released and then spent, revoked or expired: the name was paid with it"
      pure v
    ridText (RegistrationId t) = t

-- | The hash of the code a taken order was turned into.
issuedCodeHash :: DBStore -> NameRegistration -> IO (Maybe ByteString)
issuedCodeHash st NameRegistration {nrIssuedCodeId} = case nrIssuedCodeId of
  Just codeId -> fmap pcCodeHash <$> withTransaction st (`getPrepaidCodeById` codeId)
  Nothing -> pure Nothing

-- | The code's minLength for the order's price and years must be the length the page derived the code with.
checkTakenCodeLength :: Contracts -> NameRegistration -> Int -> IO (Either OrderRefusal ())
checkTakenCodeLength ct NameRegistration {nrPrice, nrYears} sent =
  codeLengthFor ct nrPrice nrYears >>= \case
    Left e -> Left ORProviderUnavailable <$ logWarn ("names: a taken code's length read failed: " <> tshow e)
    Right Nothing -> pure $ Left ORCodeUnavailable
    Right (Just len)
      | len /= sent -> pure $ Left $ ORCodeLengthChanged len
      | otherwise -> pure $ Right ()

-- | The order becomes code_issued by a compare-and-set on the version read, then its code is inserted and linked, in one
-- transaction, so the registration's row is locked before the code's. A hash another code has rolls the order's write back.
issueTakenCode :: OrdersEnv -> NameRegistration -> ByteString -> Int -> IO (Either WriteRefusal Int64)
issueTakenCode OrdersEnv {oeStore, oeNow} r@NameRegistration {nrId, nrYears} codeHash minLength = do
  now <- oeNow
  let code = NewPrepaidCode {npcCodeHash = codeHash, npcMinLength = minLength, npcYears = nrYears, npcPaymentStatus = CPSPaid, npcExpiresAt = Nothing}
      issue db =
        updateRegistration db r {nrPhase = PCodeIssued} now >>= \case
          Written v ->
            insertPrepaidCode db code now >>= \case
              Right codeId -> Right v <$ setIssuedCode db nrId codeId
              Left taken -> E.throwIO $ TakenCodeConflict taken
          -- a code_issued order is not live, so only a changed version refuses the write
          _ -> pure $ Left WRChanged
  either (\(TakenCodeConflict _) -> Left $ WRRefused ORCodeConflict) id <$> E.try (withTransaction oeStore issue)
