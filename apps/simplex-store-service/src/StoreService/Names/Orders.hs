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
    orderYears,
    checkTerms,
    checkNameFree,
    checkPrice,
    movable,
    orderFieldsOf,
    checkMoveMeets,
    checkMovePrice,
    retargetOrder,
    versionWritten,
    confirmLanded,
  )
where

import qualified Control.Exception as E
import Control.Monad (forM, unless, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Maybe (catMaybes, isJust)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Simplex.Chat.Badges.Code (maxNameLength, minNameLength)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Eth.Address (Address, recoverAddress)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (tshow)
import StoreService.Config (NamesConfig (..))
import StoreService.Log (logError, logWarn)
import StoreService.Names.Codes (CodeRefusal (..), codeRefusal, reserveCode, reserveRefusal, spendLandedCode)
import StoreService.Names.Community (agreedTermsHash, communityJSON, nameTerms, readableTerms, termsFor, termsJSON, yearsFor)
import StoreService.Names.Contracts (Contracts, NameStatus (..), chainPriceCents, confirmedTag, nameExpires, nameStatus, ownerOfToken, secondsPerYear)
import StoreService.Names.Env (NamesEnv (..))
import StoreService.Names.Phases (Phases, RegistrationId (..))
import StoreService.Names.Abi (bytes32FromHex, hex)
import StoreService.Names.Signatures (OrderFields (..), orderMessage, parseChecksumAddress, parseSignature, personalDigest)
import StoreService.Names.Store (Balance, LiveOrder (..), NamePhase (..), NameRegistration (..), NewRegistration (..), PrepaidCode (..), RegistrationPayment (..), RegistrationRefused (..), NameTransaction (..), TxFor (..), TxStatus (..), VersionWrite (..), closeOverdue, createCodeRegistration, creditBalance, getBalance, getLiveOrders, getPrepaidCode, getPrepaidCodeById, getRegistration, getRegistrationByOrderHash, isLabelRegistering, registrationTransactions, retargetRegistration, updateRegistration)
import StoreService.Names.Turns (Turns, withBalanceTurn, withCodeTurn)

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
  | ORBalanceChanged (Maybe Balance)
  | -- | the current terms and the name's community entry, as the answer carries them
    ORTermsChanged J.Value J.Value
  | ORTermsUnavailable (Maybe Int)
  | ORCodesOnly
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

maxSealedChars :: Int
maxSealedChars = 11095

-- | The terms version pages signed before, of up to 32 characters, and for a community name /community/ and the 64 hex digits
-- of its terms' hash, with a margin; the terms' hash signed now is shorter.
maxAgreedTermsChars :: Int
maxAgreedTermsChars = 112

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
  ofAgreedTerms <- textField o "terms"
  unless (agreedTermsFormat ofAgreedTerms) Nothing
  timestamp <- integerField =<< KM.lookup "timestamp" o
  signature <- either (const Nothing) Just . parseSignature =<< textField o "signature"
  pure (OrderFields {ofLabelHash, ofLength, ofDuration, ofCommitment, ofSealed, ofAgreedTerms}, timestamp, signature)

agreedTermsFormat :: Text -> Bool
agreedTermsFormat t = not (T.null t) && T.length t <= maxAgreedTermsChars && T.all termsChar t
  where
    termsChar c = isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("._/-" :: String)

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

-- | In one transaction, the overdue orders the request meets and the balance holders the caller names are closed, then the order is inserted.
-- A named holder no longer live is skipped, and one still live that cannot be closed refuses with the balance it holds.
-- Refused, changing nothing, when it meets an order it cannot close, or the insert refuses.
placeOrder :: OrdersEnv -> OrderRequest -> [NameRegistration] -> (DB.Connection -> UTCTime -> IO (Either OrderRefusal ())) -> IO (Either OrderRefusal [ClosedOrder])
placeOrder OrdersEnv {oeStore, oeNow} req holders insert = do
  now <- oeNow
  first (\(OrderRefused refusal) -> refusal) <$> E.try (withTransaction oeStore $ \db -> transaction db now)
  where
    transaction db now =
      overdueOrdersMet db req now >>= \case
        Just overdue -> do
          closed <- mapM (close db now) overdue
          closedHolders <- catMaybes <$> mapM (closeHolder db now) holders
          insert db now >>= either (E.throwIO . OrderRefused) (const $ pure $ closed <> closedHolders)
        Nothing -> E.throwIO $ OrderRefused OROrderExists
    close db now r =
      closeOverdue db r now >>= \case
        Just v -> pure ClosedOrder {coRegistration = r, coVersion = v}
        Nothing -> E.throwIO $ OrderRefused OROrderExists
    -- read again here, since another order of its own name may have closed it meanwhile
    closeHolder db now NameRegistration {nrId} =
      getRegistration db nrId >>= \case
        Just holder@NameRegistration {nrLive = True, nrBalanceSource} ->
          closeOverdue db holder now >>= \case
            Just v -> pure $ Just ClosedOrder {coRegistration = holder, coVersion = v}
            Nothing -> E.throwIO . OrderRefused . ORBalanceChanged =<< maybe (pure Nothing) (\source -> getBalance db source now) nrBalanceSource
        _ -> pure Nothing

-- | In the code's turn, the code as it reads there must pay for the name; the order's insert then reserves it.
placeCodeOrder :: OrdersEnv -> OrderRequest -> RegistrationId -> PrepaidCode -> IO (Either OrderRefusal [ClosedOrder])
placeCodeOrder env@OrdersEnv {oeStore, oeTurns, oeNow} req@OrderRequest {orOwner, orFields = fields@OrderFields {ofLength}} rid PrepaidCode {pcCodeHash, pcYears} =
  withCodeTurn oeTurns pcCodeHash $ do
    now <- oeNow
    withTransaction oeStore (`getPrepaidCode` pcCodeHash) >>= \case
      Nothing -> pure $ Left $ ORCode CRInvalid
      Just code
        | Just refusal <- codeRefusal now rid ofLength code -> pure $ Left $ ORCode refusal
        | otherwise -> placeOrder env req [] $ codeOrderInsert NewRegistration {nrgId = rid, nrgOwner = orOwner, nrgOrder = fields, nrgYears = pcYears} code

-- | The code was checked in its turn, so only a writer outside this process can make its reservation fail.
codeOrderInsert :: NewRegistration -> PrepaidCode -> DB.Connection -> UTCTime -> IO (Either OrderRefusal ())
codeOrderInsert new@NewRegistration {nrgId, nrgOrder = OrderFields {ofLength}} code@PrepaidCode {pcCodeHash} db now =
  createCodeRegistration db new code now >>= \case
    Right () -> pure $ Right ()
    Left RRLiveOrder -> pure $ Left OROrderExists
    Left RRCodeNotReservable -> Left . ORCode <$> reserveRefusal db now pcCodeHash nrgId ofLength
    -- a code order holds no balance
    Left RRBalanceChanged -> pure $ Left OROrderExists

checkTimestamp :: UTCTime -> Word64 -> Either OrderRefusal ()
checkTimestamp now timestamp =
  unless (abs (toInteger timestamp - unixSeconds now) <= signedRequestSeconds) $ Left ORBadRequest

-- | The paid years plus the free days until the term starts, by the service's clock, within the slack.
checkDuration :: NamesConfig -> UTCTime -> Int -> Word64 -> Either OrderRefusal ()
checkDuration NamesConfig {nTermStart, nTermSlackSeconds} now years duration = do
  let free = toInteger duration - toInteger years * secondsPerYear
      expected = max 0 (unixSeconds nTermStart - unixSeconds now)
  when (free < 0 || abs (free - expected) > toInteger nTermSlackSeconds) $ Left ORBadRequest

-- | The years the payment covers, or communityYears for an order signing community terms; 'checkTerms' refuses those terms
-- for any other name.
orderYears :: OrderFields -> Int -> Int
orderYears OrderFields {ofAgreedTerms} = yearsFor (isJust $ agreedTermsHash ofAgreedTerms)

-- | The terms the order signed must be the current ones for its name, the community terms for a name the policy lists,
-- which cannot be ordered while a buyer cannot read them.
checkTerms :: NamesConfig -> Contracts -> OrderFields -> IO (Either OrderRefusal ())
checkTerms cfg ct OrderFields {ofLabelHash, ofAgreedTerms} =
  nameTerms ct ofLabelHash >>= \case
    Left e -> Left ORNameCheckUnavailable <$ logWarn ("names: an order's terms check failed: " <> tshow e)
    Right (listed, terms) ->
      readableTerms ct terms <&> \case
        Left wait -> Left $ ORTermsUnavailable wait
        Right t -> unless (ofAgreedTerms == termsFor listed t) $ Left $ ORTermsChanged (termsJSON $ Just t) (communityJSON cfg ofLabelHash listed)

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

-- | A failed order, and one whose reveal was refused for its links, can move to another name.
movable :: NameRegistration -> Bool
movable NameRegistration {nrPhase, nrLinksRefusedAt} = case nrPhase of
  PFailed -> True
  PCommitted -> isJust nrLinksRefusedAt
  _ -> False

orderFieldsOf :: NameRegistration -> OrderFields
orderFieldsOf NameRegistration {nrLabelHash, nrLength, nrDuration, nrCommitment, nrSealed, nrAgreedTerms} =
  OrderFields {ofLabelHash = nrLabelHash, ofLength = nrLength, ofDuration = nrDuration, ofCommitment = nrCommitment, ofSealed = nrSealed, ofAgreedTerms = nrAgreedTerms}

-- | Any other live order of the new commitment, or of the owner and the new label hash, refuses the move.
checkMoveMeets :: DBStore -> NameRegistration -> OrderFields -> UTCTime -> IO (Either OrderRefusal ())
checkMoveMeets st NameRegistration {nrId = rid, nrOwner} OrderFields {ofLabelHash, ofCommitment} now = do
  orders <- withTransaction st $ \db -> getLiveOrders db ofCommitment nrOwner ofLabelHash now
  pure $ if any ((/= rid) . nrId . loRegistration) orders then Left OROrderExists else Right ()

-- | The new name's chain price for its years, at most what the order is worth, becomes its price.
checkMovePrice :: Contracts -> NameRegistration -> Int -> Int -> IO (Either OrderRefusal Int)
checkMovePrice ct NameRegistration {nrPrice} len years =
  chainPriceCents ct len years >>= \case
    Left e -> Left ORNameCheckUnavailable <$ logWarn ("names: a move's price read failed: " <> tshow e)
    Right price
      | price > nrPrice -> pure $ Left $ ORPriceTooHigh price
      | otherwise -> pure $ Right price

-- | The move's write, a compare-and-set on the version read. A code order's code is checked and reserved again in the code's turn.
-- A paid order moved to a cheaper name keeps the difference as its balance, credited in the move's transaction and the balance's turn.
retargetOrder :: OrdersEnv -> NameRegistration -> OrderFields -> Int -> IO (Either WriteRefusal Int64)
retargetOrder OrdersEnv {oeStore, oeTurns, oeNow} r@NameRegistration {nrId, nrPayment, nrPrice} fields@OrderFields {ofLength} price = do
  now <- oeNow
  let write db = versionWritten <$> retargetRegistration db r fields price now
      paidMove
        | price < nrPrice = withBalanceTurn oeTurns nrId $ withTransaction oeStore $ \db -> write db >>= traverse (\v -> v <$ creditBalance db nrId (nrPrice - price) now)
        | otherwise = withTransaction oeStore write
  case nrPayment of
    RPInvoice {} -> paidMove
    RPBalance -> paidMove
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

-- | /retry's first check, on the order's current name: a register mined with status 1, or the owner holding the name at the confirmation depth,
-- went through with only its confirmation lost. The order then becomes registered by a compare-and-set on the version read, a code order spending its code.
-- The caller holds the turn of the order's label hash and passes the order as registrationAt reads it.
confirmLanded :: OrdersEnv -> NameRegistration -> IO Landing
confirmLanded OrdersEnv {oeStore, oeContracts, oeTurns, oeNow} r@NameRegistration {nrId, nrOwner, nrLabelHash, nrPayment} = do
  txs <- withTransaction oeStore (`registrationTransactions` nrId)
  let mined = [(ntHash, ntMinedBlock) | NameTransaction {ntFor = TxRegister _, ntStatus = TSMined, ntHash, ntMinedBlock} <- txs]
  runExceptT (landed mined) >>= \case
    Left e -> LUnread <$ logWarn ("names: a register check's chain read failed: " <> tshow e)
    Right Nothing -> pure LNotLanded
    Right (Just (tx, expiry)) -> either (const LChanged) LRegistered <$> markRegistered tx expiry
  where
    landed mined = do
      confirmed <- ExceptT $ confirmedTag oeContracts
      registerTx <- case mined of
        tx : _ -> pure $ Just $ Just tx
        [] -> (\holder -> if holder == Just nrOwner then Just Nothing else Nothing) <$> ExceptT (ownerOfToken oeContracts confirmed nrLabelHash)
      forM registerTx $ \tx -> (tx,) <$> ExceptT (nameExpires oeContracts confirmed nrLabelHash)
    markRegistered tx expiry = do
      now <- oeNow
      let registered =
            r
              { nrPhase = PRegistered,
                -- judged by ownerOf alone, no hash is known to have mined: none is shown (contract section 6)
                nrRegisterTx = fst <$> tx,
                nrRegisterBlock = snd =<< tx,
                nrExpiresAt = Just expiry,
                nrRegisteredAt = Just now,
                nrFailedAt = Nothing,
                nrError = Nothing
              }
          write db = versionWritten <$> updateRegistration db registered now
      case nrPayment of
        RPInvoice {} -> withTransaction oeStore write
        RPBalance -> withTransaction oeStore write
        RPCode codeId -> spendLandedCode oeTurns oeStore now codeId nrId write >>= traverse (paidWith codeId)
    paidWith codeId (v, spent) = do
      unless spent $ do
        code <- withTransaction oeStore (`getPrepaidCodeById` codeId)
        logError $
          "names: registration " <> ridText nrId <> " turned out registered with prepaid code " <> maybe (tshow codeId) (hex . pcCodeHash) code
            <> ", which was released and then spent, revoked or expired: the name was paid with it"
      pure v
    ridText (RegistrationId t) = t
