{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
    SignedOrderFields,
    SentFields (..),
    SignedFields (..),
    SentOrder (..),
    orderRequest,
    signedFields,
    parseBody,
    textWith,
    checkSigner,
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
    noteRead,
    orderFieldsOf,
    checkMoveMeets,
    retargetOrder,
    versionWritten,
    confirmLanded,
    checkLanded,
  )
where

import qualified Control.Exception as E
import Control.Logger.Simple (logError, logWarn)
import Control.Monad (forM, unless, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Word (Word64)
import Simplex.Chat.Badges.Code (NameLength, nameLength)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Eth.Address (Address, recoverAddress)
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.SystemTime (RoundedSystemTime (..), roundedToUTCTime)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Codes (CodeRefusal (..), codeRefusal, reserveCode, reserveRefusal, spendLandedCode)
import StoreService.Names.Community (Community, communityOf, nameTerms, readableTerms, termsFor, yearsFor)
import StoreService.Names.Contracts (Contracts, NameStatus (..), Terms, chainPriceCents, confirmedTag, nameExpires, nameStatus, ownerOfToken, secondsPerYear)
import StoreService.Names.Env (NamesEnv (..))
import StoreService.Names.Phases (Phases, RegistrationId (..))
import StoreService.Names.Chain (Hex32 (..), TxHash)
import StoreService.Names.Registration (Commitment, LinkKind)
import StoreService.Names.Signatures (AgreedTerms (..), OrderFields (..), parseChecksumAddress, personalDigest)
import StoreService.Names.Store (CodeInsertRefused (..), LiveOrder (..), NameRegistration (..), NameReveal, NewRegistration (..), OrderHash, OrderPayment (..), OrderPhase (..), PrepaidCode (..), TxHistory (..), NameTransaction (..), RegistrationTx (..), TxFor (..), TxSettlement (..), TxState (..), VersionWrite (..), closeOverdue, createCodeRegistration, getLiveOrders, getPrepaidCode, getPrepaidCodeById, getRegistrationByOrderHash, labelHolder, markRead, registrationTransactions, retargetRegistration, spendBalance, updateRegistration)
import StoreService.Names.Turns (Turns, withBalanceTurn, withCodeTurn)
import StoreService.Names.View (movableUntil)
import StoreService.Store.Invoices (currentSecond)

data OrdersEnv = OrdersEnv
  { oeStore :: DBStore,
    oeConfig :: NamesConfig,
    oeContracts :: Contracts,
    oeTurns :: Turns,
    oePhases :: Phases,
    oeNow :: IO UTCTime,
    oeReport :: Text -> IO ()
  }

ordersEnv :: NamesEnv -> OrdersEnv
ordersEnv NamesEnv {neStore, neConfig, neContracts, neTurns, nePhases, neReport} =
  OrdersEnv {oeStore = neStore, oeConfig = neConfig, oeContracts = neContracts, oeTurns = neTurns, oePhases = nePhases, oeNow = currentSecond, oeReport = neReport}

-- | The fields both kinds of order carry, with the owner's signature over them.
data OrderRequest = OrderRequest
  { orOwner :: Address,
    orFields :: OrderFields,
    orTimestamp :: Word64,
    orSignature :: RecoverableSignature
  }

data OrderRefusal
  = ORBadRequest Text
  | ORInvalidName
  | ORInvalidLink LinkKind
  | OROrderExists
  | ORNameTaken
  | ORNameCheckUnavailable
  | ORCatalogChanged USDCents
  | ORCode CodeRefusal
  | ORPriceTooHigh USDCents
  | -- | the current terms and the name's community entry
    ORTermsChanged Terms (Maybe Community)
  | ORTermsUnavailable (Maybe Int)
  | ORCodesOnly
  deriving (Eq, Show)

newtype OrderRefused = OrderRefused OrderRefusal
  deriving (Show)

instance E.Exception OrderRefused

-- | An exact resend of a stored order, live or not; a new order, which closes the overdue orders of the same wallet and name;
-- or one meeting another live order.
data ExistingOrder = EOResent NameRegistration | EONew | EOMeets

-- | An overdue order a new order closed, at its new version; its invoice is now expired.
data ClosedOrder = ClosedOrder {coRegistration :: NameRegistration, coVersion :: Int64}

-- | Why an owner's write changed nothing: the registration changed since it was read, or a refusal.
data WriteRefusal = WRChanged | WRRefused OrderRefusal
  deriving (Eq, Show)

-- | What the register check found on the order's current name: no register through, the order registered at a new version,
-- the order changed since it was read, or a failed chain read.
data Landing = LNotLanded | LRegistered Int64 | LChanged | LUnread
  deriving (Eq, Show)

data Landed = LandedByReceipt TxHash | LandedByOwner

maxSealedChars :: Int
maxSealedChars = 11095

-- | How far a signed request's timestamp may be from the service's clock.
signedRequestSeconds :: Integer
signedRequestSeconds = 600

-- | The signed fields of an order, or of another name for one, with their timestamp and signature.
type SignedOrderFields = (OrderFields, Word64, RecoverableSignature)

data SentFields = SentFields
  { sfLabelHash :: Hex32,
    sfLength :: Int,
    sfDuration :: Word64,
    sfCommitment :: Commitment,
    sfSealed :: Text,
    sfTerms :: AgreedTerms
  }

data SignedFields = SignedFields
  { fields :: SentFields,
    timestamp :: Word64,
    signature :: RecoverableSignature
  }

data SentOrder = SentOrder {soOwner :: Address, soSigned :: SignedFields}

$(JQ.deriveFromJSON defaultJSON {J.fieldLabelModifier = dropPrefix "sf"} ''SentFields)

$(JQ.deriveFromJSON defaultJSON ''SignedFields)

instance J.FromJSON SentOrder where
  parseJSON = J.withObject "order" $ \o -> SentOrder <$> JT.explicitParseField (textWith $ first show . parseChecksumAddress) o "owner" <*> J.parseJSON (J.Object o)

orderRequest :: SentOrder -> Either OrderRefusal OrderRequest
orderRequest SentOrder {soOwner, soSigned} = (\(orFields, orTimestamp, orSignature) -> OrderRequest {orOwner = soOwner, orFields, orTimestamp, orSignature}) <$> signedFields soSigned

-- | Every format check; the length's range refuses after the formats a kind of order adds.
signedFields :: SignedFields -> Either OrderRefusal SignedOrderFields
signedFields SignedFields {fields = SentFields {sfLabelHash, sfLength, sfDuration, sfCommitment, sfSealed, sfTerms}, timestamp, signature}
  | T.length sfSealed > maxSealedChars = Left $ ORBadRequest "a malformed order: the sealed name is longer than its limit"
  | otherwise = case nameLength sfLength of
      Just ofLength -> Right (OrderFields {ofLabelHash = LabelHash $ unHex32 sfLabelHash, ofLength, ofDuration = sfDuration, ofCommitment = sfCommitment, ofSealed = sfSealed, ofAgreedTerms = sfTerms}, timestamp, signature)
      Nothing -> Left ORInvalidName

parseBody :: J.FromJSON a => Text -> J.Value -> Either OrderRefusal a
parseBody what = first (\e -> ORBadRequest $ "a malformed " <> what <> ": " <> T.pack e) . JT.parseEither J.parseJSON

textWith :: (Text -> Either String a) -> J.Value -> JT.Parser a
textWith f = J.withText "string" $ either fail pure . f

-- | The owner's EIP-191 signature over the message.
checkSigner :: Address -> ByteString -> RecoverableSignature -> IO (Either OrderRefusal ())
checkSigner owner msg signature =
  recoverAddress signature (personalDigest msg) >>= \case
    Right signer | signer == owner -> pure $ Right ()
    _ -> pure $ Left $ ORBadRequest "the signature is not the owner's"

existingOrder :: DB.Connection -> OrderRequest -> OrderHash -> UTCTime -> IO ExistingOrder
existingOrder db req hash now =
  getRegistrationByOrderHash db hash >>= \case
    Just r -> pure $ EOResent r
    Nothing -> maybe EOMeets (const EONew) <$> overdueOrdersMet db req now

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
placeOrder :: OrdersEnv -> UTCTime -> OrderRequest -> (DB.Connection -> UTCTime -> IO (Either OrderRefusal ())) -> IO (Either OrderRefusal [ClosedOrder])
placeOrder OrdersEnv {oeStore} now req insert =
  first (\(OrderRefused refusal) -> refusal) <$> E.try (withTransaction oeStore $ \db -> transaction db)
  where
    transaction db =
      overdueOrdersMet db req now >>= \case
        Just overdue -> do
          closed <- mapM (close db) overdue
          insert db now >>= either (E.throwIO . OrderRefused) (const $ pure closed)
        Nothing -> E.throwIO $ OrderRefused OROrderExists
    close db r =
      closeOverdue db r now >>= \case
        Just v -> pure ClosedOrder {coRegistration = r, coVersion = v}
        Nothing -> E.throwIO $ OrderRefused OROrderExists

-- | In the code's turn, the code as it reads there must pay for the name; the order's insert then reserves it.
placeCodeOrder :: OrdersEnv -> OrderRequest -> NameReveal -> OrderHash -> RegistrationId -> PrepaidCode -> IO (Either OrderRefusal [ClosedOrder])
placeCodeOrder env@OrdersEnv {oeStore, oeTurns, oeNow} req@OrderRequest {orOwner, orFields = fields@OrderFields {ofLength}} reveal hash rid PrepaidCode {pcCodeHash, pcYears} =
  withCodeTurn oeTurns pcCodeHash $ do
    now <- oeNow
    withTransaction oeStore (`getPrepaidCode` pcCodeHash) >>= \case
      Nothing -> pure $ Left $ ORCode CRInvalid
      Just code
        | Just refusal <- codeRefusal now rid ofLength code -> pure $ Left $ ORCode refusal
        | otherwise -> placeOrder env now req $ codeOrderInsert NewRegistration {nrgId = rid, nrgOwner = orOwner, nrgOrder = fields, nrgReveal = reveal, nrgYears = pcYears, nrgOrderHash = hash} code

-- | The code was checked in its turn, so only a writer outside this process can make its reservation fail.
codeOrderInsert :: NewRegistration -> PrepaidCode -> DB.Connection -> UTCTime -> IO (Either OrderRefusal ())
codeOrderInsert new@NewRegistration {nrgId, nrgOrder = OrderFields {ofLength}} code@PrepaidCode {pcCodeHash} db now =
  createCodeRegistration db new code now >>= \case
    Right () -> pure $ Right ()
    Left CIRLiveOrder -> pure $ Left OROrderExists
    Left CIRCodeNotReservable -> Left . ORCode <$> reserveRefusal db now pcCodeHash nrgId ofLength

checkTimestamp :: UTCTime -> Word64 -> Either OrderRefusal ()
checkTimestamp now timestamp =
  unless (abs (diffUTCTime (roundedToUTCTime $ RoundedSystemTime $ fromIntegral timestamp) now) <= fromInteger signedRequestSeconds) $ Left $ ORBadRequest "the timestamp is outside the window"

-- | The paid years plus the free days until the term starts, by the service's clock, within the slack.
checkDuration :: NamesConfig -> UTCTime -> Int -> Word64 -> Either OrderRefusal ()
checkDuration NamesConfig {nTermStart, nTermSlackSeconds} now years duration = do
  let free = toInteger duration - toInteger years * secondsPerYear
      expected = max 0 (floor $ diffUTCTime nTermStart now)
  when (free < 0 || abs (free - expected) > toInteger nTermSlackSeconds) $ Left $ ORBadRequest "the duration is not the paid years with the free days"

-- | The years the payment covers, or communityYears for an order signing community terms; 'checkTerms' refuses those terms
-- for any other name.
orderYears :: OrderFields -> Int -> Int
orderYears OrderFields {ofAgreedTerms = AgreedTerms {communityName}} = yearsFor communityName

-- | The terms the order signed must be the current ones for its name, the community terms for a name the policy lists,
-- which cannot be ordered while a buyer cannot read them.
checkTerms :: NamesConfig -> Contracts -> OrderFields -> IO (Either OrderRefusal ())
checkTerms cfg ct OrderFields {ofLabelHash, ofAgreedTerms} =
  nameTerms ct ofLabelHash >>= \case
    Left e -> Left ORNameCheckUnavailable <$ logWarn ("names: an order's terms check failed: " <> tshow e)
    Right (community, terms) ->
      readableTerms ct terms <&> \case
        Left wait -> Left $ ORTermsUnavailable wait
        Right t -> unless (ofAgreedTerms == termsFor community t) $ Left $ ORTermsChanged t (communityOf cfg ofLabelHash community)

-- | A name being registered here searches as available, so it is checked here too.
checkNameFree :: Contracts -> DBStore -> Maybe RegistrationId -> LabelHash -> IO (Either OrderRefusal ())
checkNameFree ct st moved lh =
  nameStatus ct lh >>= \case
    Left e -> Left ORNameCheckUnavailable <$ logWarn ("names: an order's name check failed: " <> tshow e)
    Right NSAvailable -> do
      holder <- withTransaction st (`labelHolder` lh)
      pure $ if isJust holder && holder /= moved then Left ORNameTaken else Right ()
    Right _ -> pure $ Left ORNameTaken

-- | The chain price for the length and years, which must be the price the order saw.
checkPrice :: Contracts -> NameLength -> Int -> USDCents -> IO (Either OrderRefusal ())
checkPrice ct len years price =
  chainPriceCents ct len years >>= \case
    Left e -> Left ORNameCheckUnavailable <$ logWarn ("names: an order's price read failed: " <> tshow e)
    Right chainPrice
      | chainPrice /= price -> pure $ Left $ ORCatalogChanged chainPrice
      | otherwise -> pure $ Right ()

-- | Only a taken order can move to another name, within the month from its payment or its code's placement.
movable :: UTCTime -> NameRegistration -> Bool
movable now = maybe False (now <) . movableUntil

readNoteSeconds :: NominalDiffTime
readNoteSeconds = 3600

noteRead :: DBStore -> UTCTime -> NameRegistration -> IO ()
noteRead st now NameRegistration {nrId, nrReadAt} =
  when (diffUTCTime now nrReadAt >= readNoteSeconds) $ withTransaction st $ \db -> markRead db nrId (addUTCTime (negate readNoteSeconds) now) now

orderFieldsOf :: NameRegistration -> OrderFields
orderFieldsOf NameRegistration {nrLabelHash, nrLength, nrDuration, nrCommitment, nrSealed, nrAgreedTerms} =
  OrderFields {ofLabelHash = nrLabelHash, ofLength = nrLength, ofDuration = nrDuration, ofCommitment = nrCommitment, ofSealed = nrSealed, ofAgreedTerms = nrAgreedTerms}

-- | Any other live order of the new commitment, or of the owner and the new label hash, refuses the move.
checkMoveMeets :: DBStore -> NameRegistration -> OrderFields -> UTCTime -> IO (Either OrderRefusal ())
checkMoveMeets st NameRegistration {nrId = rid, nrOwner} OrderFields {ofLabelHash, ofCommitment} now = do
  orders <- withTransaction st $ \db -> getLiveOrders db ofCommitment nrOwner ofLabelHash now
  pure $ if any ((/= rid) . nrId . loRegistration) orders then Left OROrderExists else Right ()

-- | The new name's chain price for its years must be the order's price.
checkMovePrice :: Contracts -> NameRegistration -> NameLength -> Int -> IO (Either OrderRefusal ())
checkMovePrice ct NameRegistration {nrPrice} len years =
  chainPriceCents ct len years >>= \case
    Left e -> Left ORNameCheckUnavailable <$ logWarn ("names: a move's price read failed: " <> tshow e)
    Right price
      | price /= nrPrice -> pure $ Left $ ORPriceTooHigh price
      | otherwise -> pure $ Right ()

-- | The move's write, a compare-and-set on the version read, keeping the order's price. A code order's code is checked and reserved
-- again in the code's turn. A paid order's price is spent from its own balance in the move's transaction and the balance's turn.
retargetOrder :: OrdersEnv -> NameRegistration -> OrderFields -> NameReveal -> IO (Either WriteRefusal Int64)
retargetOrder OrdersEnv {oeStore, oeContracts, oeTurns, oeNow} r@NameRegistration {nrId, nrPayment, nrPrice, nrYears} fields@OrderFields {ofLength} reveal = do
  now <- oeNow
  let write db = versionWritten <$> retargetRegistration db r fields reveal nrPrice now
      spent db v =
        spendBalance db nrId nrPrice now >>= \case
          True -> pure v
          False -> E.throwIO $ OrderRefused $ ORBadRequest "the order's balance does not hold its price"
  case nrPayment of
    OPInvoice {} ->
      checkMovePrice oeContracts r ofLength (orderYears fields nrYears) >>= \case
        Left refusal -> pure $ Left $ WRRefused refusal
        Right () -> withBalanceTurn oeTurns nrId $ either (\(OrderRefused refusal) -> Left $ WRRefused refusal) id <$> E.try (withTransaction oeStore $ \db -> write db >>= traverse (spent db))
    OPCode codeId -> do
      PrepaidCode {pcCodeHash} <- withTransaction oeStore (`getPrepaidCodeById` codeId)
      either (Left . WRRefused . ORCode) id <$> reserveCode oeTurns oeStore now pcCodeHash nrId ofLength write

versionWritten :: VersionWrite -> Either WriteRefusal Int64
versionWritten = \case
  Written v -> Right v
  VersionChanged -> Left WRChanged
  LiveOrderExists -> Left $ WRRefused OROrderExists
  NotLive -> Left $ WRRefused $ ORBadRequest "the order is closed"

confirmLanded :: OrdersEnv -> NameRegistration -> IO Landing
confirmLanded OrdersEnv {oeStore, oeContracts, oeTurns, oeNow} = checkLanded oeStore oeContracts oeTurns oeNow

-- | The register check, on the order's current name: a register mined with status 1, or the owner holding the name at the confirmation depth,
-- went through with only its confirmation lost. The order then becomes registered by a compare-and-set on the version read, a code order spending its code.
-- The caller holds the turn of the order's label hash.
checkLanded :: DBStore -> Contracts -> Turns -> IO UTCTime -> NameRegistration -> IO Landing
checkLanded st ct turns clock r@NameRegistration {nrId, nrOwner, nrLabelHash, nrPayment, nrHistory} = do
  txs <- withTransaction st (`registrationTransactions` nrId)
  let mined = [ntHash | NameTransaction {ntFor = TxRegistration RTRegister _, ntState = TxSettled TSMinedAt {}, ntHash} <- txs]
  runExceptT (landed mined) >>= \case
    Left e -> LUnread <$ logWarn ("names: a register check's chain read failed: " <> tshow e)
    Right Nothing -> pure LNotLanded
    Right (Just (how, expiry)) -> either (const LChanged) LRegistered <$> markRegistered how expiry
  where
    landed mined = do
      confirmed <- ExceptT $ confirmedTag ct
      how <- case mined of
        tx : _ -> pure $ Just $ LandedByReceipt tx
        [] -> (\holder -> if holder == Just nrOwner then Just LandedByOwner else Nothing) <$> ExceptT (ownerOfToken ct confirmed nrLabelHash)
      forM how $ \l -> (l,) <$> ExceptT (nameExpires ct confirmed nrLabelHash)
    markRegistered how expiry = do
      now <- clock
      let registerTx = case how of
            LandedByReceipt tx -> Just tx
            -- judged by ownerOf alone, no hash is known to have mined: none is shown (contract section 6)
            LandedByOwner -> Nothing
          registered = r {nrPhase = ORegistered expiry now, nrHistory = nrHistory {thRegisterTx = registerTx}}
          write db = versionWritten <$> updateRegistration db registered now
      case nrPayment of
        OPInvoice {} -> withTransaction st write
        OPCode codeId -> spendLandedCode turns st now codeId nrId write >>= traverse (paidWith codeId)
    paidWith codeId (v, spent) = do
      unless spent $ do
        PrepaidCode {pcCodeHash} <- withTransaction st (`getPrepaidCodeById` codeId)
        logError $
          "names: registration " <> ridText nrId <> " turned out registered with prepaid code " <> safeDecodeUtf8 (strEncode pcCodeHash)
            <> ", which was released and then spent, revoked or expired: the name was paid with it"
      pure v
    ridText (RegistrationId t) = t
