{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The checks every order runs, by paid invoice or prepaid code, in the contract's order.
module StoreService.Names.Orders
  ( OrdersEnv (..),
    ordersEnv,
    OrderRequest (..),
    OrderRefusal (..),
    ExistingOrder (..),
    ClosedOrder (..),
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
    checkTerms,
    checkNameFree,
    checkPrice,
  )
where

import qualified Control.Exception as E
import Control.Monad (unless, when)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Int (Int64)
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
import StoreService.Log (logWarn)
import StoreService.Names.Codes (CodeRefusal (..), codeRefusal, reserveRefusal)
import StoreService.Names.Community (communityJSON, termsFor, termsJSON)
import StoreService.Names.Contracts (Contracts, NameStatus (..), chainPriceCents, nameStatus, secondsPerYear)
import StoreService.Names.Env (NamesEnv (..))
import StoreService.Names.Phases (Phases, RegistrationId (..))
import StoreService.Names.Abi (bytes32FromHex)
import StoreService.Names.Signatures (OrderFields (..), orderMessage, parseChecksumAddress, parseSignature, personalDigest)
import StoreService.Names.Store (LiveOrder (..), NameRegistration (..), NewRegistration (..), PrepaidCode (..), RegistrationRefused (..), closeOverdue, createCodeRegistration, getLiveOrders, getPrepaidCode, getRegistrationByOrderHash, isLabelRegistering)
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
  | -- | the current terms and the name's community entry, as the answer carries them
    ORTermsChanged J.Value J.Value
  | ORCodesOnly
  deriving (Eq, Show)

newtype OrderRefused = OrderRefused OrderRefusal
  deriving (Show)

instance E.Exception OrderRefused

-- | An exact resend of a stored order, live or not; or the overdue orders of the same wallet and name the new order closes.
data ExistingOrder = EOResent NameRegistration | EONew [NameRegistration] | EOMeets

-- | An overdue order a new order closed, at its new version; its invoice is now expired.
data ClosedOrder = ClosedOrder {coRegistration :: NameRegistration, coVersion :: Int64}

maxSealedChars :: Int
maxSealedChars = 11095

-- | The terms version, of up to 32 characters, and a suffix such as /community.
maxAgreedTermsChars :: Int
maxAgreedTermsChars = 48

-- | How far a signed request's timestamp may be from the service's clock.
signedRequestSeconds :: Integer
signedRequestSeconds = 600

-- | Every format check; the length's range is checkLength's, after the formats a kind of order adds.
parseOrderRequest :: J.Object -> Maybe OrderRequest
parseOrderRequest o = do
  orOwner <- either (const Nothing) Just . parseChecksumAddress =<< textField o "owner"
  (orFields, orTimestamp, orSignature) <- parseOrderFields o
  pure OrderRequest {orOwner, orFields, orTimestamp, orSignature}

-- | The signed fields of an order, with their timestamp and signature.
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

-- | The terms the order signed must be the current ones for its name, the community variant for a listed name.
checkTerms :: NamesConfig -> OrderFields -> Either OrderRefusal ()
checkTerms cfg OrderFields {ofLabelHash, ofAgreedTerms} =
  unless (ofAgreedTerms == termsFor cfg ofLabelHash) $ Left $ ORTermsChanged (termsJSON cfg) (communityJSON cfg ofLabelHash)

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
