{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Contracts
  ( Contracts,
    NameStatus (..),
    RegisteredName (..),
    NameRecords (..),
    CommitmentAges (..),
    newContracts,
    secondsPerYear,
    nameStatus,
    nameRecords,
    chainPriceCents,
    codeLengthFor,
    sponsorNonce,
    resolverNonce,
    ownerOfToken,
    nameExpires,
    commitmentTime,
    controllerAvailable,
    commitmentAges,
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Word (Word64)
import Simplex.Chat.Badges.Code (maxNameLength, minNameLength)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexTLD)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..))
import StoreService.Names.Abi
import StoreService.Names.Chain (Block (..), ChainClient (..), ChainError (..), ethCall, latestBlock)
import StoreService.Names.Registration (channelRecordKey, contactRecordKey, labelNode, nodeBytes)

data Contracts = Contracts
  { ctChain :: ChainClient,
    ctTLD :: SimplexTLD,
    ctRegistrar :: IORef (Maybe RegistrarConstants),
    ctCommitmentAges :: IORef (Maybe CommitmentAges)
  }

data RegistrarConstants = RegistrarConstants {rcRegistry :: Address, rcGracePeriod :: Integer}

data NameStatus = NSAvailable | NSReserved | NSRegistered RegisteredName
  deriving (Eq, Show)

data RegisteredName = RegisteredName
  { rnOwner :: Address,
    rnResolver :: Address,
    rnExpiresAt :: Word64,
    rnGracePeriod :: Integer
  }
  deriving (Eq, Show)

-- | The default resolver's records ("" when unset) and the owner's nonce for the next edit.
data NameRecords = NameRecords {rdContact :: Text, rdChannel :: Text, rdNonce :: Word64}
  deriving (Eq, Show)

data CommitmentAges = CommitmentAges {caMin :: Integer, caMax :: Integer}
  deriving (Eq, Show)

secondsPerYear :: Integer
secondsPerYear = 31536000

attoUsdPerCent :: Integer
attoUsdPerCent = 10 ^ (16 :: Int)

-- | Reads nothing: the registrar's constants and the commitment ages are read on first use and kept once read.
newContracts :: SimplexTLD -> ChainClient -> IO Contracts
newContracts ctTLD ctChain = do
  ctRegistrar <- newIORef Nothing
  ctCommitmentAges <- newIORef Nothing
  pure Contracts {ctChain, ctTLD, ctRegistrar, ctCommitmentAges}

-- | Registered while @nameExpires + GRACE_PERIOD@ is not before the latest block's time.
nameStatus :: Contracts -> LabelHash -> IO (Either ChainError NameStatus)
nameStatus ct lh = runExceptT $ do
  RegistrarConstants {rcRegistry, rcGracePeriod} <- ExceptT $ registrarConstants ct
  expires <- ExceptT $ nameExpires ct lh
  Block {blockTimestamp} <- ExceptT $ latestBlock $ ctChain ct
  if toInteger expires + rcGracePeriod >= toInteger blockTimestamp
    then do
      let node = nodeArg ct lh
      rnOwner <- ExceptT $ call ct rcRegistry ("owner(bytes32)", node) decodeAddress
      rnResolver <- ExceptT $ call ct rcRegistry ("resolver(bytes32)", node) decodeAddress
      pure $ NSRegistered RegisteredName {rnOwner, rnResolver, rnExpiresAt = expires, rnGracePeriod = rcGracePeriod}
    else do
      reason <- ExceptT $ call ct (controller ct) ("reservedNames(bytes32)", labelHashArg lh) decodeUint256
      pure $ if reason /= 0 then NSReserved else NSAvailable

-- | Nothing for a name on another resolver contract, or none: the service reads no records from a contract it does not know.
nameRecords :: Contracts -> LabelHash -> RegisteredName -> IO (Either ChainError (Maybe NameRecords))
nameRecords ct lh RegisteredName {rnOwner, rnResolver}
  | rnResolver /= resolver ct = pure $ Right Nothing
  | otherwise = runExceptT $ do
      rdContact <- ExceptT $ textRecord contactRecordKey
      rdChannel <- ExceptT $ textRecord channelRecordKey
      rdNonce <- ExceptT $ resolverNonce ct rnOwner
      pure $ Just NameRecords {rdContact, rdChannel, rdNonce}
  where
    textRecord key = call ct (resolver ct) ("text(bytes32,string)", abiTuple [Static $ nodeArg ct lh, Dynamic $ abiBytes key]) decodeString

-- | Cents for a label of this length and these years. The oracle prices by length only,
-- so a placeholder label of that length keeps the label from the RPC node.
chainPriceCents :: Contracts -> Int -> Int -> IO (Either ChainError Int)
chainPriceCents ct len years = call ct (controller ct) ("rentPriceUSD(string,uint256)", args) $ \ret -> do
  base <- decodeUint256 ret
  let cents = base `div` attoUsdPerCent
  if cents <= toInteger (maxBound :: Int) then Right (fromInteger cents) else Left AERange
  where
    args = abiTuple [Dynamic $ abiBytes $ B.replicate len 'a', Static $ uint256 $ toInteger years * secondsPerYear]

-- | The shortest length from the shortest label such that every length up to the longest costs at most these cents
-- for these years, from one read of the oracle's whole price list, which need not fall with the length.
codeLengthFor :: Contracts -> Int -> Int -> IO (Either ChainError (Maybe Int))
codeLengthFor ct cents years = runExceptT $ do
  oracle <- ExceptT $ call ct (controller ct) ("prices()", "") decodeAddress
  (basePerYear, exceptions) <- ExceptT $ call ct oracle ("prices()", "") decodePrices
  let perYear len = M.findWithDefault basePerYear (toInteger len) $ M.fromList exceptions
      covered len = perYear len * toInteger years <= toInteger cents
  pure $ case takeWhile covered [maxNameLength, maxNameLength - 1 .. minNameLength] of
    [] -> Nothing
    lens -> Just $ last lens

sponsorNonce :: Contracts -> Address -> IO (Either ChainError Word64)
sponsorNonce ct owner = call ct (cSponsor $ chainConfig ct) ("nonces(address)", addressWord owner) decodeWord64

resolverNonce :: Contracts -> Address -> IO (Either ChainError Word64)
resolverNonce ct owner = call ct (resolver ct) ("nonces(address)", addressWord owner) decodeWord64

-- | Nothing when nobody holds the name: @ownerOf@ reverts for it, and that revert is the answer.
ownerOfToken :: Contracts -> LabelHash -> IO (Either ChainError (Maybe Address))
ownerOfToken ct lh =
  call ct (baseRegistrar ct) ("ownerOf(uint256)", labelHashArg lh) decodeAddress >>= \case
    Left (ChainReverted _) -> pure $ Right Nothing
    r -> pure $ Just <$> r

nameExpires :: Contracts -> LabelHash -> IO (Either ChainError Word64)
nameExpires ct lh = call ct (baseRegistrar ct) ("nameExpires(uint256)", labelHashArg lh) decodeWord64

-- | The block time of the commitment, Nothing when the controller holds none.
commitmentTime :: Contracts -> ByteString -> IO (Either ChainError (Maybe Word64))
commitmentTime ct commitment = fmap nonZero <$> call ct (controller ct) ("commitments(bytes32)", commitment) decodeWord64
  where
    nonZero t = if t == 0 then Nothing else Just t

controllerAvailable :: Contracts -> Text -> IO (Either ChainError Bool)
controllerAvailable ct label = call ct (controller ct) ("available(string)", abiTuple [Dynamic $ abiBytes $ encodeUtf8 label]) decodeBool

commitmentAges :: Contracts -> IO (Either ChainError CommitmentAges)
commitmentAges ct@Contracts {ctCommitmentAges} = readOnce ctCommitmentAges $ runExceptT $ do
  caMin <- ExceptT $ call ct (controller ct) ("minCommitmentAge()", "") decodeUint256
  caMax <- ExceptT $ call ct (controller ct) ("maxCommitmentAge()", "") decodeUint256
  pure CommitmentAges {caMin, caMax}

registrarConstants :: Contracts -> IO (Either ChainError RegistrarConstants)
registrarConstants ct@Contracts {ctRegistrar} = readOnce ctRegistrar $ runExceptT $ do
  rcRegistry <- ExceptT $ call ct (baseRegistrar ct) ("ens()", "") decodeAddress
  rcGracePeriod <- ExceptT $ call ct (baseRegistrar ct) ("GRACE_PERIOD()", "") decodeUint256
  pure RegistrarConstants {rcRegistry, rcGracePeriod}

-- | Concurrent first uses may each read; they read the same immutable values.
readOnce :: IORef (Maybe a) -> IO (Either ChainError a) -> IO (Either ChainError a)
readOnce ref readChain =
  readIORef ref >>= \case
    Just a -> pure $ Right a
    Nothing -> readChain >>= either (pure . Left) (\a -> Right a <$ atomicWriteIORef ref (Just a))

-- | An answer that does not decode is a node failure, as an RPC error is.
call :: Contracts -> Address -> (ByteString, ByteString) -> (ByteString -> Either AbiError a) -> IO (Either ChainError a)
call Contracts {ctChain} to (signature, args) decode =
  (>>= first malformed . decode) <$> ethCall ctChain to (selector signature <> args)
  where
    malformed e = ChainUnavailable $ "eth_call: " <> decodeLatin1 signature <> " answered " <> tshow e

labelHashArg :: LabelHash -> ByteString
labelHashArg (LabelHash h) = h

nodeArg :: Contracts -> LabelHash -> ByteString
nodeArg Contracts {ctTLD} = nodeBytes . labelNode ctTLD

chainConfig :: Contracts -> ChainConfig
chainConfig = ccConfig . ctChain

controller, resolver, baseRegistrar :: Contracts -> Address
controller = cController . chainConfig
resolver = cResolver . chainConfig
baseRegistrar = cBaseRegistrar . chainConfig
