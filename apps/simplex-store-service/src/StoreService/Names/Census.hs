{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Census
  ( RegisteredNames (..),
    NameEntry (..),
    RecordsSet (..),
    recordsLabel,
    registeredNames,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.SystemTime (RoundedSystemTime (..))
import Simplex.Messaging.Util (toChunks)
import StoreService.Names.Abi (AbiError, decodeAddress, decodeBytes32, decodeLabels, decodeString, decodeUint256, uint256)
import StoreService.Names.Chain (Block (..), BlockTag (..), ChainClient (..), ChainConfig (..), ChainError, latestBlock)
import StoreService.Names.Contracts (Contracts (ctChain), callAt, isZeroAddress, textRecordAt)
import StoreService.Names.Registration (LinkKind (..), childNode, recordKey)

data RecordsSet = RNone | RContact | RChannel | RBoth | ROtherResolver
  deriving (Eq, Ord, Show, Enum, Bounded)

recordsLabel :: RecordsSet -> Text
recordsLabel r = case r of
  RNone -> "none"
  RContact -> "contact"
  RChannel -> "channel"
  RBoth -> "both"
  ROtherResolver -> "other_resolver"

-- | Level 2 is a name such as alice.simplex, 3 its subname; the length is in characters.
data NameEntry = NameEntry {enLevel :: Int, enLength :: Int, enRecords :: RecordsSet}
  deriving (Eq, Show)

data RegisteredNames = RegisteredNames {rgBlock :: Word64, rgNames :: [NameEntry]}
  deriving (Eq, Show)

data ReadsAt = ReadsAt
  { raContracts :: Contracts,
    raAt :: BlockTag,
    raRegistry :: Address,
    raSubnames :: Address,
    raResolver :: Address
  }

type Node = (ByteString, Text)

readsAtOnce :: Int
readsAtOnce = 16

childrenPage :: Integer
childrenPage = 100

-- | Every live name and subname at the latest block: names while nameExpires is after the block's time, subnames while
-- the subname registrar names an owner. A name whose resolver is not the configured one is counted without its records.
registeredNames :: Contracts -> IO (Either ChainError RegisteredNames)
registeredNames ct = runExceptT $ do
  Block {blockNumber, blockTimestamp} <- ExceptT $ latestBlock c
  let at = AtBlock blockNumber
  registry <- ExceptT $ callAt ct at cBaseRegistrar ("ens()", "") decodeAddress
  subnames <- ExceptT $ callAt ct at cBaseRegistrar ("subnameHook()", "") decodeAddress
  base <- ExceptT $ callAt ct at cBaseRegistrar ("baseNode()", "") decodeBytes32
  total <- ExceptT $ callAt ct at cBaseRegistrar ("totalSupply()", "") decodeUint256
  let env = ReadsAt {raContracts = ct, raAt = at, raRegistry = registry, raSubnames = subnames, raResolver = cResolver}
      registrar :: ByteString -> Integer -> (ByteString -> Either AbiError a) -> IO (Either ChainError a)
      registrar signature i = readAt env cBaseRegistrar signature (uint256 i)
  ids <- readAll [registrar "tokenByIndex(uint256)" i decodeUint256 | i <- [0 .. total - 1]]
  expiries <- readAll [registrar "nameExpires(uint256)" i decodeUint256 | i <- ids]
  let live = [i | (i, expiry) <- zip ids expiries, expiry > toInteger (roundedSeconds blockTimestamp)]
  labels <- readAll [registrar "labelOf(uint256)" i decodeString | i <- live]
  RegisteredNames blockNumber <$> levels env 2 [(childNode base (LabelHash $ uint256 i), label) | (i, label) <- zip live labels]
  where
    c@ChainClient {ccConfig = ChainConfig {cBaseRegistrar, cResolver}} = ctChain ct

levels :: ReadsAt -> Int -> [Node] -> ExceptT ChainError IO [NameEntry]
levels env level nodes
  | null nodes = pure []
  | otherwise = do
      records <- recordsOf env nodes
      children <- childrenOf env nodes
      (zipWith (\(_, label) r -> NameEntry level (T.length label) r) nodes records <>) <$> levels env (level + 1) children

recordsOf :: ReadsAt -> [Node] -> ExceptT ChainError IO [RecordsSet]
recordsOf env@ReadsAt {raContracts = ct, raAt, raRegistry, raResolver} nodes = do
  resolvers <- readAll [readAt env raRegistry "resolver(bytes32)" node decodeAddress | (node, _) <- nodes]
  readAll [if r == raResolver then textsOf node else pure (Right ROtherResolver) | ((node, _), r) <- zip nodes resolvers]
  where
    textsOf node = do
      contact <- textRecordAt ct raAt node $ recordKey LKContact
      channel <- textRecordAt ct raAt node $ recordKey LKChannel
      pure $ recordsOfTexts <$> contact <*> channel

recordsOfTexts :: Text -> Text -> RecordsSet
recordsOfTexts contact channel = case (T.null contact, T.null channel) of
  (True, True) -> RNone
  (False, True) -> RContact
  (True, False) -> RChannel
  (False, False) -> RBoth

-- | A subname the subname registrar no longer names an owner of, as of a lapsed name or a previous owner, is left out.
childrenOf :: ReadsAt -> [Node] -> ExceptT ChainError IO [Node]
childrenOf env@ReadsAt {raSubnames} parents = do
  counts <- readAll [readAt env raSubnames "childrenLength(bytes32)" node decodeUint256 | (node, _) <- parents]
  let pages = [(node, start) | ((node, _), count) <- zip parents counts, start <- [0, childrenPage .. count - 1]]
  listed <- readAll [readAt env raSubnames "getChildren(bytes32,uint256,uint256)" (node <> uint256 start <> uint256 childrenPage) decodeLabels | (node, start) <- pages]
  let found = [(childNode node (LabelHash labelHash), label) | ((node, _), children) <- zip pages listed, (labelHash, label) <- children]
  owners <- readAll [readAt env raSubnames "ownerOf(uint256)" node decodeAddress | (node, _) <- found]
  pure [child | (child, owner) <- zip found owners, not (isZeroAddress owner)]

readAt :: ReadsAt -> Address -> ByteString -> ByteString -> (ByteString -> Either AbiError a) -> IO (Either ChainError a)
readAt ReadsAt {raContracts, raAt} to signature args = callAt raContracts raAt to (signature, args)

readAll :: [IO (Either ChainError a)] -> ExceptT ChainError IO [a]
readAll calls = concatMap L.toList <$> mapM (ExceptT . fmap sequence . mapConcurrently id) (toChunks readsAtOnce calls)
