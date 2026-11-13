{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Census
  ( Census (..),
    CensusName (..),
    Records (..),
    recordsLabel,
    nameCensus,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (unfoldr)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Eth.Address (Address, unAddress)
import StoreService.Config (ChainConfig (..))
import StoreService.Names.Abi
import StoreService.Names.Chain (Block (..), BlockTag (..), ChainClient (..), ChainError, latestBlock)
import StoreService.Names.Contracts (Contracts, callAt)

data Records = RNone | RContact | RChannel | RBoth | ROtherResolver
  deriving (Eq, Ord, Show, Enum, Bounded)

recordsLabel :: Records -> Text
recordsLabel r = case r of
  RNone -> "none"
  RContact -> "contact"
  RChannel -> "channel"
  RBoth -> "both"
  ROtherResolver -> "other_resolver"

-- | Level 2 is a name such as alice.simplex, 3 its subname; the length is in characters.
data CensusName = CensusName {cnLevel :: Int, cnLength :: Int, cnRecords :: Records}
  deriving (Eq, Show)

data Census = Census {cBlock :: Word64, cNames :: [CensusName]}
  deriving (Eq, Show)

data CensusEnv = CensusEnv
  { ceContracts :: Contracts,
    ceAt :: BlockTag,
    ceRegistry :: Address,
    ceSubnames :: Address,
    ceResolver :: Address
  }

type Node = (ByteString, Text)

readsAtOnce :: Int
readsAtOnce = 16

childrenPage :: Integer
childrenPage = 100

contactKey, channelKey :: ByteString
contactKey = "simplex.contact"
channelKey = "simplex.channel"

-- | Every live name and subname at the latest block: names while nameExpires is after the block's time, subnames while
-- the subname registrar names an owner. A name whose resolver is not the configured one is counted without its records.
nameCensus :: ChainClient -> Contracts -> IO (Either ChainError Census)
nameCensus c@ChainClient {ccConfig = ChainConfig {cBaseRegistrar, cResolver}} ct = runExceptT $ do
  Block {blockNumber, blockTimestamp} <- ExceptT $ latestBlock c
  let at = AtBlock blockNumber
  registry <- ExceptT $ callAt ct at cBaseRegistrar ("ens()", "") decodeAddress
  subnames <- ExceptT $ callAt ct at cBaseRegistrar ("subnameHook()", "") decodeAddress
  base <- ExceptT $ callAt ct at cBaseRegistrar ("baseNode()", "") decodeBytes32
  total <- ExceptT $ callAt ct at cBaseRegistrar ("totalSupply()", "") decodeUint256
  let env = CensusEnv {ceContracts = ct, ceAt = at, ceRegistry = registry, ceSubnames = subnames, ceResolver = cResolver}
      registrar :: ByteString -> Integer -> (ByteString -> Either AbiError a) -> IO (Either ChainError a)
      registrar signature i = readAt env cBaseRegistrar signature (uint256 i)
  ids <- readAll [registrar "tokenByIndex(uint256)" i decodeUint256 | i <- [0 .. total - 1]]
  expiries <- readAll [registrar "nameExpires(uint256)" i decodeUint256 | i <- ids]
  let live = [i | (i, expiry) <- zip ids expiries, expiry > toInteger blockTimestamp]
  labels <- readAll [registrar "labelOf(uint256)" i decodeString | i <- live]
  Census blockNumber <$> levels env 2 [(keccak256 (base <> uint256 i), label) | (i, label) <- zip live labels]

levels :: CensusEnv -> Int -> [Node] -> ExceptT ChainError IO [CensusName]
levels env level nodes
  | null nodes = pure []
  | otherwise = do
      records <- recordsOf env nodes
      children <- childrenOf env nodes
      (zipWith (\(_, label) r -> CensusName level (T.length label) r) nodes records <>) <$> levels env (level + 1) children

recordsOf :: CensusEnv -> [Node] -> ExceptT ChainError IO [Records]
recordsOf env@CensusEnv {ceRegistry, ceResolver} nodes = do
  resolvers <- readAll [readAt env ceRegistry "resolver(bytes32)" node decodeAddress | (node, _) <- nodes]
  readAll [if r == ceResolver then textsOf node else pure (Right ROtherResolver) | ((node, _), r) <- zip nodes resolvers]
  where
    textsOf node = do
      contact <- readAt env ceResolver "text(bytes32,string)" (textArgs node contactKey) decodeString
      channel <- readAt env ceResolver "text(bytes32,string)" (textArgs node channelKey) decodeString
      pure $ recordsOfTexts <$> contact <*> channel
    textArgs node key = abiTuple [Static node, Dynamic (abiBytes key)]

recordsOfTexts :: Text -> Text -> Records
recordsOfTexts contact channel = case (T.null contact, T.null channel) of
  (True, True) -> RNone
  (False, True) -> RContact
  (True, False) -> RChannel
  (False, False) -> RBoth

-- | A subname the subname registrar no longer names an owner of, as of a lapsed name or a previous owner, is left out.
childrenOf :: CensusEnv -> [Node] -> ExceptT ChainError IO [Node]
childrenOf env@CensusEnv {ceSubnames} parents = do
  counts <- readAll [readAt env ceSubnames "childrenLength(bytes32)" node decodeUint256 | (node, _) <- parents]
  let pages = [(node, start) | ((node, _), count) <- zip parents counts, start <- [0, childrenPage .. count - 1]]
  listed <- readAll [readAt env ceSubnames "getChildren(bytes32,uint256,uint256)" (node <> uint256 start <> uint256 childrenPage) decodeLabels | (node, start) <- pages]
  let found = [(keccak256 (node <> labelHash), label) | ((node, _), children) <- zip pages listed, (labelHash, label) <- children]
  owners <- readAll [readAt env ceSubnames "ownerOf(uint256)" node decodeAddress | (node, _) <- found]
  pure [child | (child, owner) <- zip found owners, not (B.all (== 0) (unAddress owner))]

readAt :: CensusEnv -> Address -> ByteString -> ByteString -> (ByteString -> Either AbiError a) -> IO (Either ChainError a)
readAt CensusEnv {ceContracts, ceAt} to signature args = callAt ceContracts ceAt to (signature, args)

readAll :: [IO (Either ChainError a)] -> ExceptT ChainError IO [a]
readAll calls = concat <$> mapM (ExceptT . fmap sequence . mapConcurrently id) (groupsOf readsAtOnce calls)

groupsOf :: Int -> [a] -> [[a]]
groupsOf n = unfoldr (\xs -> if null xs then Nothing else Just (splitAt n xs))
