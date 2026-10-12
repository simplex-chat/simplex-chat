{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.Names.Chain
  ( ChainClient (..),
    ChainError (..),
    ContractError (..),
    TxHash (..),
    sendCommit,
    sendRegister,
  )
where

import BadgeService.Names.Registration (RegisterIntent, commitCalldata, registerWithCreditCalldata, selector)
import Control.Concurrent.STM (TVar)
import Control.Exception (try)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (ChaChaDRG)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.Types as JT
import Data.Bifunctor (first)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Word (Word64)
import Network.HTTP.Client (HttpException, Manager)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (hContentType)
import Numeric (readHex)
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey)
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey)
import Simplex.Messaging.Eth.Transaction (Eip1559Tx (..), signEip1559Tx)
import Simplex.Messaging.SimplexName (SimplexTLD)
import Simplex.Messaging.Util (tshow)

data ChainClient = ChainClient
  { ccManager :: Manager,
    ccRpcUrl :: String,
    ccChainId :: Word64,
    ccController :: Address,
    ccSponsor :: Address,
    ccRegistrarKey :: Secp256k1PrivateKey
  }

data ChainError
  = CERpc Text
  | CEReverted ContractError
  | CEInternal Text
  deriving (Eq, Show)

data ContractError
  = NameNotAvailable
  | CommitmentTooNew
  | CommitmentTooOld
  | UnexpiredCommitmentExists
  | InsufficientAllowance
  | SignatureExpired
  | InvalidSignature
  | UnknownRevert ByteString
  deriving (Eq, Show)

newtype TxHash = TxHash ByteString
  deriving (Eq, Show)

sendCommit :: TVar ChaChaDRG -> ChainClient -> ByteString -> IO (Either ChainError TxHash)
sendCommit g c@ChainClient {ccController} = sendTransaction g c ccController . commitCalldata

sendRegister :: TVar ChaChaDRG -> ChainClient -> SimplexTLD -> RegisterIntent -> IO (Either ChainError TxHash)
sendRegister g c@ChainClient {ccSponsor} tld = sendTransaction g c ccSponsor . registerWithCreditCalldata tld

sendTransaction :: TVar ChaChaDRG -> ChainClient -> Address -> ByteString -> IO (Either ChainError TxHash)
sendTransaction g c@ChainClient {ccChainId, ccRegistrarKey} to calldata = runExceptT $ do
  from <- liftIO $ addressFromPrivateKey g ccRegistrarKey
  let call = J.object ["from" .= from, "to" .= to, "data" .= hex calldata]
  _ <- rpc c "eth_call" [call, "pending"]
  gas <- quantity =<< rpc c "eth_estimateGas" [call, "pending"]
  nonce <- quantity =<< rpc c "eth_getTransactionCount" [J.toJSON from, "pending"]
  tip <- quantity =<< rpc c "eth_maxPriorityFeePerGas" []
  baseFee <- quantity =<< field "baseFeePerGas" =<< rpc c "eth_getBlockByNumber" ["latest", J.Bool False]
  let tx =
        Eip1559Tx
          { txChainId = ccChainId,
            txNonce = fromInteger nonce,
            txMaxPriorityFeePerGas = fromInteger tip,
            txMaxFeePerGas = fromInteger $ 2 * baseFee + tip,
            txGasLimit = fromInteger gas,
            txTo = to,
            txValue = 0,
            txData = calldata
          }
  raw <- withExceptT (CEInternal . T.pack) $ ExceptT $ signEip1559Tx g ccRegistrarKey tx
  TxHash <$> (bytes =<< rpc c "eth_sendRawTransaction" [J.String $ hex raw])

rpc :: ChainClient -> Text -> [J.Value] -> ExceptT ChainError IO J.Value
rpc ChainClient {ccManager, ccRpcUrl} method params = do
  req <- either (throwError . CERpc . tshow) pure $ HTTP.parseRequest ccRpcUrl
  let req' = req {HTTP.method = "POST", HTTP.requestHeaders = [(hContentType, "application/json")], HTTP.requestBody = HTTP.RequestBodyLBS body}
  resp <- liftIO (try $ HTTP.httpLbs req' ccManager) >>= either (\(e :: HttpException) -> throwError $ CERpc $ tshow e) pure
  liftEither $ rpcResult =<< first (CERpc . T.pack) (J.eitherDecode $ HTTP.responseBody resp)
  where
    body = J.encode $ J.object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= method, "params" .= params]

rpcResult :: J.Value -> Either ChainError J.Value
rpcResult v = either (Left . CERpc . T.pack) id $ JT.parseEither response v
  where
    response = J.withObject "JSON-RPC response" $ \o -> o .:? "error" >>= maybe (Right <$> o .: "result") rpcError
    rpcError = J.withObject "JSON-RPC error" $ \e -> do
      revert <- e .:? "data"
      message <- e .: "message"
      pure $ Left $ case revert of
        Just (J.String d) | Just bs <- fromHex d -> CEReverted $ contractError bs
        _ -> CERpc message

contractError :: ByteString -> ContractError
contractError revert = fromMaybe (UnknownRevert revert) $ lookup (B.take 4 revert) [(selector s, e) | (s, e) <- errorSignatures]
  where
    errorSignatures =
      [ ("NameNotAvailable(string)", NameNotAvailable),
        ("CommitmentTooNew(bytes32,uint256,uint256)", CommitmentTooNew),
        ("CommitmentTooOld(bytes32,uint256,uint256)", CommitmentTooOld),
        ("UnexpiredCommitmentExists(bytes32)", UnexpiredCommitmentExists),
        ("InsufficientAllowance(uint256,uint256)", InsufficientAllowance),
        ("SignatureExpired()", SignatureExpired),
        ("InvalidSignature()", InvalidSignature)
      ]

field :: Text -> J.Value -> ExceptT ChainError IO J.Value
field name = liftEither . first (CERpc . T.pack) . JT.parseEither (J.withObject "object" (.: JK.fromText name))

quantity :: J.Value -> ExceptT ChainError IO Integer
quantity = \case
  J.String t | Just h <- T.stripPrefix "0x" t, [(n, "")] <- readHex (T.unpack h) -> pure n
  v -> throwError $ CERpc $ "not a quantity: " <> tshow v

bytes :: J.Value -> ExceptT ChainError IO ByteString
bytes = \case
  J.String t | Just bs <- fromHex t -> pure bs
  v -> throwError $ CERpc $ "not hex data: " <> tshow v

fromHex :: Text -> Maybe ByteString
fromHex t = T.stripPrefix "0x" t >>= either (const Nothing) Just . convertFromBase Base16 . encodeUtf8

hex :: ByteString -> Text
hex bs = "0x" <> decodeLatin1 (convertToBase Base16 bs)
