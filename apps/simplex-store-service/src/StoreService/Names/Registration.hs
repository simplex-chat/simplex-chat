{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Registration
  ( Registration (..),
    RegisterIntent (..),
    Node,
    nodeBytes,
    namehash,
    labelNode,
    contactRecordKey,
    channelRecordKey,
    sponsorDomain,
    makeCommitment,
    commitCalldata,
    registerWithCreditCalldata,
    registerIntentDigest,
    verifyRegisterIntent,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Eth.Address (Address, recoverAddress)
import Simplex.Messaging.Eth.EIP712 (Eip712Domain (..), Eip712Value (..), hashTypedData, serializeSignature)
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexTLD)
import StoreService.Names.Abi (AbiValue (..), abiArray, abiBytes, abiTuple, addressWord, selector, uint256)

data Registration = Registration
  { rLabel :: Text,
    rTLD :: SimplexTLD,
    rOwner :: Address,
    rDuration :: Word64,
    rSecret :: ByteString,
    rResolver :: Address,
    rContactLink :: Maybe Text,
    rChannelLink :: Maybe Text
  }

-- | Only built by 'labelNode', so it always holds 32 bytes.
newtype Node = Node ByteString

nodeBytes :: Node -> ByteString
nodeBytes (Node node) = node

contactRecordKey, channelRecordKey :: ByteString
contactRecordKey = "simplex.contact"
channelRecordKey = "simplex.channel"

data RegisterIntent = RegisterIntent
  { riRegistration :: Registration,
    riNonce :: Word64,
    riDeadline :: Word64,
    riSignature :: RecoverableSignature
  }

sponsorDomain :: Word64 -> Address -> Eip712Domain
sponsorDomain edChainId edVerifyingContract = Eip712Domain {edName = "SimplexSponsor", edVersion = "1", edChainId, edVerifyingContract}

makeCommitment :: Registration -> ByteString
makeCommitment r = keccak256 $ abiTuple [Dynamic $ registrationTuple r]

commitCalldata :: ByteString -> ByteString
commitCalldata commitment = selector "commit(bytes32)" <> commitment

registerWithCreditCalldata :: RegisterIntent -> ByteString
registerWithCreditCalldata RegisterIntent {riRegistration, riNonce, riDeadline, riSignature} =
  selector "registerWithCredit((string,address,uint256,bytes32,address,bytes[],uint8,bytes32),uint256,uint256,bytes)"
    <> abiTuple
      [ Dynamic $ registrationTuple riRegistration,
        Static $ uint256 riNonce,
        Static $ uint256 riDeadline,
        Dynamic $ abiBytes $ serializeSignature riSignature
      ]

registerIntentDigest :: Eip712Domain -> Registration -> Word64 -> Word64 -> ByteString
registerIntentDigest domain r@Registration {rLabel, rOwner, rDuration, rResolver} nonce deadline =
  hashTypedData
    domain
    "RegisterName(string label,address owner,uint256 duration,address resolver,bytes[] data,uint256 nonce,uint256 deadline)"
    [ VBytes $ encodeUtf8 rLabel,
      VAddress rOwner,
      VUint $ fromIntegral rDuration,
      VAddress rResolver,
      VArray $ map VBytes $ recordCalls r,
      VUint $ fromIntegral nonce,
      VUint $ fromIntegral deadline
    ]

verifyRegisterIntent :: Eip712Domain -> Word64 -> RegisterIntent -> IO (Either String ())
verifyRegisterIntent domain now RegisterIntent {riRegistration = r@Registration {rOwner, rSecret}, riNonce, riDeadline, riSignature} = runExceptT $ do
  unless (B.length rSecret == 32) $ throwError "secret: expected 32 bytes"
  when (now > riDeadline) $ throwError "intent expired"
  signer <- ExceptT $ recoverAddress riSignature $ registerIntentDigest domain r riNonce riDeadline
  unless (signer == rOwner) $ throwError "intent is not signed by the owner"

registrationTuple :: Registration -> ByteString
registrationTuple r@Registration {rLabel, rOwner, rDuration, rSecret, rResolver} =
  abiTuple
    [ Dynamic $ abiBytes $ encodeUtf8 rLabel,
      Static $ addressWord rOwner,
      Static $ uint256 rDuration,
      Static rSecret,
      Static $ addressWord rResolver,
      Dynamic $ abiArray $ map abiBytes $ recordCalls r,
      Static reverseRecord,
      Static referrer
    ]
  where
    reverseRecord = uint256 (0 :: Int)
    referrer = B.replicate 32 0

recordCalls :: Registration -> [ByteString]
recordCalls Registration {rLabel, rTLD, rContactLink, rChannelLink} =
  catMaybes [setText contactRecordKey <$> rContactLink, setText channelRecordKey <$> rChannelLink]
  where
    setText key link =
      selector "setText(bytes32,string,string)"
        <> abiTuple [Static node, Dynamic $ abiBytes key, Dynamic $ abiBytes $ encodeUtf8 link]
    Node node = namehash rTLD rLabel

namehash :: SimplexTLD -> Text -> Node
namehash tld = labelNode tld . LabelHash . keccak256 . encodeUtf8

labelNode :: SimplexTLD -> LabelHash -> Node
labelNode tld (LabelHash h) = Node $ keccak256 $ tldNode <> h
  where
    tldNode = keccak256 $ B.replicate 32 0 <> keccak256 (B.drop 1 $ strEncode tld)
