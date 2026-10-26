{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Registration
  ( Registration (..),
    RegisterIntent (..),
    TermsAcceptance (..),
    Node,
    nodeBytes,
    namehash,
    labelNode,
    contactRecordKey,
    channelRecordKey,
    sponsorDomain,
    resolverDomain,
    policyDomain,
    makeCommitment,
    commitCalldata,
    registerWithCreditCalldata,
    registerIntentDigest,
    verifyRegisterIntent,
    acceptanceDigest,
    verifyAcceptance,
    acceptCalldata,
    setTextDigest,
    clearRecordsDigest,
    setTextWithSigCalldata,
    clearRecordsWithSigCalldata,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Crypto.Number.Serialize (os2ip)
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

resolverDomain :: Word64 -> Address -> Eip712Domain
resolverDomain edChainId edVerifyingContract = Eip712Domain {edName = "SimplexResolver", edVersion = "1", edChainId, edVerifyingContract}

policyDomain :: Word64 -> Address -> Eip712Domain
policyDomain edChainId edVerifyingContract = Eip712Domain {edName = "SimplexNamePolicy", edVersion = "1", edChainId, edVerifyingContract}

-- | The owner's acceptance of the community terms for a commitment, relayed to the name policy before the register.
-- Its deadline is the reveal's.
data TermsAcceptance = TermsAcceptance
  { taTermsHash :: ByteString,
    taNonce :: Word64,
    taSignature :: RecoverableSignature
  }
  deriving (Eq, Show)

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

acceptanceDigest :: Eip712Domain -> ByteString -> ByteString -> Word64 -> Word64 -> ByteString
acceptanceDigest domain commitment termsHash nonce deadline =
  hashTypedData
    domain
    "AcceptBonaFideTerms(bytes32 commitment,bytes32 termsHash,uint256 nonce,uint256 deadline)"
    [word commitment, word termsHash, VUint $ fromIntegral nonce, VUint $ fromIntegral deadline]
  where
    word = VUint . fromInteger . os2ip

verifyAcceptance :: Eip712Domain -> Address -> ByteString -> Word64 -> TermsAcceptance -> IO (Either String ())
verifyAcceptance domain owner commitment deadline TermsAcceptance {taTermsHash, taNonce, taSignature} = runExceptT $ do
  signer <- ExceptT $ recoverAddress taSignature $ acceptanceDigest domain commitment taTermsHash taNonce deadline
  unless (signer == owner) $ throwError "acceptance is not signed by the owner"

acceptCalldata :: ByteString -> Address -> Word64 -> TermsAcceptance -> ByteString
acceptCalldata commitment owner deadline TermsAcceptance {taTermsHash, taNonce, taSignature} =
  selector "commitUnderBonaFideCommunityTermsWithSig(bytes32,bytes32,address,uint256,uint256,bytes)"
    <> abiTuple
      [ Static commitment,
        Static taTermsHash,
        Static $ addressWord owner,
        Static $ uint256 taNonce,
        Static $ uint256 deadline,
        Dynamic $ abiBytes $ serializeSignature taSignature
      ]

setTextDigest :: Eip712Domain -> Node -> Text -> Text -> Word64 -> Word64 -> ByteString
setTextDigest domain node key value nonce deadline =
  hashTypedData
    domain
    "SetText(bytes32 node,string key,string value,uint256 nonce,uint256 deadline)"
    [bytes32 node, VBytes $ encodeUtf8 key, VBytes $ encodeUtf8 value, VUint $ fromIntegral nonce, VUint $ fromIntegral deadline]

clearRecordsDigest :: Eip712Domain -> Node -> Word64 -> Word64 -> ByteString
clearRecordsDigest domain node nonce deadline =
  hashTypedData
    domain
    "ClearRecords(bytes32 node,uint256 nonce,uint256 deadline)"
    [bytes32 node, VUint $ fromIntegral nonce, VUint $ fromIntegral deadline]

setTextWithSigCalldata :: Node -> Text -> Text -> Word64 -> Word64 -> RecoverableSignature -> ByteString
setTextWithSigCalldata (Node node) key value nonce deadline sig =
  selector "setTextWithSig(bytes32,string,string,uint256,uint256,bytes)"
    <> abiTuple
      [ Static node,
        Dynamic $ abiBytes $ encodeUtf8 key,
        Dynamic $ abiBytes $ encodeUtf8 value,
        Static $ uint256 nonce,
        Static $ uint256 deadline,
        Dynamic $ abiBytes $ serializeSignature sig
      ]

clearRecordsWithSigCalldata :: Node -> Word64 -> Word64 -> RecoverableSignature -> ByteString
clearRecordsWithSigCalldata (Node node) nonce deadline sig =
  selector "clearRecordsWithSig(bytes32,uint256,uint256,bytes)"
    <> abiTuple [Static node, Static $ uint256 nonce, Static $ uint256 deadline, Dynamic $ abiBytes $ serializeSignature sig]

bytes32 :: Node -> Eip712Value
bytes32 (Node node) = VUint $ fromInteger $ os2ip node

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
