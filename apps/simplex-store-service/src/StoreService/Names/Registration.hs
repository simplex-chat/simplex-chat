{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module StoreService.Names.Registration
  ( Registration (..),
    RegisterIntent (..),
    DisputeDays,
    mkDisputeDays,
    unDisputeDays,
    disputeSeconds,
    minDisputeDays,
    maxDisputeDays,
    methodDisputeDays,
    codeDisputeDays,
    Commitment,
    TermsHash,
    TermsAcceptance (..),
    CommunityName,
    termsKindText,
    TermsKind (..),
    termsKind,
    Node,
    nodeBytes,
    namehash,
    labelNode,
    childNode,
    tldLabel,
    LinkKind (..),
    recordKey,
    linkMaxBytes,
    nameLink,
    sponsorDomain,
    resolverDomain,
    policyDomain,
    makeCommitment,
    commitCalldata,
    registerWithCreditCalldata,
    registerNameType,
    registerIntentDigest,
    verifyRegisterIntent,
    acceptanceDigest,
    verifyAcceptance,
    acceptCalldata,
    setTextDigest,
    setTextWithSigCalldata,
    multicallCalldata,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Crypto.Number.Serialize (os2ip)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Simplex.Chat.PaymentService.Types (ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Protocol (ConnShortLink (..), ConnectionMode (..), ContactConnType (..))
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Encoding (Encoding (..))
import Simplex.Messaging.Encoding.String (StrEncoding (..), TextEncoding (..))
import Simplex.Messaging.Eth.Address (Address, recoverAddress)
import Simplex.Messaging.Eth.EIP712 (Eip712Domain (..), Eip712Value (..), hashTypedData)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON)
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexTLD (..), labelHash)
import Simplex.Messaging.SystemTime (RoundedSystemTime (..), SystemSeconds, roundedToUTCTime)
import StoreService.Names.Abi (AbiValue (..), abiArray, abiBytes, abiTuple, addressWord, selector, uint256)
import StoreService.Names.Chain (Hex32 (..))

data Registration = Registration
  { rLabel :: Text,
    rTLD :: SimplexTLD,
    rOwner :: Address,
    rDuration :: Word64,
    rSecret :: Hex32,
    rResolver :: Address,
    rContactLink :: Maybe Text,
    rChannelLink :: Maybe Text
  }

type Commitment = Hex32

type TermsHash = Hex32

-- | Only built by 'labelNode', so it always holds 32 bytes.
newtype Node = Node ByteString

nodeBytes :: Node -> ByteString
nodeBytes (Node node) = node

data LinkKind = LKContact | LKChannel
  deriving (Eq, Show)

instance TextEncoding LinkKind where
  textEncode = \case
    LKContact -> "simplex.contact"
    LKChannel -> "simplex.channel"
  textDecode = \case
    "simplex.contact" -> Just LKContact
    "simplex.channel" -> Just LKChannel
    _ -> Nothing

recordKey :: LinkKind -> ByteString
recordKey = encodeUtf8 . textEncode

linkMaxBytes :: Int
linkMaxBytes = 1024

nameLink :: Text -> Maybe (LinkKind, ConnShortLink 'CMContact)
nameLink link
  | B.length s > linkMaxBytes = Nothing
  | otherwise = case strDecode @(ConnShortLink 'CMContact) s of
      Right sl@(CSLContact _ CCTContact _ _) -> Just (LKContact, sl)
      Right sl@(CSLContact _ CCTChannel _ _) -> Just (LKChannel, sl)
      _ -> Nothing
  where
    s = encodeUtf8 link

data RegisterIntent = RegisterIntent
  { riRegistration :: Registration,
    riDisputeDays :: DisputeDays,
    riNonce :: Word64,
    riDeadline :: SystemSeconds,
    riSignature :: RecoverableSignature
  }

newtype DisputeDays = DisputeDays Int
  deriving (Eq, Ord, Show)
  deriving newtype (J.ToJSON)

mkDisputeDays :: Int -> Maybe DisputeDays
mkDisputeDays n
  | minDisputeDays <= days && days <= maxDisputeDays = Just days
  | otherwise = Nothing
  where
    days = DisputeDays n

unDisputeDays :: DisputeDays -> Int
unDisputeDays (DisputeDays n) = n

disputeSeconds :: DisputeDays -> Word64
disputeSeconds (DisputeDays n) = fromIntegral n * 86400

minDisputeDays, maxDisputeDays :: DisputeDays
minDisputeDays = DisputeDays 14
maxDisputeDays = DisputeDays 120

methodDisputeDays :: ServicePaymentMethod -> DisputeDays
methodDisputeDays = \case
  SPMCard _ -> cardDisputeDays
  SPMCrypto _ -> cryptoDisputeDays

cardDisputeDays, cryptoDisputeDays, codeDisputeDays :: DisputeDays
cardDisputeDays = DisputeDays 120
cryptoDisputeDays = DisputeDays 14
codeDisputeDays = DisputeDays 14

sponsorDomain :: Word64 -> Address -> Eip712Domain
sponsorDomain edChainId edVerifyingContract = Eip712Domain {edName = "SimplexSponsor", edVersion = "1", edChainId, edVerifyingContract}

resolverDomain :: Word64 -> Address -> Eip712Domain
resolverDomain edChainId edVerifyingContract = Eip712Domain {edName = "SimplexResolver", edVersion = "1", edChainId, edVerifyingContract}

policyDomain :: Word64 -> Address -> Eip712Domain
policyDomain edChainId edVerifyingContract = Eip712Domain {edName = "SimplexNamePolicy", edVersion = "1", edChainId, edVerifyingContract}

-- | The owner's acceptance of the terms of the name's kind for a commitment, relayed to the name policy before the register.
-- Its deadline is the reveal's.
data TermsAcceptance = TermsAcceptance
  { termsHash :: TermsHash,
    nonce :: Word64,
    signature :: RecoverableSignature
  }
  deriving (Eq, Show)

type CommunityName = Bool

termsKindText :: CommunityName -> Text
termsKindText community = if community then "community" else "general"

data TermsKind = TermsKind {tkAcceptType :: ByteString, tkRelayFunction :: ByteString}

termsKind :: CommunityName -> TermsKind
termsKind community
  | community =
      TermsKind
        { tkAcceptType = "AcceptCommunityTerms(bytes32 commitment,bytes32 termsHash,uint256 nonce,uint256 deadline)",
          tkRelayFunction = "commitUnderCommunityTermsWithSig(bytes32,bytes32,address,uint256,uint256,bytes)"
        }
  | otherwise =
      TermsKind
        { tkAcceptType = "AcceptGeneralTerms(bytes32 commitment,bytes32 termsHash,uint256 nonce,uint256 deadline)",
          tkRelayFunction = "commitUnderGeneralTermsWithSig(bytes32,bytes32,address,uint256,uint256,bytes)"
        }

makeCommitment :: Registration -> Commitment
makeCommitment r = Hex32 $ keccak256 $ abiTuple [Dynamic $ registrationTuple r]

commitCalldata :: Commitment -> ByteString
commitCalldata (Hex32 commitment) = selector "commit(bytes32)" <> commitment

registerWithCreditCalldata :: RegisterIntent -> ByteString
registerWithCreditCalldata RegisterIntent {riRegistration, riDisputeDays, riNonce, riDeadline, riSignature} =
  selector "registerWithCredit((string,address,uint256,bytes32,address,bytes[],uint8,bytes32),uint256,uint256,uint256,bytes)"
    <> abiTuple
      [ Dynamic $ registrationTuple riRegistration,
        Static $ uint256 $ unDisputeDays riDisputeDays,
        Static $ uint256 riNonce,
        Static $ uint256 $ roundedSeconds riDeadline,
        Dynamic $ abiBytes $ smpEncode riSignature
      ]

registerNameType :: ByteString
registerNameType = "RegisterName(string label,address owner,uint256 duration,address resolver,bytes[] data,uint256 disputeDays,uint256 nonce,uint256 deadline)"

registerIntentDigest :: Eip712Domain -> Registration -> DisputeDays -> Word64 -> SystemSeconds -> ByteString
registerIntentDigest domain r@Registration {rLabel, rOwner, rDuration, rResolver} days nonce deadline =
  hashTypedData
    domain
    registerNameType
    [ VBytes $ encodeUtf8 rLabel,
      VAddress rOwner,
      VUint $ fromIntegral rDuration,
      VAddress rResolver,
      VArray $ map VBytes $ recordCalls r,
      VUint $ fromIntegral $ unDisputeDays days,
      VUint $ fromIntegral nonce,
      VUint $ fromIntegral $ roundedSeconds deadline
    ]

verifyRegisterIntent :: Eip712Domain -> UTCTime -> RegisterIntent -> IO (Either String ())
verifyRegisterIntent domain now RegisterIntent {riRegistration = r@Registration {rOwner}, riDisputeDays, riNonce, riDeadline, riSignature} = runExceptT $ do
  when (now > roundedToUTCTime riDeadline) $ throwError "intent expired"
  signer <- ExceptT $ recoverAddress riSignature $ registerIntentDigest domain r riDisputeDays riNonce riDeadline
  unless (signer == rOwner) $ throwError "intent is not signed by the owner"

acceptanceDigest :: Eip712Domain -> TermsKind -> Commitment -> TermsHash -> Word64 -> SystemSeconds -> ByteString
acceptanceDigest domain TermsKind {tkAcceptType} (Hex32 commitment) (Hex32 termsHash) nonce deadline =
  hashTypedData
    domain
    tkAcceptType
    [bytes32 commitment, bytes32 termsHash, VUint $ fromIntegral nonce, VUint $ fromIntegral $ roundedSeconds deadline]

verifyAcceptance :: Eip712Domain -> TermsKind -> Address -> Commitment -> SystemSeconds -> TermsAcceptance -> IO (Either String ())
verifyAcceptance domain kind owner commitment deadline TermsAcceptance {termsHash, nonce, signature} = runExceptT $ do
  signer <- ExceptT $ recoverAddress signature $ acceptanceDigest domain kind commitment termsHash nonce deadline
  unless (signer == owner) $ throwError "acceptance is not signed by the owner"

acceptCalldata :: TermsKind -> Commitment -> Address -> SystemSeconds -> TermsAcceptance -> ByteString
acceptCalldata TermsKind {tkRelayFunction} (Hex32 commitment) owner deadline TermsAcceptance {termsHash = Hex32 termsHash, nonce, signature} =
  selector tkRelayFunction
    <> abiTuple
      [ Static commitment,
        Static termsHash,
        Static $ addressWord owner,
        Static $ uint256 nonce,
        Static $ uint256 $ roundedSeconds deadline,
        Dynamic $ abiBytes $ smpEncode signature
      ]

setTextDigest :: Eip712Domain -> Node -> LinkKind -> Text -> Word64 -> SystemSeconds -> ByteString
setTextDigest domain node kind value nonce deadline =
  hashTypedData
    domain
    "SetText(bytes32 node,string key,string value,uint256 nonce,uint256 deadline)"
    [bytes32 $ nodeBytes node, VBytes $ recordKey kind, VBytes $ encodeUtf8 value, VUint $ fromIntegral nonce, VUint $ fromIntegral $ roundedSeconds deadline]

setTextWithSigCalldata :: Node -> LinkKind -> Text -> Word64 -> SystemSeconds -> RecoverableSignature -> ByteString
setTextWithSigCalldata (Node node) kind value nonce deadline sig =
  selector "setTextWithSig(bytes32,string,string,uint256,uint256,bytes)"
    <> abiTuple
      [ Static node,
        Dynamic $ abiBytes $ recordKey kind,
        Dynamic $ abiBytes $ encodeUtf8 value,
        Static $ uint256 nonce,
        Static $ uint256 $ roundedSeconds deadline,
        Dynamic $ abiBytes $ smpEncode sig
      ]

-- | The resolver runs each call on itself in order, and reverts all of them when one fails.
multicallCalldata :: [ByteString] -> ByteString
multicallCalldata calls = selector "multicall(bytes[])" <> abiTuple [Dynamic $ abiArray $ map abiBytes calls]

bytes32 :: ByteString -> Eip712Value
bytes32 b = VUint $ fromInteger $ os2ip b

registrationTuple :: Registration -> ByteString
registrationTuple r@Registration {rLabel, rOwner, rDuration, rSecret = Hex32 rSecret, rResolver} =
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
  catMaybes [setText (recordKey LKContact) <$> rContactLink, setText (recordKey LKChannel) <$> rChannelLink]
  where
    setText key link =
      selector "setText(bytes32,string,string)"
        <> abiTuple [Static node, Dynamic $ abiBytes key, Dynamic $ abiBytes $ encodeUtf8 link]
    Node node = namehash rTLD rLabel

namehash :: SimplexTLD -> Text -> Node
namehash tld = labelNode tld . labelHash

labelNode :: SimplexTLD -> LabelHash -> Node
labelNode tld = Node . childNode (childNode (B.replicate 32 0) (labelHash $ tldLabel tld))

childNode :: ByteString -> LabelHash -> ByteString
childNode parent (LabelHash h) = keccak256 $ parent <> h

tldLabel :: SimplexTLD -> Text
tldLabel TLDSimplex = "simplex"
tldLabel TLDTesting = "testing"
tldLabel TLDWeb = ""

$(JQ.deriveJSON (enumJSON $ dropPrefix "LK") ''LinkKind)

$(JQ.deriveFromJSON defaultJSON ''TermsAcceptance)
