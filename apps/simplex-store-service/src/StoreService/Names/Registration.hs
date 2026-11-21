{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module StoreService.Names.Registration
  ( RegistrationId (..),
    Registration (..),
    RegisterIntent (..),
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
    LabelError (..),
    validLabel,
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
    registerIntentDigest,
    verifyRegisterIntent,
    acceptanceDigest,
    verifyAcceptance,
    acceptCalldata,
    setTextDigest,
    setTextWithSigCalldata,
    multicallCalldata,
    AddressError (..),
    OrderFields (..),
    AgreedTerms (..),
    communityTermsPrefix,
    parseSignature,
    personalDigest,
    orderMessage,
    findMessage,
    retargetMessage,
    spendMessage,
    parseChecksumAddress,
    parseAnyCaseAddress,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Crypto.Number.Serialize (os2ip)
import qualified Data.Aeson.TH as JQ
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Simplex.Chat.Badges.Code (NameLength, nameLength, nameLengthInt)
import Simplex.Messaging.Agent.Protocol (ConnShortLink (..), ConnectionMode (..), ContactConnType (..))
import Simplex.Messaging.Agent.Store.DB (FromField (..), ToField (..), fromTextField_)
import Simplex.Messaging.Crypto (keccak256, sha256Hash)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Encoding (Encoding (..))
import Simplex.Messaging.Encoding.String (StrEncoding (..), TextEncoding (..))
import Simplex.Messaging.Eth.Address (Address, recoverAddress, unAddress)
import Simplex.Messaging.Eth.EIP712 (Eip712Domain (..), Eip712Value (..), hashTypedData)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON)
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexDomain (..), SimplexTLD (..), labelHash)
import Simplex.Messaging.SystemTime (RoundedSystemTime (..), SystemSeconds, roundedToUTCTime)
import Simplex.Messaging.Util (eitherToMaybe, tshow)
import StoreService.Names.Chain (AbiValue (..), Hex32 (..), abiArray, abiBytes, abiTuple, abiWordSize, addressWord, hex, lowerHexP, parseText, selector, uint256)

newtype RegistrationId = RegistrationId Text
  deriving (Eq, Ord, Show)

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

data LabelError = LELength | LEChars
  deriving (Eq, Show)

validLabel :: SimplexTLD -> Text -> Either LabelError Text
validLabel tld label
  | isNothing (nameLength $ T.length label) = Left LELength
  | otherwise = case strDecode (encodeUtf8 $ label <> decodeLatin1 (strEncode tld)) of
      Right SimplexDomain {domain, subDomain = []} | domain == label -> Right label
      _ -> Left LEChars

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
    riNonce :: Word64,
    riDeadline :: SystemSeconds,
    riSignature :: RecoverableSignature
  }

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
registerWithCreditCalldata RegisterIntent {riRegistration, riNonce, riDeadline, riSignature} =
  selector "registerWithCredit((string,address,uint256,bytes32,address,bytes[],uint8,bytes32),uint256,uint256,bytes)"
    <> abiTuple
      [ Dynamic $ registrationTuple riRegistration,
        Static $ uint256 riNonce,
        Static $ uint256 $ roundedSeconds riDeadline,
        Dynamic $ abiBytes $ smpEncode riSignature
      ]

registerIntentDigest :: Eip712Domain -> Registration -> Word64 -> SystemSeconds -> ByteString
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
      VUint $ fromIntegral $ roundedSeconds deadline
    ]

verifyRegisterIntent :: Eip712Domain -> UTCTime -> RegisterIntent -> IO (Either String ())
verifyRegisterIntent domain now RegisterIntent {riRegistration = r@Registration {rOwner}, riNonce, riDeadline, riSignature} = runExceptT $ do
  when (now > roundedToUTCTime riDeadline) $ throwError "intent expired"
  signer <- ExceptT $ recoverAddress riSignature $ registerIntentDigest domain r riNonce riDeadline
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

data AddressError
  = ADFormat
  | ADChecksum
  deriving (Eq, Show)

-- | The fields an owner signs in an order and in another name for it, with the terms the owner agreed to for that name.
data OrderFields = OrderFields
  { ofLabelHash :: LabelHash,
    ofLength :: NameLength,
    ofDuration :: Word64,
    ofCommitment :: Commitment,
    ofSealed :: Text,
    ofAgreedTerms :: AgreedTerms
  }
  deriving (Eq)

-- | The hash of the terms an order agreed to, the community terms for a name the policy lists.
data AgreedTerms = AgreedTerms {communityName :: CommunityName, termsHash :: TermsHash}
  deriving (Eq, Show)

-- | As the order signs it: the hash in 64 lowercase hex digits, after communityTermsPrefix for the community terms.
instance TextEncoding AgreedTerms where
  textEncode AgreedTerms {communityName, termsHash} = (if communityName then communityTermsPrefix else "") <> T.drop 2 (decodeLatin1 $ strEncode termsHash)
  textDecode = eitherToMaybe . parseText (AgreedTerms <$> (True <$ A.string (encodeUtf8 communityTermsPrefix) <|> pure False) <*> (Hex32 <$> lowerHexP abiWordSize))

instance FromField AgreedTerms where fromField = fromTextField_ textDecode

instance ToField AgreedTerms where toField = toField . textEncode

communityTermsPrefix :: Text
communityTermsPrefix = "community/"

parseSignature :: Text -> Either String RecoverableSignature
parseSignature = strDecode . encodeUtf8

-- | EIP-191 @personal_sign@ digest.
personalDigest :: ByteString -> ByteString
personalDigest msg = keccak256 $ "\x19\&Ethereum Signed Message:\n" <> BC.pack (show $ B.length msg) <> msg

orderMessage :: OrderFields -> Word64 -> ByteString
orderMessage order timestamp = message $ ["order"] <> orderWords order <> [tshow timestamp]

findMessage :: Address -> Word64 -> ByteString
findMessage owner timestamp = message ["find", hex $ unAddress owner, tshow timestamp]

retargetMessage :: RegistrationId -> OrderFields -> Word64 -> ByteString
retargetMessage (RegistrationId rid) order timestamp = message $ ["name", rid] <> orderWords order <> [tshow timestamp]

-- | The registration id is the order whose balance pays; the fields are the new order's.
spendMessage :: RegistrationId -> OrderFields -> Word64 -> ByteString
spendMessage (RegistrationId rid) order timestamp = message $ ["spend", rid] <> orderWords order <> [tshow timestamp]

message :: [Text] -> ByteString
message = encodeUtf8 . T.unwords . ("simplex.domains" :)

orderWords :: OrderFields -> [Text]
orderWords OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed, ofAgreedTerms} =
  [hexText $ Hex32 lh, tshow (nameLengthInt ofLength), tshow ofDuration, hexText ofCommitment, hexText $ Hex32 $ sha256Hash $ encodeUtf8 ofSealed, textEncode ofAgreedTerms]
  where
    hexText = decodeLatin1 . strEncode

parseChecksumAddress :: Text -> Either AddressError Address
parseChecksumAddress t = do
  a <- parseAnyCaseAddress t
  unless (decodeLatin1 (strEncode a) == t) $ Left ADChecksum
  pure a

parseAnyCaseAddress :: Text -> Either AddressError Address
parseAnyCaseAddress t
  | "0x" `T.isPrefixOf` t = first (const ADFormat) $ strDecode $ encodeUtf8 $ T.toLower t
  | otherwise = Left ADFormat

$(JQ.deriveJSON (enumJSON $ dropPrefix "LK") ''LinkKind)

$(JQ.deriveFromJSON defaultJSON ''TermsAcceptance)

$(JQ.deriveJSON defaultJSON ''AgreedTerms)
