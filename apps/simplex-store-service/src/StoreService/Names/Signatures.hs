{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Signatures
  ( SignatureError (..),
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
import Control.Monad (unless)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Simplex.Messaging.Agent.Store.DB (FromField (..), ToField (..), fromTextField_)
import Simplex.Messaging.Crypto (keccak256, sha256Hash)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature, deserializeSignature, lowS)
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import Simplex.Messaging.Eth.Address (Address, anyCaseAddressP, checksumAddressP, unAddress)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (eitherToMaybe, tshow)
import StoreService.Names.Abi (abiWordSize, hex, hexP, lowerHexP, parseText)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Registration (Commitment, CommunityName, TermsHash)

data SignatureError
  = SEFormat
  | SERecoveryId
  | SEHighS
  deriving (Eq, Show)

data AddressError
  = ADFormat
  | ADChecksum
  deriving (Eq, Show)

-- | The fields an owner signs in an order and in another name for it, with the terms the owner agreed to for that name.
data OrderFields = OrderFields
  { ofLabelHash :: LabelHash,
    ofLength :: Int,
    ofDuration :: Word64,
    ofCommitment :: Commitment,
    ofSealed :: Text,
    ofAgreedTerms :: AgreedTerms
  }
  deriving (Eq)

-- | The hash of the terms an order agreed to, the community terms for a name the policy lists.
data AgreedTerms = AgreedTerms {atCommunityName :: CommunityName, atTermsHash :: TermsHash}
  deriving (Eq, Show)

-- | As the order signs it: the hash in 64 lowercase hex digits, after communityTermsPrefix for the community terms.
instance TextEncoding AgreedTerms where
  textEncode AgreedTerms {atCommunityName, atTermsHash} = (if atCommunityName then communityTermsPrefix else "") <> T.drop 2 (hex atTermsHash)
  textDecode = eitherToMaybe . parseText (AgreedTerms <$> (True <$ A.string (encodeUtf8 communityTermsPrefix) <|> pure False) <*> lowerHexP abiWordSize)

instance FromField AgreedTerms where fromField = fromTextField_ textDecode

instance ToField AgreedTerms where toField = toField . textEncode

communityTermsPrefix :: Text
communityTermsPrefix = "community/"

signatureBytes :: Int
signatureBytes = 65

-- the contracts' SignatureChecker refuses a higher s, though recovery accepts it
parseSignature :: Text -> Either SignatureError RecoverableSignature
parseSignature t = do
  sig <- first (const SERecoveryId) . deserializeSignature =<< first (const SEFormat) (parseText (hexP signatureBytes) t)
  unless (lowS sig) $ Left SEHighS
  pure sig

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
orderWords OrderFields {ofLabelHash = LabelHash labelHash, ofLength, ofDuration, ofCommitment, ofSealed, ofAgreedTerms} =
  [hex labelHash, tshow ofLength, tshow ofDuration, hex ofCommitment, hex $ sha256Hash $ encodeUtf8 ofSealed, textEncode ofAgreedTerms]

parseChecksumAddress :: Text -> Either AddressError Address
parseChecksumAddress t = parseAnyCaseAddress t >> first (const ADChecksum) (parseText checksumAddressP t)

parseAnyCaseAddress :: Text -> Either AddressError Address
parseAnyCaseAddress = first (const ADFormat) . parseText anyCaseAddressP
