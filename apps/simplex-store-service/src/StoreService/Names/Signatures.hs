{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module StoreService.Names.Signatures
  ( AddressError (..),
    OrderFields (..),
    AgreedTerms (..),
    communityTermsPrefix,
    parseSignature,
    personalDigest,
    orderMessage,
    findMessage,
    selectionMessage,
    parseChecksumAddress,
    parseAnyCaseAddress,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import qualified Data.Aeson.TH as JQ
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Word (Word64)
import Simplex.Chat.Badges.Code (NameLength, nameLengthInt)
import Simplex.Messaging.Agent.Store.DB (FromField (..), ToField (..), fromTextField_)
import Simplex.Messaging.Crypto (keccak256, sha256Hash)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Encoding.String (StrEncoding (..), TextEncoding (..))
import Simplex.Messaging.Eth.Address (Address, unAddress)
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (eitherToMaybe, tshow)
import StoreService.Names.Abi (abiWordSize, hex, lowerHexP, parseText)
import StoreService.Names.Chain (Hex32 (..))
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Registration (Commitment, CommunityName, TermsHash)

data AddressError
  = ADFormat
  | ADChecksum
  deriving (Eq, Show)

-- | The fields an owner signs in an order and in the selection of a name for it, with the terms the owner agreed to for that name.
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
{-# INLINE parseSignature #-}

-- | EIP-191 @personal_sign@ digest.
personalDigest :: ByteString -> ByteString
personalDigest msg = keccak256 $ "\x19\&Ethereum Signed Message:\n" <> BC.pack (show $ B.length msg) <> msg

orderMessage :: OrderFields -> Word64 -> ByteString
orderMessage order timestamp = message $ ["order"] <> orderWords order <> [tshow timestamp]

findMessage :: Address -> Word64 -> ByteString
findMessage owner timestamp = message ["find", hex $ unAddress owner, tshow timestamp]
{-# INLINE findMessage #-}

selectionMessage :: RegistrationId -> OrderFields -> Word64 -> ByteString
selectionMessage (RegistrationId rid) order timestamp = message $ ["name", rid] <> orderWords order <> [tshow timestamp]

message :: [Text] -> ByteString
message = encodeUtf8 . T.unwords . ("simplex.domains" :)
{-# INLINE message #-}

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

$(JQ.deriveJSON defaultJSON ''AgreedTerms)
