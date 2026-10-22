{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Signatures
  ( SignatureError (..),
    AddressError (..),
    OrderFields (..),
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

import Control.Monad (unless, when)
import Crypto.Number.Serialize (os2ip)
import qualified Crypto.PubKey.ECC.Types as ECC
import Data.Bifunctor (first)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAsciiLower, isDigit, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Simplex.Messaging.Crypto (keccak256, sha256Hash)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature (..))
import Simplex.Messaging.Encoding.String (strDecode, strEncode)
import Simplex.Messaging.Eth.Address (Address, unAddress)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (tshow)
import StoreService.Names.Abi (hex)
import StoreService.Names.Phases (RegistrationId (..))

data SignatureError
  = SEFormat
  | SERecoveryId
  | SEHighS
  deriving (Eq, Show)

data AddressError
  = ADFormat
  | ADChecksum
  deriving (Eq, Show)

-- | The fields an owner signs in an order and in another name for it.
data OrderFields = OrderFields
  { ofLabelHash :: LabelHash,
    ofLength :: Int,
    ofDuration :: Word64,
    ofCommitment :: ByteString,
    ofSealed :: Text
  }
  deriving (Eq)

signatureHexLength :: Int
signatureHexLength = 130

scalarSize :: Int
scalarSize = 32

addressHexLength :: Int
addressHexLength = 40

-- the contracts' SignatureChecker refuses a higher s, though recovery accepts it
maxS :: Integer
maxS = ECC.ecc_n (ECC.common_curve $ ECC.getCurveByName ECC.SEC_p256k1) `div` 2

parseSignature :: Text -> Either SignatureError RecoverableSignature
parseSignature t = do
  h <- maybe (Left SEFormat) Right $ T.stripPrefix "0x" t
  unless (T.length h == signatureHexLength && T.all isLowerHex h) $ Left SEFormat
  sig <- first (const SEFormat) $ convertFromBase Base16 (encodeUtf8 h)
  let (rsR, (rsS, v)) = B.splitAt scalarSize <$> B.splitAt scalarSize sig
  rsRecId <- case B.unpack v of
    [27] -> Right 0
    [28] -> Right 1
    _ -> Left SERecoveryId
  when (os2ip rsS > maxS) $ Left SEHighS
  pure RecoverableSignature {rsR, rsS, rsRecId}
  where
    isLowerHex c = isDigit c || (isAsciiLower c && isHexDigit c)

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
orderWords OrderFields {ofLabelHash = LabelHash labelHash, ofLength, ofDuration, ofCommitment, ofSealed} =
  [hex labelHash, tshow ofLength, tshow ofDuration, hex ofCommitment, hex $ sha256Hash $ encodeUtf8 ofSealed]

-- the address parser checks EIP-55 only on mixed-case hex, so one-case text is refused here by comparing the encoding
parseChecksumAddress :: Text -> Either AddressError Address
parseChecksumAddress t = do
  a <- parseAnyCaseAddress t
  unless (strEncode a == encodeUtf8 t) $ Left ADChecksum
  pure a

parseAnyCaseAddress :: Text -> Either AddressError Address
parseAnyCaseAddress t = case T.stripPrefix "0x" t of
  Just h | T.length h == addressHexLength && T.all isHexDigit h -> first (const ADFormat) $ strDecode $ encodeUtf8 $ T.toLower h
  _ -> Left ADFormat
