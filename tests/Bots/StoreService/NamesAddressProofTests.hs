{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesAddressProofTests
  ( namesAddressProofTests,
    devBitcoinXpub,
    devBitcoinHrp,
    devMoneroAddress,
    devMoneroViewKey,
    devBitcoinKey,
    devMoneroKey,
    devBitcoinAddresses,
    stagenetSubaddresses,
    addressProofHolds,
  )
where

import ChatTests.Utils (writeFixture)
import Control.Monad (forM, forM_, guard)
import Crypto.Error (maybeCryptoError, throwCryptoError)
import qualified Crypto.ECC.Edwards25519 as Ed
import Crypto.Number.ModArithmetic (expFast)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.PubKey.ECC.Types as ECC
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Either (isLeft)
import Data.List (elemIndex, foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Word (Word32)
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import StoreService.Providers.AddressProof
import Test.Hspec

namesAddressProofTests :: Spec
namesAddressProofTests = describe "StoreService names: address proofs" $ do
  describe "Bitcoin" $ do
    it "derives m/0H/1 from the xpub of m/0H, BIP-32 test vector 1" testBip32Vector1
    it "encodes the P2WPKH addresses of BIP-173's public key" testBip173
    it "derives the receiving addresses of BIP-84's account from its zpub" testBip84
    it "proves each receiving address of the dev account with its tweak" testDevBitcoin
    it "refuses another network's, kind's or checksum's extended key, a scheme with a suffix and another prefix" testBitcoinKeyRefusals
    it "refuses a key path outside the receiving branch" testKeyPathRefusals
  describe "Monero" $ do
    it "derives a stagenet wallet's subaddresses from its view key, as monero-python's published vectors" testStagenetSubaddresses
    it "proves the view key of each subaddress, a proof that a changed C or spend tweak fails" testViewProof
    it "refuses a subaddress, a bad checksum, an unreduced view key and another wallet's view key" testMoneroKeyRefusals
  it "stores each proof as text and reads it back" testProofText
  it "answers each proof as the names page reads it" testProofJSON
  it "records the vectors and the dev keys' values the names page holds" testFixtures

devBitcoinXpub, devBitcoinHrp, devMoneroAddress, devMoneroViewKey :: Text
devBitcoinXpub = "tpubDC8msFGeGuwnKG9Upg7DM2b4DaRqg3CUZa5g8v2SRQ6K4NSkxUgd7HsL2XVWbVm39yBA4LAxysQAm397zwQSQoQgewGiYZqrA9DsP4zbQ1M"
devBitcoinHrp = "bcrt"
devMoneroAddress = "52jzuBBUMty3xPL3JsQxGP74LDuV6E1LS8Zda1PbdqQjGzFmH6N9ep9McbFKMALujVT9S5mKpbEgC5VPhfoAiVj8LdAqbp6"
devMoneroViewKey = "fd5c0d25f8f994268079a4f7844274dc870a7c2b88fbfc24ba318375e1d9430f"

devBitcoinKey :: BitcoinKey
devBitcoinKey = either error id $ bitcoinKey devBitcoinXpub devBitcoinHrp

devMoneroKey :: MoneroKey
devMoneroKey = either error id $ moneroKey devMoneroAddress devMoneroViewKey

devReceiveKey :: ByteString
devReceiveKey = unhex "036bf340760eb0da07d4d2bbb9d9aee7b7c27891cb72b6388cc0ee8d2a82d33e46"

devKeyPaths :: [Text]
devKeyPaths = map fst devBitcoinAddresses

devBitcoinAddresses :: [(Text, Text)]
devBitcoinAddresses =
  [ ("0/0", "bcrt1q6rz28mcfaxtmd6v789l9rrlrusdprr9pz3cppk"),
    ("0/1", "bcrt1qd7spv5q28348xl4myc8zmh983w5jx32cs707jh"),
    ("0/2", "bcrt1qxdyjf6h5d6qxap4n2dap97q4j5ps6ua8jkxz0z"),
    ("0/3", "bcrt1qynpgs6wap6h9uvy7j0xlesew2w82qn039tzepj")
  ]

devSubaddresses :: [(Word32, Word32)]
devSubaddresses = [(0, 1), (0, 2), (0, 3), (1, 0)]

bip32Parent :: Text
bip32Parent = "xpub68Gmy5EdvgibQVfPdqkBBCHxA5htiqg55crXYuXoQRKfDBFA1WEjWgP6LHhwBZeNK1VTsfTFUHCdrfp1bgwQ9xv5ski8PX9rL2dZXvgGDnw"

bip173Key :: ByteString
bip173Key = unhex "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"

bip173Addresses :: [(Text, Text)]
bip173Addresses = [("bc", "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"), ("tb", "tb1qw508d6qejxtdg4y5r3zarvary0c5xw7kxpjzsx")]

bip84Zpub, bip84Xpub :: Text
bip84Zpub = "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs"
bip84Xpub = "xpub6CatWdiZiodmUeTDp8LT5or8nmbKNcuyvz7WyksVFkKB4RHwCD3XyuvPEbvqAQY3rAPshWcMLoP2fMFMKHPJ4ZeZXYVUhLv1VMrjPC7PW6V"

bip84Key :: BitcoinKey
bip84Key = either error id $ bitcoinKey bip84Xpub "bc"

xpubVersion, zpubVersion :: ByteString
xpubVersion = "\x04\x88\xB2\x1E"
zpubVersion = "\x04\xB2\x47\x46"

stagenetSubaddresses :: [((Word32, Word32), Text)]
stagenetSubaddresses =
  [ ((0, 1), "7AeQwvrLtPeYoXVPRkEu8oEL7N9wnqHjYKwSvTf6YKbHgYmw6AJMsjggzVLo21egMK9qcoV1mxCTfP4FbaGb7JEMDfpLetk"),
    ((0, 2), "73YMtNiQmazP56aYSAR2SjT6uCfCNEBFnfYc4PHUUJaqC8urLbipC1Z3NUUqoraQV4PJmLRHV81cnadnoUAcLiqD8jQB4mL"),
    ((0, 3), "7Ab6cPPwU97LD8HwX1X8QSJGLtkLq4vYHG5RFYiLXAvvX74yHoMxPGYXPchEmeQaVGjT72mXBYouNA8HLSvnxdP9LD931f3"),
    ((1, 0), "73WqhTXVZM6Rq9TLi2EQHGK5jwopSRNHheT6JXCfmg9DVPps4um6En5Yn1RMcKezSM3iyUZYDEnEdamWPABSpRS1Eq7gnsr")
  ]

mainnetAddress, mainnetViewKey :: Text
mainnetAddress = "47ewoP19TN7JEEnFKUJHAYhGxkeTRH82sf36giEp9AcNfDBfkAtRLX7A6rZz18bbNHPNV7ex6WYbMN3aKisFRJZ8Ebsmgef"
mainnetViewKey = "6d9056aa2c096bfcd2f272759555e5764ba204dd362604a983fa3e0aafd35901"

testBip32Vector1 :: IO ()
testBip32Vector1 = do
  Right parent <- pure $ extendedKey xpubVersion bip32Parent
  Just (_, child@ExtendedKey {xkChainCode}) <- pure $ childKey parent 1
  compressedKey child `shouldBe` unhex "03501e454bf00751f24b1b489aa925215d66af2234e3891c3b21a52bedb3cd711c"
  xkChainCode `shouldBe` unhex "2a7857631386ba23dacac34180dd1983734e444fdbf774041578e9b6adb37c19"
  childKey parent 0x80000000 `shouldBe` Nothing

testBip173 :: IO ()
testBip173 = forM_ bip173Addresses $ \(hrp, address) -> p2wpkhAddress hrp bip173Key `shouldBe` address

testBip84 :: IO ()
testBip84 = do
  Right account <- pure $ extendedKey zpubVersion bip84Zpub
  Just (_, receive) <- pure $ childKey account 0
  map (fmap (p2wpkhAddress "bc" . compressedKey . snd) . childKey receive) [0, 1]
    `shouldBe` [Just "bc1qcr8te4kr609gcawutmrza0j4xv80jy8z306fyu", Just "bc1qnjg0jd8228aq7egyzacy8cys3knf9xvrerkf9g"]
  map (fmap fst . bitcoinPayee bip84Key) ["0/0", "0/1"]
    `shouldBe` [Right "bc1qcr8te4kr609gcawutmrza0j4xv80jy8z306fyu", Right "bc1qnjg0jd8228aq7egyzacy8cys3knf9xvrerkf9g"]

testDevBitcoin :: IO ()
testDevBitcoin = do
  receiveKeyText devBitcoinKey `shouldBe` "0x036bf340760eb0da07d4d2bbb9d9aee7b7c27891cb72b6388cc0ee8d2a82d33e46"
  map (fmap fst . bitcoinPayee devBitcoinKey) devKeyPaths `shouldBe` map (Right . snd) devBitcoinAddresses
  fmap fst (bitcoinKey devBitcoinXpub "tb" >>= (`bitcoinPayee` "0/0")) `shouldBe` Right "tb1q6rz28mcfaxtmd6v789l9rrlrusdprr9pqcpvkl"
  forM_ devKeyPaths $ \keyPath -> do
    Right (address, proof) <- pure $ bitcoinPayee devBitcoinKey keyPath
    addressProofHolds address proof `shouldBe` True
    addressProofHolds (T.replace "bcrt1q" "bcrt1p" address) proof `shouldBe` False

testBitcoinKeyRefusals :: IO ()
testBitcoinKeyRefusals = do
  let refused xpub hrp = isLeft (bitcoinKey xpub hrp) `shouldBe` True
  refused bip84Xpub "bcrt"
  refused devBitcoinXpub "bc"
  refused (devBitcoinXpub <> "-[legacy]") devBitcoinHrp
  refused (devBitcoinXpub <> "-[p2sh]") devBitcoinHrp
  refused bip84Zpub "bc"
  refused (T.init devBitcoinXpub <> "N") devBitcoinHrp
  refused devBitcoinXpub "ltc"
  refused "" devBitcoinHrp

testKeyPathRefusals :: IO ()
testKeyPathRefusals = forM_ ["1/0", "0/2147483648", "0/4294967296", "m/0/1", "0/1/2", "0/", "0/x", "0/-1", ""] $ \keyPath ->
  bitcoinPayee devBitcoinKey keyPath `shouldSatisfy` isLeft

testStagenetSubaddresses :: IO ()
testStagenetSubaddresses = do
  primaryAddress devMoneroKey `shouldBe` devMoneroAddress
  subaddressPrefix devMoneroKey `shouldBe` 36
  forM_ stagenetSubaddresses $ \((i, j), address) -> fst (moneroPayee devMoneroKey i j) `shouldBe` address

testViewProof :: IO ()
testViewProof = forM_ devSubaddresses $ \(i, j) -> do
  let (address, proof) = moneroPayee devMoneroKey i j
      (spendD, viewC) = moneroKeys address
      changedC = Ed.pointEncode $ Ed.pointAdd (edPoint viewC) (Ed.toPoint $ edScalar "\x01")
  addressProofHolds address proof `shouldBe` True
  addressProofHolds (moneroText 36 spendD changedC) proof `shouldBe` False
  case proof of
    APMonero {spendTweak, viewProof} -> addressProofHolds address APMonero {spendTweak = B.reverse spendTweak, viewProof} `shouldBe` False
    APBitcoin {} -> expectationFailure "a Monero proof"

testMoneroKeyRefusals :: IO ()
testMoneroKeyRefusals = do
  let refused address viewKey = isLeft (moneroKey address viewKey) `shouldBe` True
  forM_ stagenetSubaddresses $ \(_, address) -> refused address devMoneroViewKey
  refused (T.init devMoneroAddress <> "7") devMoneroViewKey
  refused devMoneroAddress mainnetViewKey
  refused devMoneroAddress (T.replicate 64 "f")
  refused devMoneroAddress (T.toUpper devMoneroViewKey)
  refused mainnetAddress devMoneroViewKey
  refused "" devMoneroViewKey
  subaddressPrefix <$> moneroKey mainnetAddress mainnetViewKey `shouldBe` Right 42

devProofs :: [AddressProof]
devProofs = [snd $ either error id $ bitcoinPayee devBitcoinKey "0/1", snd $ moneroPayee devMoneroKey 0 1]

testProofText :: IO ()
testProofText = forM_ devProofs $ \proof -> textDecode (textEncode proof) `shouldBe` Just proof

testProofJSON :: IO ()
testProofJSON =
  map J.toJSON devProofs
    `shouldBe` [ J.object ["type" .= ("btc" :: Text), "keyTweak" .= ("0c45f0820be2ab1d790510970aab4faab00b6ea23f0f8224723d7e078dcdfa5a" :: Text)],
                 J.object
                   [ "type" .= ("xmr" :: Text),
                     "spendTweak" .= ("a3ef01217924f949738026a9803be31c45e1c2806f41224136bb971a8c5e740f" :: Text),
                     "viewProof" .= J.object ["challenge" .= ("437e7ad7b7951cdb43faa898a9a72b159d55b732b0a967498c79148db82a1f0f" :: Text), "response" .= ("259d290075af76c3a8f80217e53c4eaaebc254415a8ed5bde5e6af3db4347708" :: Text)]
                   ]
               ]

testFixtures :: IO ()
testFixtures = do
  bip84 <- forM ["0/0", "0/1"] $ bitcoinVector bip84Key
  dev <- forM devKeyPaths $ bitcoinVector devBitcoinKey
  xmr <- forM devSubaddresses moneroVector
  Right parent <- pure $ extendedKey xpubVersion bip32Parent
  Just (_, child@ExtendedKey {xkChainCode}) <- pure $ childKey parent 1
  let bitcoinSection key xpub hrp invoices = J.object ["hrp" .= (hrp :: Text), "invoices" .= invoices, "receiveKey" .= receiveKeyText key, "xpub" .= (xpub :: Text)]
  writeFixture "store-service/rest-api/vectors-address-proofs" $
    J.object
      [ "bitcoin"
          .= J.object
            [ "bip173" .= [J.object ["address" .= p2wpkhAddress hrp bip173Key, "hrp" .= hrp, "publicKey" .= hexText bip173Key] | (hrp, _) <- bip173Addresses],
              "bip32" .= J.object ["chainCode" .= hexText xkChainCode, "index" .= (1 :: Int), "publicKey" .= hexText (compressedKey child), "xpub" .= bip32Parent],
              "bip84" .= bitcoinSection bip84Key bip84Xpub "bc" bip84,
              "dev" .= bitcoinSection devBitcoinKey devBitcoinXpub devBitcoinHrp dev
            ],
        "monero" .= J.object ["dev" .= J.object ["invoices" .= xmr, "primaryAddress" .= primaryAddress devMoneroKey, "subaddressPrefix" .= subaddressPrefix devMoneroKey, "viewKey" .= devMoneroViewKey]]
      ]
  writeFixture "store-service/rest-api/deployment" $
    J.object
      [ "bitcoinHrp" .= devBitcoinHrp,
        "bitcoinReceiveKeys" .= [receiveKeyText devBitcoinKey],
        "moneroPrimaryAddresses" .= [primaryAddress devMoneroKey],
        "moneroSubaddressPrefix" .= subaddressPrefix devMoneroKey
      ]
  where
    bitcoinVector key keyPath = case bitcoinPayee key keyPath of
      Right (address, APBitcoin {keyTweak}) -> pure $ J.object ["address" .= address, "keyPath" .= keyPath, "keyTweak" .= hexDigits keyTweak]
      other -> fail $ "a Bitcoin proof: " <> show other
    moneroVector (i, j) = case moneroPayee devMoneroKey i j of
      (address, APMonero {spendTweak, viewProof = ViewProof {challenge, response}}) ->
        pure $ J.object ["accountIndex" .= i, "address" .= address, "addressIndex" .= j, "spendTweak" .= hexDigits spendTweak, "viewProof" .= J.object ["challenge" .= hexDigits challenge, "response" .= hexDigits response]]
      other -> fail $ "a Monero proof: " <> show other

addressProofHolds :: Text -> AddressProof -> Bool
addressProofHolds address = \case
  APBitcoin {keyTweak} -> fromMaybe False $ do
    w <- secpPoint devReceiveKey
    let t = os2ip keyTweak
    guard $ B.length keyTweak == 32 && t < secpOrder
    pure $ p2wpkhAddress devBitcoinHrp (secpBytes $ ECC.pointAdd secp256k1 (ECC.pointBaseMul secp256k1 t) w) == address
  APMonero {spendTweak, viewProof = ViewProof {challenge, response}} -> fromMaybe False $ do
    let (spendB, viewA) = moneroKeys devMoneroAddress
        (spendD, viewC) = moneroKeys address
    [b, a, d, c] <- mapM (maybeCryptoError . Ed.pointDecode) [spendB, viewA, spendD, viewC]
    [m, e, s] <- mapM canonical [spendTweak, challenge, response]
    let r1 = Ed.pointAdd (Ed.toPoint s) (Ed.pointMul e a)
        r2 = Ed.pointAdd (Ed.pointMul s d) (Ed.pointMul e c)
        digest = keccak256 $ "simplex.domains view proof" <> viewA <> spendD <> viewC <> Ed.pointEncode r1 <> Ed.pointEncode r2
    pure $
      Ed.pointEncode (Ed.pointAdd b (Ed.toPoint m)) == spendD
        && Ed.pointHasPrimeOrder c
        && Ed.scalarEncode (edScalar digest) == challenge
  where
    canonical bytes = let s = edScalar bytes in if Ed.scalarEncode s == bytes then Just s else Nothing

secp256k1 :: ECC.Curve
secp256k1 = ECC.getCurveByName ECC.SEC_p256k1

secpOrder :: Integer
secpOrder = ECC.ecc_n $ ECC.common_curve secp256k1

secpPrime :: Integer
secpPrime = 2 ^ (256 :: Int) - 2 ^ (32 :: Int) - 977

secpPoint :: ByteString -> Maybe ECC.Point
secpPoint bytes = do
  (prefix, xBytes) <- B.uncons bytes
  let x = os2ip xBytes
      y = expFast ((x * x * x + 7) `mod` secpPrime) ((secpPrime + 1) `div` 4) secpPrime
  guard $ prefix `elem` [2, 3] && B.length xBytes == 32
  pure $ ECC.Point x $ if odd y == (prefix == 3) then y else secpPrime - y

secpBytes :: ECC.Point -> ByteString
secpBytes = \case
  ECC.Point x y -> B.cons (if odd y then 3 else 2) (i2ospOf_ 32 x)
  ECC.PointO -> B.empty

edScalar :: ByteString -> Ed.Scalar
edScalar = throwCryptoError . Ed.scalarDecodeLong

edPoint :: ByteString -> Ed.Point
edPoint = throwCryptoError . Ed.pointDecode

moneroKeys :: Text -> (ByteString, ByteString)
moneroKeys address = B.splitAt 32 $ B.take 64 $ B.drop 1 $ moneroDecode address

moneroText :: Int -> ByteString -> ByteString -> Text
moneroText prefix spend view = T.concat $ map encodedBlock $ chunks $ body <> B.take 4 (keccak256 body)
  where
    body = B.cons (fromIntegral prefix) (spend <> view)
    chunks bytes = if B.null bytes then [] else B.take 8 bytes : chunks (B.drop 8 bytes)
    encodedBlock bytes = T.pack [BC.index base58Alphabet (fromInteger $ n `mod` 58) | n <- reverse $ take (blockSizes !! B.length bytes) $ iterate (`div` 58) $ os2ip bytes]

moneroDecode :: Text -> ByteString
moneroDecode t
  | T.null t = B.empty
  | otherwise = i2ospOf_ size value <> moneroDecode rest
  where
    (chunk, rest) = T.splitAt 11 t
    size = fromMaybe 0 $ T.length chunk `elemIndex` blockSizes
    value = foldl' (\acc ch -> acc * 58 + maybe 0 toInteger (BC.elemIndex ch base58Alphabet)) 0 (T.unpack chunk)

base58Alphabet :: ByteString
base58Alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

blockSizes :: [Int]
blockSizes = [0, 2, 3, 5, 6, 7, 9, 10, 11]

unhex :: Text -> ByteString
unhex t = either error id $ convertFromBase Base16 $ encodeUtf8 $ fromMaybe t $ T.stripPrefix "0x" t

hexDigits :: ByteString -> Text
hexDigits = decodeLatin1 . convertToBase Base16

hexText :: ByteString -> Text
hexText = ("0x" <>) . hexDigits
