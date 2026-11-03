{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeService.FakeAppStore
  ( FakeAppStore (..),
    AppStoreAnswer (..),
    withFakeAppStore,
    answerStatus,
    writeApiKey,
  )
where

import BadgeService.Config (AppleServerApi (..))
import Bots.BadgeService.FakePlay (der, pem)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types (Status, hAuthorization, hContentType, methodGet, mkStatus, notFound404, ok200, unauthorized401)
import Network.Wai (Application, Response, pathInfo, requestHeaders, requestMethod, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import UnliftIO.Temporary (withTempDirectory)

data AppStoreAnswer = AppStoreBody J.Value | AppStoreStatus Int | AppStoreHang

data FakeAppStore = FakeAppStore
  { fasApi :: AppleServerApi,
    fasAnswers :: TVar (M.Map Text AppStoreAnswer),
    -- the bearer token of every request the fake accepted, in order
    fasTokens :: TVar [B.ByteString]
  }

fakeKeyId, fakeIssuerId :: Text
fakeKeyId = "2X9R4HXF34"
fakeIssuerId = "57246542-96fe-1a63-e053-0824d011072a"

-- | The App Store Server API for the bundle id given, accepting only tokens its own key signed. The key
-- is made per run, as Play's is, and written as the .p8 file App Store Connect hands out.
withFakeAppStore :: Text -> (FakeAppStore -> IO a) -> IO a
withFakeAppStore bundleId action = do
  fasAnswers <- newTVarIO M.empty
  fasTokens <- newTVarIO []
  createDirectoryIfMissing True "tests/tmp"
  withTempDirectory "tests/tmp" "fake-app-store" $ \d -> do
    let keyFile = d </> "AuthKey.p8"
    publicKey <- writeApiKey keyFile
    let fake base =
          FakeAppStore
            { fasApi = AppleServerApi {aKeyId = fakeKeyId, aIssuerId = fakeIssuerId, aPrivateKeyFile = keyFile, aApiHost = base},
              fasAnswers,
              fasTokens
            }
    Warp.testWithApplication (pure $ fakeApp bundleId publicKey (fake "")) $ \prt ->
      action $ fake $ "http://127.0.0.1:" <> T.pack (show prt)

-- | A new P-256 key, as the PKCS#8 PEM of an App Store Connect .p8 file; answers its public half.
writeApiKey :: FilePath -> IO ECDSA.PublicKey
writeApiKey path = do
  (publicKey@(ECDSA.PublicKey _ q), ECDSA.PrivateKey _ d) <- ECC.generate $ ECC.getCurveByName ECC.SEC_p256r1
  case q of
    ECC.Point x y -> do
      let ecPrivateKey = der 0x30 $ der 0x02 "\x01" <> der 0x04 (i2ospOf_ 32 d) <> der 0xa1 (der 0x03 $ "\x00\x04" <> i2ospOf_ 32 x <> i2ospOf_ 32 y)
          algorithm = der 0x30 $ der 0x06 "\x2a\x86\x48\xce\x3d\x02\x01" <> der 0x06 "\x2a\x86\x48\xce\x3d\x03\x01\x07"
      B.writeFile path $ pem "PRIVATE KEY" $ der 0x30 $ der 0x02 "\x00" <> algorithm <> der 0x04 ecPrivateKey
      pure publicKey
    ECC.PointO -> writeApiKey path

answerStatus :: FakeAppStore -> Text -> AppStoreAnswer -> IO ()
answerStatus FakeAppStore {fasAnswers} originalId answer = atomically $ modifyTVar' fasAnswers $ M.insert originalId answer

fakeApp :: Text -> ECDSA.PublicKey -> FakeAppStore -> Application
fakeApp bundleId publicKey FakeAppStore {fasAnswers, fasTokens} req respond =
  case (requestMethod req, pathInfo req) of
    (m, ["inApps", "v1", "subscriptions", originalId]) | m == methodGet -> do
      now <- round <$> getPOSIXTime
      case lookup hAuthorization (requestHeaders req) >>= B.stripPrefix "Bearer " of
        Just bearer | validToken now bearer -> do
          atomically $ modifyTVar' fasTokens (<> [bearer])
          answer originalId
        _ -> respond $ status unauthorized401
    _ -> respond $ status notFound404
  where
    answer originalId =
      M.lookup originalId <$> readTVarIO fasAnswers >>= \case
        Just (AppStoreBody v) -> respond $ responseLBS ok200 [(hContentType, "application/json")] $ J.encode v
        Just (AppStoreStatus code) -> respond $ status (mkStatus code "Answer")
        Just AppStoreHang -> forever $ threadDelay 1000000
        Nothing -> respond $ status notFound404
    -- Apple's rules: ES256 with this key's id, and at most an hour between issue and expiry
    validToken :: Integer -> B.ByteString -> Bool
    validToken now token = case B8.split '.' token of
      [header, claims, signature]
        | Right sig <- B64U.decodeUnpadded signature,
          B.length sig == 64,
          (r, s) <- B.splitAt 32 sig,
          ECDSA.verify SHA256 publicKey (ECDSA.Signature (os2ip r) (os2ip s)) (header <> "." <> claims),
          Just h <- part header,
          Just c <- part claims,
          Just iat <- number "iat" c,
          Just expiry <- number "exp" c ->
            KM.lookup "alg" h == Just "ES256"
              && KM.lookup "kid" h == Just (J.String fakeKeyId)
              && KM.lookup "typ" h == Just "JWT"
              && KM.lookup "iss" c == Just (J.String fakeIssuerId)
              && KM.lookup "aud" c == Just "appstoreconnect-v1"
              && KM.lookup "bid" c == Just (J.String bundleId)
              && iat <= now
              && now < expiry
              && expiry - iat <= 3600
      _ -> False
    part p = case J.decodeStrict' =<< either (const Nothing) Just (B64U.decodeUnpadded p) of
      Just (J.Object o) -> Just o
      _ -> Nothing
    number k o = case KM.lookup k o of
      Just v | J.Success n <- J.fromJSON v -> Just n
      _ -> Nothing

status :: Status -> Response
status st = responseLBS st [(hContentType, "application/json")] $ J.encode $ J.object ["errorMessage" J..= show st]
