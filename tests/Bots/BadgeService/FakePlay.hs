{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeService.FakePlay
  ( FakePlay (..),
    PlayAnswer (..),
    withFakePlay,
    answerPurchase,
    answerAcknowledgements,
    answerTokenRequests,
    grantTokensFor,
    fakePackageName,
  )
where

import BadgeService.Config (PlayStoreConfig (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Number.Serialize (i2osp)
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (Status, badRequest400, hAuthorization, hContentType, methodGet, methodPost, mkStatus, notFound404, ok200, parseSimpleQuery, unauthorized401)
import Network.Wai (Application, Response, pathInfo, requestHeaders, requestMethod, responseLBS, strictRequestBody)
import qualified Network.Wai.Handler.Warp as Warp
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import UnliftIO.Temporary (withTempDirectory)

data PlayAnswer = PlayRecord J.Value | PlayBody LB.ByteString | PlayStatus Int | PlayHang

data FakePlay = FakePlay
  { fpConfig :: PlayStoreConfig,
    fpAnswers :: TVar (M.Map Text PlayAnswer),
    -- a status the token endpoint answers instead of granting
    fpTokenRefusal :: TVar (Maybe Int),
    fpTokenSeconds :: TVar Int,
    fpGrantedTokens :: TVar [B.ByteString],
    fpPurchasePaths :: TVar [[Text]],
    fpAcknowledgeRefusal :: TVar (Maybe Int),
    fpAcknowledged :: TVar [Text]
  }

fakePackageName :: Text
fakePackageName = "chat.simplex.app"

fakeClientEmail :: Text
fakeClientEmail = "badge-verifier@simplex-badges-test.iam.gserviceaccount.com"

-- | Warp chooses the port, so a server left over from an earlier run cannot answer instead.
-- The service account key is made per run, since a committed one trips secret scanning.
withFakePlay :: (FakePlay -> IO a) -> IO a
withFakePlay action = do
  (key, privateKey) <- RSA.generate 128 65537
  fpAnswers <- newTVarIO M.empty
  fpTokenRefusal <- newTVarIO Nothing
  fpTokenSeconds <- newTVarIO 3600
  fpGrantedTokens <- newTVarIO []
  fpPurchasePaths <- newTVarIO []
  fpAcknowledgeRefusal <- newTVarIO Nothing
  fpAcknowledged <- newTVarIO []
  createDirectoryIfMissing True "tests/tmp"
  withTempDirectory "tests/tmp" "fake-play" $ \d -> do
    let accountFile = d </> "service-account.json"
        fake base =
          FakePlay
            { fpConfig = PlayStoreConfig {gPackageName = fakePackageName, gServiceAccountFile = accountFile, gApiHost = base, gTokenUrl = base <> "/token"},
              fpAnswers,
              fpTokenRefusal,
              fpTokenSeconds,
              fpGrantedTokens,
              fpPurchasePaths,
              fpAcknowledgeRefusal,
              fpAcknowledged
            }
    LB.writeFile accountFile $ J.encode $ J.object ["client_email" J..= fakeClientEmail, "private_key" J..= decodeUtf8 (pkcs8Pem privateKey)]
    Warp.testWithApplication (pure $ \req respond -> fakeApp key (fake $ origin req) req respond) $ \prt ->
      action (fake $ "http://127.0.0.1:" <> T.pack (show prt))
  where
    -- the app needs the base URL for the audience it checks, and the request names the port it reached
    origin req = maybe "" (("http://" <>) . T.pack . B8.unpack) (lookup "Host" (requestHeaders req))

-- | The PKCS#8 PEM Google puts in a service account key, so the service's own parser reads it.
pkcs8Pem :: RSA.PrivateKey -> B.ByteString
pkcs8Pem k =
  "-----BEGIN PRIVATE KEY-----\n" <> B8.unlines (chunks $ B64.encode pkcs8) <> "-----END PRIVATE KEY-----\n"
  where
    RSA.PublicKey {RSA.public_n, RSA.public_e} = RSA.private_pub k
    pkcs8 = der 0x30 $ derInteger 0 <> der 0x30 (rsaEncryption <> "\x05\x00") <> der 0x04 pkcs1
    pkcs1 = der 0x30 $ foldMap derInteger [0, public_n, public_e, RSA.private_d k, RSA.private_p k, RSA.private_q k, RSA.private_dP k, RSA.private_dQ k, RSA.private_qinv k]
    rsaEncryption = der 0x06 "\x2a\x86\x48\x86\xf7\x0d\x01\x01\x01"
    derInteger n = der 0x02 $ case i2osp n of
      bs | B.null bs || B.head bs >= 0x80 -> B.cons 0 bs
      bs -> bs
    der tag content = B.pack (tag : derLength (B.length content)) <> content
    derLength n
      | n < 0x80 = [fromIntegral n]
      | otherwise = let bs = B.unpack (i2osp $ toInteger n) in 0x80 + fromIntegral (length bs) : bs
    chunks bs
      | B.null bs = []
      | otherwise = let (line, rest) = B.splitAt 64 bs in line : chunks rest

answerPurchase :: FakePlay -> Text -> PlayAnswer -> IO ()
answerPurchase FakePlay {fpAnswers} token answer = atomically $ modifyTVar' fpAnswers $ M.insert token answer

answerTokenRequests :: FakePlay -> Maybe Int -> IO ()
answerTokenRequests FakePlay {fpTokenRefusal} = atomically . writeTVar fpTokenRefusal

grantTokensFor :: FakePlay -> Int -> IO ()
grantTokensFor FakePlay {fpTokenSeconds} = atomically . writeTVar fpTokenSeconds

answerAcknowledgements :: FakePlay -> Maybe Int -> IO ()
answerAcknowledgements FakePlay {fpAcknowledgeRefusal} = atomically . writeTVar fpAcknowledgeRefusal

fakeApp :: RSA.PublicKey -> FakePlay -> Application
fakeApp key FakePlay {fpConfig = PlayStoreConfig {gTokenUrl}, fpAnswers, fpTokenRefusal, fpTokenSeconds, fpGrantedTokens, fpPurchasePaths, fpAcknowledgeRefusal, fpAcknowledged} req respond =
  case (requestMethod req, pathInfo req) of
    (m, ["token"]) | m == methodPost -> strictRequestBody req >>= grant
    (m, path@["androidpublisher", "v3", "applications", pkg, "purchases", "products", _, "tokens", token])
      | m == methodGet && pkg == fakePackageName -> authorized $ do
          atomically $ modifyTVar' fpPurchasePaths (<> [path])
          purchase token
      | m == methodPost && pkg == fakePackageName, Just acknowledgedToken <- T.stripSuffix ":acknowledge" token -> authorized $ acknowledge acknowledgedToken
    _ -> respond $ status notFound404
  where
    authorized act = do
      granted <- readTVarIO fpGrantedTokens
      case lookup hAuthorization (requestHeaders req) of
        Just auth | Just bearer <- B.stripPrefix "Bearer " auth, take 1 (reverse granted) == [bearer] -> act
        _ -> respond $ status unauthorized401
    acknowledge token =
      readTVarIO fpAcknowledgeRefusal >>= \case
        Just code -> respond $ status (mkStatus code "Refused")
        Nothing -> do
          atomically $ modifyTVar' fpAcknowledged (<> [token])
          respond $ responseLBS ok200 [] ""
    grant body =
      readTVarIO fpTokenRefusal >>= \case
        Just code -> respond $ status (mkStatus code "Refused")
        Nothing
          | validAssertion (parseSimpleQuery $ LB.toStrict body) -> do
              bearer <- atomically $ stateTVar fpGrantedTokens $ \ts -> let t = "fake-access-" <> B8.pack (show (length ts)) in (t, ts <> [t])
              seconds <- readTVarIO fpTokenSeconds
              respond $ json ok200 $ J.object ["access_token" J..= B8.unpack bearer, "expires_in" J..= seconds, "token_type" J..= ("Bearer" :: Text)]
          | otherwise -> respond $ json badRequest400 $ J.object ["error" J..= ("invalid_grant" :: Text)]
    validAssertion form = case (lookup "grant_type" form, B8.split '.' <$> lookup "assertion" form) of
      (Just "urn:ietf:params:oauth:grant-type:jwt-bearer", Just [header, claims, sig])
        | Right sigBytes <- B64U.decodeUnpadded sig,
          RSA.verify (Just SHA256) key (header <> "." <> claims) sigBytes,
          Right claimBytes <- B64U.decodeUnpadded claims,
          Just (J.Object c) <- J.decodeStrict' claimBytes ->
            KM.lookup "aud" c == Just (J.String gTokenUrl)
              && KM.lookup "scope" c == Just (J.String "https://www.googleapis.com/auth/androidpublisher")
              && KM.lookup "iss" c == Just (J.String fakeClientEmail)
      _ -> False
    purchase token =
      M.lookup token <$> readTVarIO fpAnswers >>= \case
        Just (PlayRecord v) -> respond $ json ok200 v
        Just (PlayBody b) -> respond $ responseLBS ok200 [(hContentType, "application/json")] b
        Just (PlayStatus code) -> respond $ status (mkStatus code "Answer")
        Just PlayHang -> forever $ threadDelay 1000000
        Nothing -> respond $ status notFound404

json :: Status -> J.Value -> Response
json st = responseLBS st [(hContentType, "application/json")] . J.encode

status :: Status -> Response
status st = json st $ J.object ["error" J..= J.object ["code" J..= show st]]
