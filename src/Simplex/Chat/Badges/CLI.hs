{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Offline operator tooling for supporter badges, invoked as `simplex-chat badge ...`.
--   keygen     - the issuer keypair (the "secret" signs; the "public" is hardcoded into the app config).
--   master-key - the user's master secret (their unlinkability secret; generated client-side in the real flow).
--   sign       - bind a user master secret to a badge with the issuer secret, printed as one-line JSON for `/badge add`.
module Simplex.Chat.Badges.CLI (runBadgeCommand) where

import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Options.Applicative
import Simplex.Chat.Badges
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (BBSPublicKey (..), BBSSecretKey (..), bbsKeyGen)
import Simplex.Messaging.Encoding.String (strDecode, strEncode, textDecode)
import System.Exit (die)

-- the issuer keypair is secret (32 bytes) <> public (96 bytes).
bbsSecretLen, bbsPublicLen :: Int
bbsSecretLen = 32
bbsPublicLen = 96

data BadgeCommand
  = Keygen
  | MasterKey
  | Sign Int BBSSecretKey BBSPublicKey BadgeMasterKey BadgeType (Maybe UTCTime)

runBadgeCommand :: [String] -> IO ()
runBadgeCommand args =
  handleParseResult (execParserPure defaultPrefs badgeInfo args) >>= \case
    Keygen -> keygen
    MasterKey -> genMasterKey
    Sign keyIdx sk pk ms badgeType badgeExpiry -> sign keyIdx sk pk ms badgeType badgeExpiry
  where
    badgeInfo = info (helper <*> hsubparser badgeCmd) fullDesc
    badgeCmd = command "badge" (info (helper <*> badgeCommandP) (progDesc "SimpleX supporter badge tooling"))

badgeCommandP :: Parser BadgeCommand
badgeCommandP =
  hsubparser $
    command "keygen" (info (pure Keygen) (progDesc "generate an issuer keypair (issuer secret + public, base64url)"))
      <> command "master-key" (info (pure MasterKey) (progDesc "generate a user master secret (base64url)"))
      <> command "sign" (info signP (progDesc "sign a badge for a user master secret, printed as one-line JSON"))
  where
    signP =
      (\keyIdx (sk, pk) -> Sign keyIdx sk pk)
        <$> option auto (long "key-idx" <> metavar "KEY_IDX" <> help "index of the issuer key in the app config")
        <*> option (eitherReader issuerKeyR) (long "secret" <> metavar "ISSUER_KEY" <> help "issuer keypair from keygen (base64url)")
        <*> option (eitherReader (strDecode . B.pack)) (long "master" <> metavar "MASTER" <> help "user master secret from master-key (base64url)")
        <*> option (eitherReader badgeTypeR) (long "type" <> metavar "TYPE" <> help "badge type (supporter, legend, investor)")
        <*> option (eitherReader expireR) (long "expire" <> metavar "lifetime|YYYY-MM-DD" <> help "expiry date, or 'lifetime'")
    issuerKeyR s = do
      kp <- strDecode (B.pack s)
      if B.length kp == bbsSecretLen + bbsPublicLen
        then let (sk, pk) = B.splitAt bbsSecretLen kp in Right (BBSSecretKey sk, BBSPublicKey pk)
        else Left "bad issuer key - use the 'secret' value from keygen"
    badgeTypeR = maybe (Left "invalid badge type") Right . textDecode . T.pack
    expireR = \case
      "lifetime" -> Right Nothing
      s -> maybe (Left "use 'lifetime' or YYYY-MM-DD") (Right . Just) $ parseTimeM True defaultTimeLocale "%Y-%m-%d" s

keygen :: IO ()
keygen =
  bbsKeyGen >>= \case
    Left e -> die $ "keygen failed: " <> e
    Right (BBSSecretKey sk, BBSPublicKey pk) -> do
      B.putStrLn $ "secret " <> strEncode (sk <> pk)
      B.putStrLn $ "public " <> strEncode pk

genMasterKey :: IO ()
genMasterKey = do
  drg <- C.newRandom
  mk <- generateMasterKey drg
  B.putStrLn $ strEncode mk

sign :: Int -> BBSSecretKey -> BBSPublicKey -> BadgeMasterKey -> BadgeType -> Maybe UTCTime -> IO ()
sign keyIdx secretKey publicKey masterKey badgeType badgeExpiry = do
  let req = VerifiedBadgeRequest (BadgeRequest {masterKey, badgeInfo = BadgeInfo {badgeType, badgeExpiry, badgeExtra = ""}} :: BadgeRequest)
  issueBadge keyIdx secretKey publicKey req >>= \case
    Left e -> die $ "sign failed: " <> e
    -- single-line JSON (master secret + signature + info), pasted into the app via `/badge add`
    Right cred -> LB.putStrLn $ J.encode cred
