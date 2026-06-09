{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Offline operator tooling for supporter badges, invoked as `simplex-chat badge ...`.
-- `keygen` prints a base64url keypair (the public key is hardcoded into the app config);
-- `sign` mints a credential as one-line JSON to paste into the app via `/badge add`.
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
import Simplex.Messaging.Crypto.BBS (BBSPublicKey, BBSSecretKey, bbsKeyGen)
import Simplex.Messaging.Encoding.String (strDecode, strEncode, textDecode)
import System.Exit (die)

data BadgeCommand
  = Keygen
  | Sign BBSSecretKey BBSPublicKey BadgeType (Maybe UTCTime)

runBadgeCommand :: [String] -> IO ()
runBadgeCommand args =
  handleParseResult (execParserPure defaultPrefs badgeInfo args) >>= \case
    Keygen -> keygen
    Sign sk pk badgeType badgeExpiry -> sign sk pk badgeType badgeExpiry
  where
    badgeInfo = info (helper <*> hsubparser badgeCmd) fullDesc
    badgeCmd = command "badge" (info (helper <*> badgeCommandP) (progDesc "SimpleX supporter badge tooling"))

badgeCommandP :: Parser BadgeCommand
badgeCommandP =
  hsubparser $
    command "keygen" (info (pure Keygen) (progDesc "generate a BBS issuer keypair (base64url)"))
      <> command "sign" (info signP (progDesc "sign a badge credential, printed as one-line JSON"))
  where
    signP =
      Sign
        <$> keyOpt "secret" "SK" "issuer secret key (base64url)"
        <*> keyOpt "key" "PK" "issuer public key (base64url)"
        <*> option (eitherReader badgeTypeR) (long "type" <> metavar "TYPE" <> help "badge type (supporter, business, ...)")
        <*> option (eitherReader expireR) (long "expire" <> metavar "lifetime|YYYY-MM-DD" <> help "expiry date, or 'lifetime'")
    keyOpt l m h = option (eitherReader $ strDecode . B.pack) (long l <> metavar m <> help h)
    badgeTypeR = maybe (Left "invalid badge type") Right . textDecode . T.pack
    expireR = \case
      "lifetime" -> Right Nothing
      s -> maybe (Left "use 'lifetime' or YYYY-MM-DD") (Right . Just) $ parseTimeM True defaultTimeLocale "%Y-%m-%d" s

keygen :: IO ()
keygen =
  bbsKeyGen >>= \case
    Left e -> die $ "keygen failed: " <> e
    Right (sk, pk) -> do
      B.putStrLn $ "secret " <> strEncode sk
      B.putStrLn $ "public " <> strEncode pk

sign :: BBSSecretKey -> BBSPublicKey -> BadgeType -> Maybe UTCTime -> IO ()
sign secretKey publicKey badgeType badgeExpiry = do
  drg <- C.newRandom
  masterKey <- generateMasterKey drg
  let req = VerifiedBadgeRequest BadgeRequest {masterKey, badgeInfo = BadgeInfo {badgeType, badgeExpiry, badgeExtra = ""}}
  issueBadge secretKey publicKey req >>= \case
    Left e -> die $ "sign failed: " <> e
    -- single-line JSON, pasted into the app via `/badge add`
    Right cred -> LB.putStrLn $ J.encode cred
