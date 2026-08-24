{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Issuer key loading and credential signing (decision 4, B4). Signing itself is never
--   reimplemented here -- 'Simplex.Chat.Badges.issueBadge' does the BBS signing; this module
--   only loads the issuer secret from its config file and computes the expiry that goes into
--   the signed 'BadgeInfo'.
module BadgeService.Credentials
  ( loadIssuerKey,
    issueSignedBadge,
  )
where

import qualified Data.ByteString.Char8 as B
import Data.Maybe (mapMaybe)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Badges (BadgeCredential, BadgeInfo (..), BadgeRequest (..), VerifiedBadgeRequest (..), issueBadge)
import Simplex.Chat.Badges.Months (sundayAfter)
import Simplex.Chat.Badges.Service (BadgeServiceErrorCode (..))
import Simplex.Messaging.Crypto.BBS (BBSSecretKey)
import Simplex.Messaging.Encoding.String (strDecode)
import System.Directory (doesFileExist)
import System.Exit (die)

-- | Load and validate the '[issuer] key_file' / 'key_idx' pair (A6 reads the ini; this loads
--   what it points to). 'key_file' is the output of @simplex-chat badge keygen@
--   (Badges/CLI.hs), two labelled lines: @secret <base64url>@ and @public <base64url>@; only
--   the 'secret' line is read. Fails fast, naming the file, when it is absent, unreadable, or
--   missing/malformed 'secret' line; fails fast on a non-positive 'key_idx'.
loadIssuerKey :: FilePath -> Int -> IO BBSSecretKey
loadIssuerKey path keyIdx
  | keyIdx < 1 = die $ "issuer key_idx must be a positive integer, got " <> show keyIdx
  | otherwise = do
      exists <- doesFileExist path
      if not exists
        then die $ path <> ": file not found"
        else do
          contents <- B.readFile path
          case parseIssuerSecret contents of
            Left e -> die $ path <> ": " <> e
            Right sk -> pure sk

issuerSecretLinePrefix :: B.ByteString
issuerSecretLinePrefix = "secret "

parseIssuerSecret :: B.ByteString -> Either String BBSSecretKey
parseIssuerSecret contents = case mapMaybe (B.stripPrefix issuerSecretLinePrefix) (B.lines contents) of
  (b64 : _) -> strDecode b64
  [] -> Left "missing 'secret <base64url>' line (expected `simplex-chat badge keygen` output)"

-- | Sign a badge request into a credential (server side). 'badgeExpiry' is always
--   'sundayAfter periodEnd' -- the authoritative, server-computed expiry -- overriding
--   whatever 'badgeInfo' the caller passed in; 'issue' (BadgeService.Ledger) never yields a
--   period beyond the funded balance, so no further cap applies here. A non-empty
--   'badgeExtra' is rejected by 'issueBadge' itself; that failure is surfaced as
--   'BSEBadRequest'.
issueSignedBadge :: Int -> BBSSecretKey -> BadgeRequest -> UTCTime -> IO (Either BadgeServiceErrorCode BadgeCredential)
issueSignedBadge keyIdx sk req@BadgeRequest {badgeInfo} periodEnd = do
  let req' = req {badgeInfo = badgeInfo {badgeExpiry = Just (sundayAfter periodEnd)}}
  issueBadge keyIdx sk (VerifiedBadgeRequest req') >>= \case
    Left _ -> pure $ Left BSEBadRequest
    Right cred -> pure $ Right cred
