{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Names.Codes
  ( CodeState (..),
    codeState,
    CodeRefusal (..),
    codeRefusal,
    reserveRefusal,
    readPrepaidCode,
    prepaidCodeHash,
    releaseCode,
    spendLandedCode,
    issueNameCode,
    codeExpiry,
    revokeNameCode,
  )
where

import Control.Applicative ((<|>))
import Control.Logger.Simple (logError, logWarn)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime (..))
import Simplex.Chat.Badges.Code (NameLength, NamePrepaidCode, minNameYears, nameLengthInt, namePrepaidCodeHash, randomNamePrepaidCode)
import Simplex.Chat.Badges.Ledger (addMonths)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Bot.Store (withDB')
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import StoreService.Group.Command (maxNameYears)
import StoreService.Names.Chain (Hex32 (..))
import StoreService.Names.Phases (RegistrationId)
import StoreService.Names.Store (CmdSource, CodeHash, CodeHashTaken (..), CodeId, NewPrepaidCode (..), PrepaidCode (..), getPrepaidCode, getPrepaidCodeById, insertPrepaidCode, releasePrepaidCode, revokePrepaidCode, spendLandedPrepaidCode)
import StoreService.Names.Turns (Turns, withCodeTurn)
import StoreService.Store (RevokeResult (..))
import StoreService.Store.Invoices (currentSecond)

data CodeState = CSUnused | CSReserved | CSUsed | CSRevoked | CSExpired | CSUnpaid
  deriving (Eq, Show, Enum, Bounded)

-- | A code revoked while its order held it can still be spent by that order, so a spent code reads as used.
codeState :: UTCTime -> PrepaidCode -> CodeState
codeState now PrepaidCode {pcReservedBy, pcHolderTakenAt, pcSpentAt, pcRevokedAt, pcPaymentStatus, pcExpiresAt}
  | isJust pcSpentAt = CSUsed
  | isJust pcRevokedAt = CSRevoked
  | isJust pcReservedBy && not (expired && maybe False ((<= now) . addMonths 1) pcHolderTakenAt) = CSReserved
  | CPSUnpaid <- pcPaymentStatus = CSUnpaid
  | expired = CSExpired
  | otherwise = CSUnused
  where
    expired = maybe False (<= now) pcExpiresAt

instance TextEncoding CodeState where
  textEncode = \case
    CSUnused -> "unused"
    CSReserved -> "reserved"
    CSUsed -> "used"
    CSRevoked -> "revoked"
    CSExpired -> "expired"
    CSUnpaid -> "unpaid"
  textDecode = \case
    "unused" -> Just CSUnused
    "reserved" -> Just CSReserved
    "used" -> Just CSUsed
    "revoked" -> Just CSRevoked
    "expired" -> Just CSExpired
    "unpaid" -> Just CSUnpaid
    _ -> Nothing

data CodeRefusal = CRInvalid | CRUsed | CRReserved | CRExpired | CRPaymentPending | CRNotCovering
  deriving (Eq, Show)

-- | Why the registration's name of this length cannot be registered with the code. A code the registration
-- already holds stays good for it until spent, unless revoked, and past its expiry until a month after the order's take.
codeRefusal :: UTCTime -> RegistrationId -> Maybe UTCTime -> NameLength -> PrepaidCode -> Maybe CodeRefusal
codeRefusal now rid takenAt len code@PrepaidCode {pcReservedBy, pcSpentAt, pcRevokedAt, pcExpiresAt, pcMinLength}
  | isJust pcSpentAt = Just CRUsed
  | pcReservedBy == Just rid = (CRInvalid <$ pcRevokedAt) <|> heldExpired <|> covering
  | otherwise = stateRefusal (codeState now code) <|> covering
  where
    heldExpired
      | maybe False (<= now) pcExpiresAt && maybe True ((<= now) . addMonths 1) takenAt = Just CRExpired
      | otherwise = Nothing
    covering
      | nameLengthInt len < pcMinLength = Just CRNotCovering
      | otherwise = Nothing

stateRefusal :: CodeState -> Maybe CodeRefusal
stateRefusal = \case
  CSUnused -> Nothing
  CSReserved -> Just CRReserved
  CSUsed -> Just CRUsed
  CSRevoked -> Just CRInvalid
  CSExpired -> Just CRExpired
  CSUnpaid -> Just CRPaymentPending

-- | Why a conditional reservation of the code refused it, as the code reads now.
reserveRefusal :: DB.Connection -> UTCTime -> CodeHash -> RegistrationId -> Maybe UTCTime -> NameLength -> IO CodeRefusal
reserveRefusal db now codeHash rid takenAt len = maybe CRInvalid (fromMaybe CRReserved . codeRefusal now rid takenAt len) <$> getPrepaidCode db codeHash
{-# INLINE reserveRefusal #-}

readPrepaidCode :: DBStore -> NamePrepaidCode -> IO (Maybe PrepaidCode)
readPrepaidCode st code = withTransaction st (`getPrepaidCode` prepaidCodeHash code)
{-# INLINE readPrepaidCode #-}

prepaidCodeHash :: NamePrepaidCode -> CodeHash
prepaidCodeHash = Hex32 . namePrepaidCodeHash
{-# INLINE prepaidCodeHash #-}

-- | The write runs first, and the code is released only if it succeeds. False when the registration did not hold the code.
releaseCode :: Turns -> DBStore -> CodeId -> RegistrationId -> (DB.Connection -> IO (Either e a)) -> IO (Either e (a, Bool))
releaseCode turns st codeId rid = afterWrite turns st codeId (\db -> releasePrepaidCode db codeId rid)
{-# INLINE releaseCode #-}

-- | The write runs first, and the code is spent only if it succeeds: the code the registration holds, or one it no longer holds
-- while still usable, as its register went through. False when the code was released and then spent, revoked or expired meanwhile.
spendLandedCode :: Turns -> DBStore -> UTCTime -> CodeId -> RegistrationId -> (DB.Connection -> IO (Either e a)) -> IO (Either e (a, Bool))
spendLandedCode turns st now codeId rid = afterWrite turns st codeId (\db -> spendLandedPrepaidCode db codeId rid now)
{-# INLINE spendLandedCode #-}

afterWrite :: Turns -> DBStore -> CodeId -> (DB.Connection -> IO Bool) -> (DB.Connection -> IO (Either e a)) -> IO (Either e (a, Bool))
afterWrite turns st codeId change write = do
  PrepaidCode {pcCodeHash} <- withTransaction st (`getPrepaidCodeById` codeId)
  withCodeTurn turns pcCodeHash $ withTransaction st $ \db -> write db >>= traverse (\a -> (a,) <$> change db)

codeValidMonths :: Integer
codeValidMonths = 3

-- | The end of the given day, UTC, or 3 calendar months from now.
codeExpiry :: UTCTime -> Maybe Day -> UTCTime
codeExpiry now = maybe (addMonths codeValidMonths now) (`UTCTime` 86399)

-- | The code table keeps only the hash, so the caller must deliver the code.
issueNameCode :: ChatController -> CmdSource -> NameLength -> Int -> BadgeCodePaymentStatus -> Maybe Day -> IO (Either String (NamePrepaidCode, UTCTime))
issueNameCode cc issuer len years paymentStatus lastDay
  | years < minNameYears || years > maxNameYears = Left outside <$ logError ("names: " <> T.pack outside)
  | otherwise = issue (3 :: Int)
  where
    outside = "name code years " <> show years <> " is outside " <> show minNameYears <> "-" <> show maxNameYears
    issue retries = do
      code <- randomNamePrepaidCode (random cc) len
      now <- currentSecond
      let expiresAt = codeExpiry now lastDay
          new = NewPrepaidCode {npcCodeHash = prepaidCodeHash code, npcMinLength = nameLengthInt len, npcYears = years, npcPaymentStatus = paymentStatus, npcExpiresAt = Just expiresAt, npcIssuer = issuer}
      withDB' "issueNameCode" cc (\db -> insertPrepaidCode db new now) >>= \case
        Right (Right _) -> pure $ Right (code, expiresAt)
        Right (Left CodeHashTaken)
          | retries > 0 -> logWarn "names: a new prepaid code's hash is taken, so another code is drawn" >> issue (retries - 1)
          | otherwise -> Left "prepaid code hash taken" <$ logError "names: the hashes of 4 new prepaid codes in a row are taken, so no code is issued"
        Left e -> pure $ Left e

-- | Left is a refusal or a failure; both sides are the text to show the operator.
revokeNameCode :: Turns -> ChatController -> NamePrepaidCode -> IO (Either Text Text)
revokeNameCode turns cc code = withCodeTurn turns codeHash $ do
  now <- currentSecond
  withDB' "revokeNameCode" cc (\db -> revokePrepaidCode db codeHash now) <&> \case
    Right (Revoked _, Just PrepaidCode {pcReservedBy = Just _}) -> Right "Revoked. The order holding it keeps it."
    Right (Revoked _, _) -> Right "Revoked."
    Right (AlreadyRevoked _, _) -> Right "Already revoked."
    Right (AlreadyRedeemed, _) -> Left "Already used. It cannot be revoked."
    Right (NoSuchCode, _) -> Left "No such code."
    Left _ -> Left "The code could not be revoked."
  where
    codeHash = prepaidCodeHash code
