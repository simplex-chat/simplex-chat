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
    reserveCode,
    releaseCode,
    spendLandedCode,
    issueNameCode,
    revokeNameCode,
  )
where

import Control.Applicative ((<|>))
import qualified Control.Exception as E
import Control.Logger.Simple (logError, logWarn)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
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
import StoreService.Names.Store (CmdSource, CodeHash, CodeHashTaken (..), CodeId, NewPrepaidCode (..), PrepaidCode (..), getPrepaidCode, getPrepaidCodeById, insertPrepaidCode, releasePrepaidCode, reserveOwnPrepaidCode, revokePrepaidCode, spendLandedPrepaidCode)
import StoreService.Names.Turns (Turns, withCodeTurn)
import StoreService.Store (RevokeResult (..))
import StoreService.Store.Invoices (currentSecond)

data CodeState = CSUnused | CSReserved | CSUsed | CSRevoked | CSExpired | CSUnpaid
  deriving (Eq, Show, Enum, Bounded)

-- | A code revoked while its order held it can still be spent by that order, so a spent code reads as used.
codeState :: UTCTime -> PrepaidCode -> CodeState
codeState now PrepaidCode {pcReservedBy, pcSpentAt, pcRevokedAt, pcPaymentStatus, pcExpiresAt}
  | isJust pcSpentAt = CSUsed
  | isJust pcRevokedAt = CSRevoked
  | isJust pcReservedBy = CSReserved
  | CPSUnpaid <- pcPaymentStatus = CSUnpaid
  | maybe False (<= now) pcExpiresAt = CSExpired
  | otherwise = CSUnused

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

-- | Why the code cannot pay for the registration's name of this length. A code the registration
-- already holds stays good for it until spent, even if revoked or expired since.
codeRefusal :: UTCTime -> RegistrationId -> NameLength -> PrepaidCode -> Maybe CodeRefusal
codeRefusal now rid len code@PrepaidCode {pcReservedBy, pcSpentAt, pcMinLength}
  | isJust pcSpentAt = Just CRUsed
  | pcReservedBy == Just rid = covering
  | otherwise = stateRefusal (codeState now code) <|> covering
  where
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

ownCodeRefusal :: RegistrationId -> NameLength -> PrepaidCode -> Maybe CodeRefusal
ownCodeRefusal rid len PrepaidCode {pcReservedBy, pcSpentAt, pcRevokedAt, pcMinLength}
  | isJust pcSpentAt = Just CRUsed
  | maybe False (/= rid) pcReservedBy = Just CRReserved
  | isNothing pcReservedBy && isJust pcRevokedAt = Just CRInvalid
  | nameLengthInt len < pcMinLength = Just CRNotCovering
  | otherwise = Nothing

-- | Why a conditional reservation of the code refused it, as the code reads now.
reserveRefusal :: DB.Connection -> UTCTime -> CodeHash -> RegistrationId -> NameLength -> IO CodeRefusal
reserveRefusal db now codeHash rid len = maybe CRInvalid (fromMaybe CRReserved . codeRefusal now rid len) <$> getPrepaidCode db codeHash

readPrepaidCode :: DBStore -> NamePrepaidCode -> IO (Maybe PrepaidCode)
readPrepaidCode st code = withTransaction st (`getPrepaidCode` prepaidCodeHash code)

prepaidCodeHash :: NamePrepaidCode -> CodeHash
prepaidCodeHash = Hex32 . namePrepaidCodeHash

newtype ReservationRefused = ReservationRefused CodeRefusal
  deriving (Show)

instance E.Exception ReservationRefused

-- | In the code's turn and one transaction, the registration's own code is checked, the write runs, then the code is reserved for the
-- registration again, so the registration's row is locked before the code's. As a held code, it stays good for its registration
-- even if expired since, and if revoked only while the registration holds it. A refused reservation rolls the write back.
reserveCode :: Turns -> DBStore -> UTCTime -> CodeHash -> RegistrationId -> NameLength -> (DB.Connection -> IO (Either e a)) -> IO (Either CodeRefusal (Either e a))
reserveCode turns st now codeHash rid len write =
  withCodeTurn turns codeHash $ either (\(ReservationRefused refusal) -> Left refusal) id <$> E.try (withTransaction st reserve)
  where
    reserve db =
      getPrepaidCode db codeHash >>= \case
        Nothing -> pure $ Left CRInvalid
        Just code@PrepaidCode {pcId, pcReservedBy}
          | Just refusal <- ownCodeRefusal rid len code -> pure $ Left refusal
          | otherwise ->
              write db >>= \case
                written@(Right _)
                  | pcReservedBy /= Just rid ->
                      reserveOwnPrepaidCode db pcId rid now >>= \case
                        True -> pure $ Right written
                        -- Only a writer outside this process, which takes no turn here, can refuse it.
                        False -> E.throwIO . ReservationRefused . maybe CRInvalid (fromMaybe CRReserved . ownCodeRefusal rid len) =<< getPrepaidCode db codeHash
                written -> pure $ Right written

-- | The write runs first, and the code is released only if it succeeds. False when the registration did not hold the code.
releaseCode :: Turns -> DBStore -> CodeId -> RegistrationId -> (DB.Connection -> IO (Either e a)) -> IO (Either e (a, Bool))
releaseCode turns st codeId rid = afterWrite turns st codeId (\db -> releasePrepaidCode db codeId rid)

-- | The write runs first, and the code is spent only if it succeeds: the code the registration holds, or one it no longer holds
-- while still usable, as its register went through. False when the code was released and then spent, revoked or expired meanwhile.
spendLandedCode :: Turns -> DBStore -> UTCTime -> CodeId -> RegistrationId -> (DB.Connection -> IO (Either e a)) -> IO (Either e (a, Bool))
spendLandedCode turns st now codeId rid = afterWrite turns st codeId (\db -> spendLandedPrepaidCode db codeId rid now)

afterWrite :: Turns -> DBStore -> CodeId -> (DB.Connection -> IO Bool) -> (DB.Connection -> IO (Either e a)) -> IO (Either e (a, Bool))
afterWrite turns st codeId change write = do
  PrepaidCode {pcCodeHash} <- withTransaction st (`getPrepaidCodeById` codeId)
  withCodeTurn turns pcCodeHash $ withTransaction st $ \db -> write db >>= traverse (\a -> (a,) <$> change db)

codeValidMonths :: Integer
codeValidMonths = 3

-- | The code table keeps only the hash, so the caller must deliver the code.
issueNameCode :: ChatController -> CmdSource -> NameLength -> Int -> BadgeCodePaymentStatus -> IO (Either String (NamePrepaidCode, UTCTime))
issueNameCode cc issuer len years paymentStatus
  | years < minNameYears || years > maxNameYears = Left outside <$ logError ("names: " <> T.pack outside)
  | otherwise = issue (3 :: Int)
  where
    outside = "name code years " <> show years <> " is outside " <> show minNameYears <> "-" <> show maxNameYears
    issue retries = do
      code <- randomNamePrepaidCode (random cc) len
      now <- currentSecond
      let expiresAt = addMonths codeValidMonths now
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
