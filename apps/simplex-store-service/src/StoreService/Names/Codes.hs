{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Names.Codes
  ( CodeState (..),
    codeState,
    codeStateText,
    CodeRefusal (..),
    codeRefusal,
    codeRefusalTag,
    reserveRefusal,
    readPrepaidCode,
    reserveCode,
    releaseCode,
    spendCode,
    spendLandedCode,
    issueNameCode,
    revokeNameCode,
  )
where

import Control.Applicative ((<|>))
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Simplex.Chat.Badges.Code (NamePrepaidCode, maxNameLength, minNameLength, namePrepaidCodeHash, randomNamePrepaidCode)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Bot.Store (withDB')
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import StoreService.Config (minNameYears)
import StoreService.Group.Command (maxNameYears)
import StoreService.Names.Phases (RegistrationId)
import StoreService.Names.Store (CodeHashTaken (..), NewPrepaidCode (..), PrepaidCode (..), getPrepaidCode, getPrepaidCodeById, insertPrepaidCode, releasePrepaidCode, reservePrepaidCode, revokePrepaidCode, spendLandedPrepaidCode, spendPrepaidCode)
import StoreService.Names.Turns (Turns, withCodeTurn)
import StoreService.Store (RevokeResult (..))

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

codeStateText :: CodeState -> Text
codeStateText = \case
  CSUnused -> "unused"
  CSReserved -> "reserved"
  CSUsed -> "used"
  CSRevoked -> "revoked"
  CSExpired -> "expired"
  CSUnpaid -> "unpaid"

data CodeRefusal = CRInvalid | CRUsed | CRReserved | CRExpired | CRPaymentPending | CRNotCovering
  deriving (Eq, Show)

codeRefusalTag :: CodeRefusal -> Text
codeRefusalTag = \case
  CRInvalid -> "code_invalid"
  CRUsed -> "code_used"
  CRReserved -> "code_reserved"
  CRExpired -> "code_expired"
  CRPaymentPending -> "payment_pending"
  CRNotCovering -> "code_not_covering"

-- | Why the code cannot pay for the registration's name of this length. A code the registration
-- already holds stays good for it until spent, even if revoked or expired since.
codeRefusal :: UTCTime -> RegistrationId -> Int -> PrepaidCode -> Maybe CodeRefusal
codeRefusal now rid len code@PrepaidCode {pcReservedBy, pcSpentAt, pcMinLength}
  | isJust pcSpentAt = Just CRUsed
  | pcReservedBy == Just rid = covering
  | otherwise = stateRefusal (codeState now code) <|> covering
  where
    covering
      | len < pcMinLength = Just CRNotCovering
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
reserveRefusal :: DB.Connection -> UTCTime -> ByteString -> RegistrationId -> Int -> IO CodeRefusal
reserveRefusal db now codeHash rid len = maybe CRInvalid (fromMaybe CRReserved . codeRefusal now rid len) <$> getPrepaidCode db codeHash

readPrepaidCode :: DBStore -> NamePrepaidCode -> IO (Maybe PrepaidCode)
readPrepaidCode st code = withTransaction st (`getPrepaidCode` namePrepaidCodeHash code)

newtype ReservationRefused = ReservationRefused CodeRefusal
  deriving (Show)

instance E.Exception ReservationRefused

-- | In the code's turn and one transaction, the code is checked, the write runs, then the code is reserved for the
-- registration, so the registration's row is locked before the code's. A refused reservation rolls the write back.
reserveCode :: Turns -> DBStore -> UTCTime -> ByteString -> RegistrationId -> Int -> (DB.Connection -> IO (Either e a)) -> IO (Either CodeRefusal (Either e a))
reserveCode turns st now codeHash rid len write =
  withCodeTurn turns codeHash $ either (\(ReservationRefused refusal) -> Left refusal) id <$> E.try (withTransaction st reserve)
  where
    reserve db =
      getPrepaidCode db codeHash >>= \case
        Nothing -> pure $ Left CRInvalid
        Just code@PrepaidCode {pcId, pcReservedBy}
          | Just refusal <- codeRefusal now rid len code -> pure $ Left refusal
          | otherwise ->
              write db >>= \case
                written@(Right _)
                  | pcReservedBy /= Just rid ->
                      reservePrepaidCode db pcId rid now >>= \case
                        True -> pure $ Right written
                        -- Only a writer outside this process, which takes no turn here, can refuse it.
                        False -> E.throwIO . ReservationRefused =<< reserveRefusal db now codeHash rid len
                written -> pure $ Right written

-- | The write runs first, and the code is released only if it succeeds. False when the registration did not hold the code.
releaseCode :: Turns -> DBStore -> Int64 -> RegistrationId -> (DB.Connection -> IO (Either e a)) -> IO (Either e (a, Bool))
releaseCode turns st codeId rid = afterWrite turns st codeId (\db -> releasePrepaidCode db codeId rid)

-- | The write runs first, and the code is spent only if it succeeds. False when the registration did not hold the code.
spendCode :: Turns -> DBStore -> UTCTime -> Int64 -> RegistrationId -> (DB.Connection -> IO (Either e a)) -> IO (Either e (a, Bool))
spendCode turns st now codeId rid = afterWrite turns st codeId (\db -> spendPrepaidCode db codeId rid now)

-- | As spendCode, for a registration whose register went through: a code it no longer holds is spent too while still usable.
-- False when the code was released and then spent, revoked or expired meanwhile.
spendLandedCode :: Turns -> DBStore -> UTCTime -> Int64 -> RegistrationId -> (DB.Connection -> IO (Either e a)) -> IO (Either e (a, Bool))
spendLandedCode turns st now codeId rid = afterWrite turns st codeId (\db -> spendLandedPrepaidCode db codeId rid now)

afterWrite :: Turns -> DBStore -> Int64 -> (DB.Connection -> IO Bool) -> (DB.Connection -> IO (Either e a)) -> IO (Either e (a, Bool))
afterWrite turns st codeId change write = do
  codeHash_ <- withTransaction st $ \db -> fmap pcCodeHash <$> getPrepaidCodeById db codeId
  let run = withTransaction st $ \db -> write db >>= traverse (\a -> (a,) <$> change db)
  maybe run (\codeHash -> withCodeTurn turns codeHash run) codeHash_

-- | The code table keeps only the hash, so the caller must deliver the code.
issueNameCode :: ChatController -> Int -> Int -> BadgeCodePaymentStatus -> IO (Either String NamePrepaidCode)
issueNameCode cc len years paymentStatus
  | len < minNameLength || len > maxNameLength = pure $ Left $ "name code length " <> show len <> " is outside " <> show minNameLength <> "-" <> show maxNameLength
  | years < minNameYears || years > maxNameYears = pure $ Left $ "name code years " <> show years <> " is outside " <> show minNameYears <> "-" <> show maxNameYears
  | otherwise = do
      code <- randomNamePrepaidCode (random cc) len
      now <- getCurrentTime
      let new = NewPrepaidCode {npcCodeHash = namePrepaidCodeHash code, npcMinLength = len, npcYears = years, npcPaymentStatus = paymentStatus, npcExpiresAt = Nothing}
      withDB' "issueNameCode" cc (\db -> insertPrepaidCode db new now) <&> \case
        Right (Right _) -> Right code
        Right (Left CodeHashTaken) -> Left "prepaid code hash taken"
        Left e -> Left e

-- | Left is a refusal or a failure; both sides are the text to show the operator.
revokeNameCode :: Turns -> ChatController -> NamePrepaidCode -> IO (Either Text Text)
revokeNameCode turns cc code = withCodeTurn turns codeHash $ do
  now <- getCurrentTime
  withDB' "revokeNameCode" cc (\db -> (,) <$> revokePrepaidCode db codeHash now <*> getPrepaidCode db codeHash) <&> \case
    Right (Revoked _, Just PrepaidCode {pcReservedBy = Just _}) -> Right "Revoked. The order holding it keeps it."
    Right (Revoked _, _) -> Right "Revoked."
    Right (AlreadyRevoked _, _) -> Right "Already revoked."
    Right (AlreadyRedeemed, _) -> Left "Already used. It cannot be revoked."
    Right (NoSuchCode, _) -> Left "No such code."
    Left _ -> Left "The code could not be revoked."
  where
    codeHash = namePrepaidCodeHash code
