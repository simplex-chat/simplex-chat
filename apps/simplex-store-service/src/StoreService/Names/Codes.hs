{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Codes
  ( issueNameCode,
    revokeNameCode,
  )
where

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Simplex.Chat.Badges.Code (NamePrepaidCode, maxNameLength, minNameLength, namePrepaidCodeHash, randomNamePrepaidCode)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Bot.Store (withDB')
import Simplex.Chat.Controller (ChatController (..))
import StoreService.Config (minNameYears)
import StoreService.Group.Command (maxNameYears)
import StoreService.Names.Store (CodeHashTaken (..), NewPrepaidCode (..), insertPrepaidCode, revokePrepaidCode)
import StoreService.Names.Turns (Turns, withCodeTurn)
import StoreService.Store (RevokeResult (..))

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
  withDB' "revokeNameCode" cc (\db -> revokePrepaidCode db codeHash now) <&> \case
    Right (Revoked _) -> Right "Revoked."
    Right (AlreadyRevoked _) -> Right "Already revoked."
    Right AlreadyRedeemed -> Left "Already used. It cannot be revoked."
    Right NoSuchCode -> Left "No such code."
    Left _ -> Left "The code could not be revoked."
  where
    codeHash = namePrepaidCodeHash code
