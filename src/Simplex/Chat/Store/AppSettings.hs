{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Store.AppSettings where

import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import Database.SQLite.Simple (Only (..))
import Simplex.Chat.AppSettings (AppSettings (..), defaultAppSettings)
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Messaging.Agent.Store.SQLite (maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB

saveAppSettings :: DB.Connection -> AppSettings -> IO ()
saveAppSettings db appSettings = do
  DB.execute_ db "DELETE FROM app_settings"
  DB.execute db "INSERT INTO app_settings (app_settings) VALUES (?)" (Only $ J.encode appSettings)

getAppSettings :: DB.Connection -> ExceptT StoreError IO AppSettings
getAppSettings db = ExceptT $ do
  liftIO (maybeFirstRow fromOnly $ DB.query_ db "SELECT app_settings FROM app_settings")
    >>= maybe (pure $ Right defaultAppSettings) (pure . first SEAppSettingsInvalid . J.eitherDecodeStrict)
