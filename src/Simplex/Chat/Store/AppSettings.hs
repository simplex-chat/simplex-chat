{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Store.AppSettings where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.Maybe (fromMaybe)
import Database.SQLite.Simple (Only (..))
import Simplex.Chat.AppSettings (AppSettings (..), combineAppSettings, defaultAppSettings, defaultParseAppSettings)
import Simplex.Messaging.Agent.Store.SQLite (maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB

saveAppSettings :: DB.Connection -> AppSettings -> IO ()
saveAppSettings db appSettings = do
  DB.execute_ db "DELETE FROM app_settings"
  DB.execute db "INSERT INTO app_settings (app_settings) VALUES (?)" (Only $ J.encode appSettings)

getAppSettings :: DB.Connection -> Maybe AppSettings -> IO AppSettings
getAppSettings db platformDefaults = do
  stored_ <- join <$> liftIO (maybeFirstRow (J.decodeStrict . fromOnly) $ DB.query_ db "SELECT app_settings FROM app_settings")
  pure $ combineAppSettings (fromMaybe defaultAppSettings platformDefaults) (fromMaybe defaultParseAppSettings stored_)
