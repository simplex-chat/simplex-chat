module Simplex.Chat.Util (catchExcept, week) where

import Control.Monad.Except
import Control.Monad.IO.Unlift
import Data.Time (NominalDiffTime)
-- import Simplex.Messaging.Agent ()
import Simplex.Messaging.Util (tryError)
import qualified UnliftIO.Exception as E

week :: NominalDiffTime
week = 7 * 86400

catchExcept :: (MonadUnliftIO m, MonadError e m) => (E.SomeException -> e) -> m a -> (e -> m a) -> m a
catchExcept err action handle = do
  r <- tryError action `E.catch` (pure . Left . err)
  case r of
    Right a -> pure a
    Left e -> handle e
