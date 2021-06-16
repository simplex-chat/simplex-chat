{-# LANGUAGE LambdaCase #-}

module Simplex.Keyboard where

import Control.Monad.IO.Class (liftIO)
import System.Exit (exitSuccess)
import System.Terminal (Event (..), Interrupt (..), Key (..), Modifiers, MonadTerminal)
import qualified System.Terminal as C

getKey :: MonadTerminal m => m (Key, Modifiers)
getKey =
  C.flush >> C.awaitEvent >>= \case
    Left Interrupt -> liftIO exitSuccess
    Right (KeyEvent key ms) -> pure (key, ms)
    _ -> getKey
