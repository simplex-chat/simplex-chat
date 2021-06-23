{-# LANGUAGE LambdaCase #-}

module Simplex.Input where

import Control.Monad.IO.Class (liftIO)
import System.Exit (exitSuccess)
import System.Terminal

data InputEvent = InputCommand String | InputControl Char

getKey :: MonadTerminal m => m (Key, Modifiers)
getKey =
  flush >> awaitEvent >>= \case
    Left Interrupt -> liftIO exitSuccess
    Right (KeyEvent key ms) -> pure (key, ms)
    _ -> getKey
