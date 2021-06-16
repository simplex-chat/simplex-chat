{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Terminal where

import Control.Concurrent.STM (TVar, newTVarIO)
import System.Terminal (runTerminalT, withTerminal)
import qualified System.Terminal as C

data ChatTerminal = ChatTerminal
  { termState :: TVar TerminalState,
    termSize :: C.Size
  }

newtype TerminalState = TerminalState Int -- stub

newChatTerminal :: IO ChatTerminal
newChatTerminal = do
  termState <- newTVarIO $ TerminalState 0
  termSize <- withTerminal . runTerminalT $ do
    -- C.eraseInDisplay C.EraseAll
    C.setAlternateScreenBuffer True
    -- C.setAutoWrap False
    C.flush
    C.getWindowSize
  pure ChatTerminal {termState, termSize}
