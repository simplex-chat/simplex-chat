module Main where

import Broadcast.Bot
import Broadcast.Options
import Control.Concurrent.Async
import Control.Monad
import Simplex.Chat.Core
import Simplex.Chat.Terminal (terminalChatConfig)

main :: IO ()
main = do
  opts <- welcomeGetOpts
  simplexChatCore terminalChatConfig (mkChatOpts opts) $ \user cc ->
    race_ (forever $ void getLine) $ broadcastBot opts user cc
