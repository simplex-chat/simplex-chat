module Main where

import Broadcast.Bot
import Broadcast.Options
import Simplex.Chat.Core
import Simplex.Chat.Terminal (terminalChatConfig)

main :: IO ()
main = do
  opts <- welcomeGetOpts
  simplexChatCore terminalChatConfig (mkChatOpts opts) $ broadcastBot opts
