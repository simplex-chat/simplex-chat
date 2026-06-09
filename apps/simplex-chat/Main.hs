module Main where

import Server (simplexChatServer)
import Simplex.Chat.Badges.CLI (runBadgeCommand)
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("badge" : _) -> runBadgeCommand args
    _ -> simplexChatCLI terminalChatConfig (Just simplexChatServer)
