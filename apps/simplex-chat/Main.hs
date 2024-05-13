module Main where

import Server (simplexChatServer)
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI)

main :: IO ()
main = simplexChatCLI terminalChatConfig (Just simplexChatServer)
