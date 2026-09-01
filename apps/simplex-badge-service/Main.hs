{-# LANGUAGE NamedFieldPuns #-}

module Main where

import BadgeService.Options (BadgeServiceOpts (..))
import BadgeService.Service
import Simplex.Chat.Terminal (terminalChatConfig)

main :: IO ()
main = do
  opts@BadgeServiceOpts {runCLI} <- welcomeGetOpts
  if runCLI
    then badgeServiceCLI opts
    else newServiceState >>= badgeService opts terminalChatConfig
