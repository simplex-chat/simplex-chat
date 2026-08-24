{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import BadgeService.Admin (runAdminCmd)
import BadgeService.Options (BadgeServiceCommand (..), BadgeServiceOpts (..), getBadgeServiceCommand)
import BadgeService.Service
import Simplex.Chat.Terminal (terminalChatConfig)
import System.Directory (getAppUserDataDirectory)

-- | 'getBadgeServiceCommand' decides, from the same combined parser that generates --help,
-- whether this is the operator @codes@ subcommand or a plain service run (the default with
-- no arguments -- 'BadgeService.Options.badgeServiceCommand' wraps the @codes@ subparser in
-- 'optional' so this never becomes mandatory). The @codes@ branch runs and exits without ever
-- starting the bot; the plain-run branch falls through to 'welcomeGetOpts', unchanged, which
-- re-parses the same arguments to print the startup banner before starting the service.
main :: IO ()
main = do
  appDir <- getAppUserDataDirectory "simplex"
  getBadgeServiceCommand appDir "simplex_badge_service" >>= \case
    RunAdmin adminOpts -> runAdminCmd adminOpts
    RunService _ -> do
      opts@BadgeServiceOpts {runCLI} <- welcomeGetOpts
      if runCLI
        then badgeServiceCLI opts
        else badgeService opts terminalChatConfig
