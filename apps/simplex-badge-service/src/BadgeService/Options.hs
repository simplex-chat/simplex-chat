{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.Options
  ( BadgeServiceOpts (..),
    CliCommand (..),
    getBadgeServiceOpts,
    getBadgeServiceCommand,
    badgeServiceOpts,
    mkChatOpts,
  )
where

import BadgeService.Admin (AdminOpts (..), adminCommandParser)
import qualified Data.Text as T
import Options.Applicative
import Simplex.Chat.Controller (updateStr, versionNumber, versionString)
import Simplex.Chat.Options (ChatCmdLog (..), ChatOpts (..), CoreChatOpts, CreateBotOpts (..), coreChatOptsP)
import System.FilePath ((</>))

data BadgeServiceOpts = BadgeServiceOpts
  { coreOptions :: CoreChatOpts,
    serviceName :: T.Text,
    clientService :: Bool,
    noAddress :: Bool,
    runCLI :: Bool,
    testing :: Bool,
    configFile :: FilePath
  }

-- | Either the plain service run -- the DEFAULT when no subcommand is given -- or the
-- operator @codes@ subcommand ("BadgeService.Admin"). Named apart from
-- 'Simplex.Chat.Badges.Service.BadgeServiceCommand' (the RPC protocol's command sum, unrelated
-- and imported into "BadgeService.Service" alongside this module): that name was already
-- taken, and a CLI-invocation type borrowing it would have been confusing on its own terms
-- regardless of the clash. 'hsubparser' alone makes a subcommand mandatory, which would break
-- every existing way of starting the service, so 'badgeServiceCommand' wraps it in 'optional'
-- (decision 3): both branches of the parser always run, and only the VALUE produced (not the
-- parser structure) depends on whether @codes@ was given.
data CliCommand
  = RunService BadgeServiceOpts
  | RunAdmin AdminOpts

badgeServiceOpts :: FilePath -> FilePath -> Parser BadgeServiceOpts
badgeServiceOpts appDir defaultDbName = do
  coreOptions <- coreChatOptsP appDir defaultDbName
  configFile <-
    strOption
      ( long "config"
          <> metavar "FILE"
          <> help "Path to the badge service deployment configuration (ini)"
          <> value (appDir </> "badge_service.ini")
          <> showDefault
      )
  serviceName <-
    strOption
      ( long "service-name"
          <> metavar "SERVICE_NAME"
          <> help "The display name of the badge service bot, without *'s and spaces (SimpleX Badges)"
          <> value "SimpleX Badges"
      )
  clientService <-
    switch
      ( long "client-service"
          <> help "Use client service certificate"
      )
  noAddress <-
    switch
      ( long "no-address"
          <> help "skip checking and creating service address"
      )
  runCLI <-
    switch
      ( long "run-cli"
          <> help "Run badge service as CLI"
      )
  pure
    BadgeServiceOpts
      { coreOptions,
        serviceName = T.pack serviceName,
        clientService,
        noAddress,
        runCLI,
        testing = False,
        configFile
      }

getBadgeServiceOpts :: FilePath -> FilePath -> IO BadgeServiceOpts
getBadgeServiceOpts appDir defaultDbName =
  execParser $
    info
      (helper <*> versionOption <*> badgeServiceOpts appDir defaultDbName)
      (header versionStr <> fullDesc <> progDesc "Start SimpleX Badge Service with DB_FILE options")
  where
    versionStr = versionString versionNumber
    versionOption = infoOption versionAndUpdate (long "version" <> short 'v' <> help "Show version")
    versionAndUpdate = versionStr <> "\n" <> updateStr

-- | Parses either the plain service run or the @codes@ subcommand (see 'CliCommand').
-- Plain 'Applicative' combinators, not @do@\/'ApplicativeDo': 'Parser' has no 'Monad'
-- instance, and GHC's 'ApplicativeDo' desugaring does not always find an applicative-only
-- reading of a @do@ block ending in a 'case' over an earlier bind, as this one does.
-- @--config@ and the core database options are parsed unconditionally by 'badgeServiceOpts',
-- so they apply the same way to both branches: the subcommand loads the same ini as a
-- service run.
badgeServiceCommand :: FilePath -> FilePath -> Parser CliCommand
badgeServiceCommand appDir defaultDbName =
  toCommand <$> badgeServiceOpts appDir defaultDbName <*> optional codesSubparser
  where
    codesSubparser =
      hsubparser $
        command "codes" (info adminCommandParser (progDesc "Operator commands for redemption codes"))
    toCommand opts@BadgeServiceOpts {coreOptions, configFile} = \case
      Nothing -> RunService opts
      Just adminCmd -> RunAdmin AdminOpts {adminCoreOptions = coreOptions, adminConfigFile = configFile, adminCmd}

getBadgeServiceCommand :: FilePath -> FilePath -> IO CliCommand
getBadgeServiceCommand appDir defaultDbName =
  execParser $
    info
      (helper <*> versionOption <*> badgeServiceCommand appDir defaultDbName)
      (header versionStr <> fullDesc <> progDesc "Start SimpleX Badge Service, or run an operator subcommand")
  where
    versionStr = versionString versionNumber
    versionOption = infoOption versionAndUpdate (long "version" <> short 'v' <> help "Show version")
    versionAndUpdate = versionStr <> "\n" <> updateStr

mkChatOpts :: BadgeServiceOpts -> ChatOpts
mkChatOpts BadgeServiceOpts {coreOptions, serviceName, clientService} =
  ChatOpts
    { coreOptions,
      chatCmd = "",
      chatCmdDelay = 3,
      chatCmdLog = CCLNone,
      chatServerPort = Nothing,
      optFilesFolder = Nothing,
      optTempDirectory = Nothing,
      showReactions = False,
      showFullLinks = False,
      allowInstantFiles = True,
      autoAcceptFileSize = 0,
      muteNotifications = True,
      markRead = False,
      createBot = Just CreateBotOpts {botDisplayName = serviceName, allowFiles = False, clientService},
      userDisplayName = Nothing,
      userImageFile = Nothing
    }
