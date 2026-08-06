{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.Options
  ( BadgeServiceOpts (..),
    getBadgeServiceOpts,
    badgeServiceOpts,
    mkChatOpts,
  )
where

import qualified Data.Text as T
import Options.Applicative
import Simplex.Chat.Controller (updateStr, versionNumber, versionString)
import Simplex.Chat.Options (ChatCmdLog (..), ChatOpts (..), CoreChatOpts, CreateBotOpts (..), coreChatOptsP)

data BadgeServiceOpts = BadgeServiceOpts
  { coreOptions :: CoreChatOpts,
    serviceName :: T.Text,
    clientService :: Bool,
    noAddress :: Bool,
    runCLI :: Bool,
    testing :: Bool
  }

badgeServiceOpts :: FilePath -> FilePath -> Parser BadgeServiceOpts
badgeServiceOpts appDir defaultDbName = do
  coreOptions <- coreChatOptsP appDir defaultDbName
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
        testing = False
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
