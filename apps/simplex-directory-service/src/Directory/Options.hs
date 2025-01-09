{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Directory.Options
  ( DirectoryOpts (..),
    getDirectoryOpts,
    mkChatOpts,
  )
where

import qualified Data.Text as T
import Options.Applicative
import Simplex.Chat.Bot.KnownContacts
import Simplex.Chat.Controller (updateStr, versionNumber, versionString)
import Simplex.Chat.Options (ChatOpts (..), ChatCmdLog (..), CoreChatOpts, coreChatOptsP)

data DirectoryOpts = DirectoryOpts
  { coreOptions :: CoreChatOpts,
    adminUsers :: [KnownContact],
    superUsers :: [KnownContact],
    directoryLog :: Maybe FilePath,
    serviceName :: T.Text,
    runCLI :: Bool,
    searchResults :: Int,
    testing :: Bool
  }

directoryOpts :: FilePath -> FilePath -> Parser DirectoryOpts
directoryOpts appDir defaultDbFileName = do
  coreOptions <- coreChatOptsP appDir defaultDbFileName
  adminUsers <-
    option
      parseKnownContacts
      ( long "admin-users"
          <> metavar "ADMIN_USERS"
          <> help "Comma-separated list of admin-users in the format CONTACT_ID:DISPLAY_NAME who will be allowed to manage the directory"
      )
  superUsers <-
    option
      parseKnownContacts
      ( long "super-users"
          <> metavar "SUPER_USERS"
          <> help "Comma-separated list of super-users in the format CONTACT_ID:DISPLAY_NAME who will be allowed to manage the directory"
      )
  directoryLog <-
    Just
      <$> strOption
        ( long "directory-file"
            <> metavar "DIRECTORY_FILE"
            <> help "Append only log for directory state"
        )
  serviceName <-
    strOption
      ( long "service-name"
          <> metavar "SERVICE_NAME"
          <> help "The display name of the directory service bot, without *'s and spaces (SimpleX-Directory)"
          <> value "SimpleX-Directory"
      )
  runCLI <- 
    switch
      ( long "run-cli"
          <> help "Run directory service as CLI"
      )
  pure
    DirectoryOpts
      { coreOptions,
        adminUsers,
        superUsers,
        directoryLog,
        serviceName = T.pack serviceName,
        runCLI,
        searchResults = 10,
        testing = False
      }

getDirectoryOpts :: FilePath -> FilePath -> IO DirectoryOpts
getDirectoryOpts appDir defaultDbFileName =
  execParser $
    info
      (helper <*> versionOption <*> directoryOpts appDir defaultDbFileName)
      (header versionStr <> fullDesc <> progDesc "Start SimpleX Directory Service with DB_FILE, DIRECTORY_FILE and SUPER_USERS options")
  where
    versionStr = versionString versionNumber
    versionOption = infoOption versionAndUpdate (long "version" <> short 'v' <> help "Show version")
    versionAndUpdate = versionStr <> "\n" <> updateStr

mkChatOpts :: DirectoryOpts -> ChatOpts
mkChatOpts DirectoryOpts {coreOptions} =
  ChatOpts
    { coreOptions,
      deviceName = Nothing,
      chatCmd = "",
      chatCmdDelay = 3,
      chatCmdLog = CCLNone,
      chatServerPort = Nothing,
      optFilesFolder = Nothing,
      optTempDirectory = Nothing,
      showReactions = False,
      allowInstantFiles = True,
      autoAcceptFileSize = 0,
      muteNotifications = True,
      markRead = False,
      maintenance = False
    }
