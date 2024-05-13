{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Broadcast.Options where

import Data.Maybe (fromMaybe)
import Options.Applicative
import Simplex.Chat.Bot.KnownContacts
import Simplex.Chat.Controller (updateStr, versionNumber, versionString)
import Simplex.Chat.Options (ChatCmdLog (..), ChatOpts (..), CoreChatOpts, coreChatOptsP)

data BroadcastBotOpts = BroadcastBotOpts
  { coreOptions :: CoreChatOpts,
    publishers :: [KnownContact],
    welcomeMessage :: String,
    prohibitedMessage :: String
  }

defaultWelcomeMessage :: [KnownContact] -> String
defaultWelcomeMessage ps = "Hello! I am a broadcast bot.\nI broadcast messages to all connected users from " <> knownContactNames ps <> "."

defaultProhibitedMessage :: [KnownContact] -> String
defaultProhibitedMessage ps = "Sorry, only these users can broadcast messages: " <> knownContactNames ps <> ". Your message is deleted."

broadcastBotOpts :: FilePath -> FilePath -> Parser BroadcastBotOpts
broadcastBotOpts appDir defaultDbFileName = do
  coreOptions <- coreChatOptsP appDir defaultDbFileName
  publishers <-
    option
      parseKnownContacts
      ( long "publishers"
          <> metavar "PUBLISHERS"
          <> help "Comma-separated list of publishers in the format CONTACT_ID:DISPLAY_NAME whose messages will be broadcasted"
          <> value []
      )
  welcomeMessage_ <-
    optional $
      strOption
        ( long "welcome"
            <> metavar "WELCOME"
            <> help "Welcome message to be sent to all connecting users (default message will list allowed publishers)"
        )
  prohibitedMessage_ <-
    optional $
      strOption
        ( long "prohibited"
            <> metavar "PROHIBITED"
            <> help "Reply to non-publishers who try to send messages (default reply will list allowed publishers)"
            <> showDefault
        )
  pure
    BroadcastBotOpts
      { coreOptions,
        publishers,
        welcomeMessage = fromMaybe (defaultWelcomeMessage publishers) welcomeMessage_,
        prohibitedMessage = fromMaybe (defaultProhibitedMessage publishers) prohibitedMessage_
      }

getBroadcastBotOpts :: FilePath -> FilePath -> IO BroadcastBotOpts
getBroadcastBotOpts appDir defaultDbFileName =
  execParser $
    info
      (helper <*> versionOption <*> broadcastBotOpts appDir defaultDbFileName)
      (header versionStr <> fullDesc <> progDesc "Start chat bot with DB_FILE file and use SERVER as SMP server")
  where
    versionStr = versionString versionNumber
    versionOption = infoOption versionAndUpdate (long "version" <> short 'v' <> help "Show version")
    versionAndUpdate = versionStr <> "\n" <> updateStr

mkChatOpts :: BroadcastBotOpts -> ChatOpts
mkChatOpts BroadcastBotOpts {coreOptions} =
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
