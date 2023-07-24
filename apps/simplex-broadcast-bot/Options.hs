{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Options where

import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative
import Simplex.Chat.Bot.KnownContacts
import Simplex.Chat.Controller (updateStr, versionNumber, versionString)
import Simplex.Chat.Options (ChatOpts (..), CoreChatOpts, coreChatOptsP)
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (safeDecodeUtf8)

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
      chatCmd = "",
      chatCmdDelay = 3,
      chatServerPort = Nothing,
      optFilesFolder = Nothing,
      showReactions = False,
      allowInstantFiles = True,
      autoAcceptFileSize = 0,
      muteNotifications = True,
      maintenance = False
    }
