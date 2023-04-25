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
import Simplex.Chat.Controller (updateStr, versionNumber, versionString)
import Simplex.Chat.Options (ChatOpts (..), CoreChatOpts, coreChatOptsP)
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (safeDecodeUtf8)

data Publisher = Publisher
  { contactId :: Int64,
    localDisplayName :: Text
  }
  deriving (Eq)

data BroadcastBotOpts = BroadcastBotOpts
  { coreOptions :: CoreChatOpts,
    publishers :: [Publisher],
    welcomeMessage :: String,
    prohibitedMessage :: String
  }

defaultWelcomeMessage :: [Publisher] -> String
defaultWelcomeMessage ps = "Witam! Jestem botem rozgłaszającym.\nRozgłaszam wiadomości do wszystkich podłączonych użytkowników z " <> publisherNames ps <> "."

defaultProhibitedMessage :: [Publisher] -> String
defaultProhibitedMessage ps = "Niestety, tylko ci użytkownicy mogą rozgłaszać wiadomości: " <> publisherNames ps <> ". Twoja wiadomość została usunięta."

publisherNames :: [Publisher] -> String
publisherNames = T.unpack . T.intercalate ", " . map (("@" <>) . localDisplayName)

broadcastBotOpts :: FilePath -> FilePath -> Parser BroadcastBotOpts
broadcastBotOpts appDir defaultDbFileName = do
  coreOptions <- coreChatOptsP appDir defaultDbFileName
  publishers <-
    option
      parsePublishers
      ( long "publishers"
          <> metavar "PUBLISHERS"
          <> help "Rozdzielona przecinkami lista wydawców w formacie CONTACT_ID:DISPLAY_NAME , których wiadomości zostaną rozesłane"
          <> value []
      )
  welcomeMessage_ <-
    optional $
      strOption
        ( long "welcome"
            <> metavar "WELCOME"
            <> help "Wiadomość powitalna wysyłana do wszystkich podłączających się użytkowników (domyślnie wiadomość zawiera listę dozwolonych wydawców)"
        )
  prohibitedMessage_ <-
    optional $
      strOption
        ( long "prohibited"
            <> metavar "PROHIBITED"
            <> help "Odpowiadaj osobom niebędącym wydawcami, które próbują wysyłać wiadomości (domyślna odpowiedź zawiera listę dozwolonych wydawców)"
            <> showDefault
        )
  pure
    BroadcastBotOpts
      { coreOptions,
        publishers,
        welcomeMessage = fromMaybe (defaultWelcomeMessage publishers) welcomeMessage_,
        prohibitedMessage = fromMaybe (defaultProhibitedMessage publishers) prohibitedMessage_
      }

parsePublishers :: ReadM [Publisher]
parsePublishers = eitherReader $ parseAll publishersP . encodeUtf8 . T.pack

publishersP :: A.Parser [Publisher]
publishersP = publisherP `A.sepBy1` A.char ','
  where
    publisherP = do
      contactId <- A.decimal <* A.char ':'
      localDisplayName <- safeDecodeUtf8 <$> A.takeTill (A.inClass ", ")
      pure Publisher {contactId, localDisplayName}

getBroadcastBotOpts :: FilePath -> FilePath -> IO BroadcastBotOpts
getBroadcastBotOpts appDir defaultDbFileName =
  execParser $
    info
      (helper <*> versionOption <*> broadcastBotOpts appDir defaultDbFileName)
      (header versionStr <> fullDesc <> progDesc "Uruchom chatbota z plikiem DB_FILE i użyj SERVER jako serwera SMP")
  where
    versionStr = versionString versionNumber
    versionOption = infoOption versionAndUpdate (long "version" <> short 'v' <> help "Pokaż wersję")
    versionAndUpdate = versionStr <> "\n" <> updateStr

mkChatOpts :: BroadcastBotOpts -> ChatOpts
mkChatOpts BroadcastBotOpts {coreOptions} =
  ChatOpts
    { coreOptions,
      chatCmd = "",
      chatCmdDelay = 3,
      chatServerPort = Nothing,
      optFilesFolder = Nothing,
      allowInstantFiles = True,
      maintenance = False
    }
