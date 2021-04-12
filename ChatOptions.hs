{-# LANGUAGE LambdaCase #-}

module ChatOptions (getChatOpts, ChatOpts (..)) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Options.Applicative
import Simplex.Messaging.Agent.Transmission (SMPServer (..), smpServerP)
import System.FilePath (combine)
import Types

data ChatOpts = ChatOpts
  { dbFileName :: String,
    smpServer :: SMPServer,
    termMode :: TermMode
  }

chatOpts :: FilePath -> Parser ChatOpts
chatOpts appDir =
  ChatOpts
    <$> strOption
      ( long "database"
          <> short 'd'
          <> metavar "DB_FILE"
          <> help ("sqlite database file path (" <> defaultDbFilePath <> ")")
          <> value defaultDbFilePath
      )
    <*> option
      parseSMPServer
      ( long "server"
          <> short 's'
          <> metavar "SERVER"
          <> help "SMP server to use (smp.simplex.im:5223)"
          <> value (SMPServer "smp.simplex.im" (Just "5223") Nothing)
      )
    <*> option
      parseTermMode
      ( long "term"
          <> short 't'
          <> metavar "TERM"
          <> help ("terminal mode: editor or basic (" <> termModeName TermModeEditor <> ")")
          <> value TermModeEditor
      )
  where
    defaultDbFilePath = combine appDir "smp-chat.db"

parseSMPServer :: ReadM SMPServer
parseSMPServer = eitherReader $ A.parseOnly (smpServerP <* A.endOfInput) . B.pack

parseTermMode :: ReadM TermMode
parseTermMode = maybeReader $ \case
  "basic" -> Just TermModeBasic
  "editor" -> Just TermModeEditor
  _ -> Nothing

getChatOpts :: FilePath -> IO ChatOpts
getChatOpts appDir = execParser opts
  where
    opts =
      info
        (chatOpts appDir <**> helper)
        ( fullDesc
            <> header "Chat prototype using Simplex Messaging Protocol (SMP)"
            <> progDesc "Start chat with DB_FILE file and use SERVER as SMP server"
        )
