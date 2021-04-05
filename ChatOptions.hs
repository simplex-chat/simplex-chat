{-# LANGUAGE LambdaCase #-}

module ChatOptions (getChatOpts, ChatOpts (..)) where

import qualified Data.ByteString.Char8 as B
import Options.Applicative
import Simplex.Messaging.Agent.Transmission (SMPServer (..), smpServerP)
import Simplex.Messaging.Parsers (parseAll)
import System.FilePath (combine)
import System.Info (os)
import Types

data ChatOpts = ChatOpts
  { name :: Maybe B.ByteString,
    dbFileName :: String,
    smpServer :: SMPServer,
    termMode :: TermMode
  }

chatOpts :: FilePath -> Parser ChatOpts
chatOpts appDir =
  ChatOpts
    <$> option
      (Just <$> str)
      ( long "name"
          <> short 'n'
          <> metavar "NAME"
          <> help "optional name to use for invitations"
          <> value Nothing
      )
    <*> strOption
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
          <> help ("terminal mode: editor or basic (" <> termModeName deafultTermMode <> ")")
          <> value deafultTermMode
      )
  where
    defaultDbFilePath = combine appDir "smp-chat.db"
    deafultTermMode
      | os == "mingw32" = TermModeBasic
      | otherwise = TermModeEditor

parseSMPServer :: ReadM SMPServer
parseSMPServer = eitherReader $ parseAll smpServerP . B.pack

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
