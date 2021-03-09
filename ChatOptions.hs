{-# LANGUAGE LambdaCase #-}

module ChatOptions (getChatOpts, ChatOpts (..)) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Options.Applicative
import Simplex.Messaging.Agent.Transmission (SMPServer (..), smpServerP)
import Types

data ChatOpts = ChatOpts
  { name :: Maybe B.ByteString,
    dbFileName :: String,
    smpServer :: SMPServer,
    termMode :: TermMode
  }

chatOpts :: Parser ChatOpts
chatOpts =
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
          <> help "sqlite database filename (smp-chat.db)"
          <> value "smp-chat.db"
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
          <> help "terminal mode: \"editor\", \"simple\" or \"basic\" (editor)"
          <> value TermModeEditor
      )

parseSMPServer :: ReadM SMPServer
parseSMPServer = eitherReader $ A.parseOnly (smpServerP <* A.endOfInput) . B.pack

parseTermMode :: ReadM TermMode
parseTermMode = maybeReader $ \case
  "basic" -> Just TermModeBasic
  "simple" -> Just TermModeSimple
  "editor" -> Just TermModeEditor
  _ -> Nothing

getChatOpts :: IO ChatOpts
getChatOpts = execParser opts
  where
    opts =
      info
        (chatOpts <**> helper)
        ( fullDesc
            <> header "Chat prototype using Simplex Messaging Protocol (SMP)"
            <> progDesc "Start chat with DB_FILE file and use SERVER as SMP server"
        )
