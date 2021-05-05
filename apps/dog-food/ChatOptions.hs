{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatOptions (getChatOpts, ChatOpts (..)) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Options.Applicative
import Simplex.Messaging.Agent.Transmission (SMPServer (..), smpServerP)
import Simplex.Messaging.Parsers (parseAll)
import System.FilePath (combine)
import Types

data ChatOpts = ChatOpts
  { dbFile :: String,
    smpServers :: NonEmpty SMPServer,
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
          <> help "SMP server(s) to use (smp1.simplex.im:5223#pLdiGvm0jD1CMblnov6Edd/391OrYsShw+RgdfR0ChA=)"
          <> value (L.fromList ["smp1.simplex.im:5223#pLdiGvm0jD1CMblnov6Edd/391OrYsShw+RgdfR0ChA="])
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

parseSMPServer :: ReadM (NonEmpty SMPServer)
parseSMPServer = eitherReader $ parseAll servers . B.pack
  where
    servers = L.fromList <$> smpServerP `A.sepBy1` A.char ','

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
