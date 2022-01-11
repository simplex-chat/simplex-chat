{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Options (getChatOpts, ChatOpts (..)) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Options.Applicative
import Simplex.Messaging.Agent.Protocol (SMPServer (..))
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (parseAll)
import System.FilePath (combine)

data ChatOpts = ChatOpts
  { dbFile :: String,
    smpServers :: NonEmpty SMPServer
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
          <> help
            ( "SMP server(s) to use"
                <> "\n(smp2.simplex.im,smp3.simplex.im)"
            )
          <> value
            ( L.fromList
                [ "smp://CTMzyymBBawF0yuMln3UxTip6RgFVtYPL8UYuCoIBwE=@139.162.205.110", -- London, UK
                  "smp://t82czNx4tiftzbk_M4KEITL1RS9CmcTWiCLHSlNsEZ8=@96.126.97.196" -- Fremont, CA
                ]
            )
      )
  where
    defaultDbFilePath = combine appDir "simplex_v1"

parseSMPServer :: ReadM (NonEmpty SMPServer)
parseSMPServer = eitherReader $ parseAll servers . B.pack
  where
    servers = L.fromList <$> strP `A.sepBy1` A.char ','

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
