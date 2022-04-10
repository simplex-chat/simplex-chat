{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Options
  ( ChatOpts (..),
    getChatOpts,
    smpServersP,
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Options.Applicative
import Simplex.Chat.Controller (updateStr, versionStr)
import Simplex.Messaging.Agent.Protocol (SMPServer (..))
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (parseAll)
import System.FilePath (combine)

data ChatOpts = ChatOpts
  { dbFilePrefix :: String,
    smpServers :: [SMPServer],
    logConnections :: Bool,
    logAgent :: Bool,
    chatCmd :: String,
    chatCmdDelay :: Int
  }

chatOpts :: FilePath -> FilePath -> Parser ChatOpts
chatOpts appDir defaultDbFileName = do
  dbFilePrefix <-
    strOption
      ( long "database"
          <> short 'd'
          <> metavar "DB_FILE"
          <> help "Path prefix to chat and agent database files"
          <> value defaultDbFilePath
          <> showDefault
      )
  smpServers <-
    option
      parseSMPServers
      ( long "server"
          <> short 's'
          <> metavar "SERVER"
          <> help
            "Comma separated list of SMP server(s) to use"
          <> value []
      )
  logConnections <-
    switch
      ( long "connections"
          <> short 'c'
          <> help "Log every contact and group connection on start"
      )
  logAgent <-
    switch
      ( long "log-agent"
          <> short 'l'
          <> help "Enable logs from SMP agent"
      )
  chatCmd <-
    strOption
      ( long "execute"
          <> short 'e'
          <> metavar "COMMAND"
          <> help "Execute chat command (received messages won't be logged) and exit"
          <> value ""
      )
  chatCmdDelay <-
    option
      auto
      ( long "time"
          <> short 't'
          <> metavar "TIME"
          <> help "Time to wait after sending chat command before exiting, seconds"
          <> value 3
          <> showDefault
      )
  pure ChatOpts {dbFilePrefix, smpServers, logConnections, logAgent, chatCmd, chatCmdDelay}
  where
    defaultDbFilePath = combine appDir defaultDbFileName

parseSMPServers :: ReadM [SMPServer]
parseSMPServers = eitherReader $ parseAll smpServersP . B.pack

smpServersP :: A.Parser [SMPServer]
smpServersP = strP `A.sepBy1` A.char ','

getChatOpts :: FilePath -> FilePath -> IO ChatOpts
getChatOpts appDir defaultDbFileName =
  execParser $
    info
      (helper <*> versionOption <*> chatOpts appDir defaultDbFileName)
      (header versionStr <> fullDesc <> progDesc "Start chat with DB_FILE file and use SERVER as SMP server")
  where
    versionOption = infoOption versionAndUpdate (long "version" <> short 'v' <> help "Show version")
    versionAndUpdate = versionStr <> "\n" <> updateStr
