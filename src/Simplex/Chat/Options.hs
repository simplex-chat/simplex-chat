{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Options
  ( ChatOpts (..),
    getChatOpts,
    smpServersP,
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)
import Network.Socket (HostAddress, SockAddr (..), tupleToHostAddress)
import Network.Socks5 (SocksConf, defaultSocksConf)
import Options.Applicative
import Simplex.Chat.Controller (updateStr, versionStr)
import Simplex.Messaging.Agent.Protocol (SMPServer)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (parseAll)
import System.FilePath (combine)

data ChatOpts = ChatOpts
  { dbFilePrefix :: String,
    smpServers :: [SMPServer],
    socksProxy :: Maybe SocksConf,
    tcpTimeout :: Int,
    logConnections :: Bool,
    logAgent :: Bool,
    chatCmd :: String,
    chatCmdDelay :: Int,
    chatServerPort :: Maybe String,
    maintenance :: Bool
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
          <> help "Comma separated list of SMP server(s) to use"
          <> value []
      )
  socksProxy <-
    flag' (Just defaultSocksProxy) (short 'x' <> help "use local SOCKS5 proxy at :9050")
      <|> option
        parseSocksConf
        ( long "socks-proxy"
            <> metavar "SOCKS5"
            <> help "`ipv4:port` or `:port` of SOCKS5 proxy"
            <> value Nothing
        )
  t <-
    option
      auto
      ( long "tcp-timeout"
          <> metavar "TIMEOUT"
          <> help "TCP timeout, seconds (default: 5/10 without/with SOCKS5 proxy)"
          <> value 0
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
  chatServerPort <-
    option
      parseServerPort
      ( long "chat-server-port"
          <> short 'p'
          <> metavar "PORT"
          <> help "Run chat server on specified port"
          <> value Nothing
      )
  maintenance <-
    switch
      ( long "maintenance"
          <> short 'm'
          <> help "Run in maintenance mode (/_start to start chat)"
      )
  pure ChatOpts {dbFilePrefix, smpServers, socksProxy, tcpTimeout = useTcpTimeout socksProxy t, logConnections, logAgent, chatCmd, chatCmdDelay, chatServerPort, maintenance}
  where
    useTcpTimeout p t = 1000000 * if t > 0 then t else maybe 5 (const 10) p
    defaultDbFilePath = combine appDir defaultDbFileName

parseSMPServers :: ReadM [SMPServer]
parseSMPServers = eitherReader $ parseAll smpServersP . B.pack

defaultSocksHost :: HostAddress
defaultSocksHost = tupleToHostAddress (127, 0, 0, 1)

defaultSocksProxy :: SocksConf
defaultSocksProxy = defaultSocksConf $ SockAddrInet 9050 defaultSocksHost

parseSocksConf :: ReadM (Maybe SocksConf)
parseSocksConf = eitherReader $ parseAll socksConfP . B.pack
  where
    socksConfP = do
      host <- maybe defaultSocksHost tupleToHostAddress <$> optional ipv4P
      port <- fromMaybe 9050 <$> optional (A.char ':' *> (fromInteger <$> A.decimal))
      pure . Just . defaultSocksConf $ SockAddrInet port host
    ipv4P = (,,,) <$> ipNum <*> ipNum <*> ipNum <*> A.decimal
    ipNum = A.decimal <* A.char '.'

parseServerPort :: ReadM (Maybe String)
parseServerPort = eitherReader $ parseAll serverPortP . B.pack

serverPortP :: A.Parser (Maybe String)
serverPortP = Just . B.unpack <$> A.takeWhile A.isDigit

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
