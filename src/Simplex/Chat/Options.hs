{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Options
  ( ChatOpts (..),
    CoreChatOpts (..),
    chatOptsP,
    coreChatOptsP,
    getChatOpts,
    protocolServersP,
    fullNetworkConfig,
  )
where

import Control.Logger.Simple (LogLevel (..))
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import Numeric.Natural (Natural)
import Options.Applicative
import Simplex.Chat.Controller (ChatLogLevel (..), updateStr, versionNumber, versionString)
import Simplex.FileTransfer.Description (mb)
import Simplex.Messaging.Client (NetworkConfig (..), defaultNetworkConfig)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Protocol (ProtoServerWithAuth, ProtocolTypeI, SMPServerWithAuth, XFTPServerWithAuth)
import Simplex.Messaging.Transport.Client (SocksProxy, defaultSocksProxy)
import System.FilePath (combine)

data ChatOpts = ChatOpts
  { coreOptions :: CoreChatOpts,
    deviceName :: Maybe Text,
    chatCmd :: String,
    chatCmdDelay :: Int,
    chatServerPort :: Maybe String,
    optFilesFolder :: Maybe FilePath,
    showReactions :: Bool,
    allowInstantFiles :: Bool,
    autoAcceptFileSize :: Integer,
    muteNotifications :: Bool,
    markRead :: Bool,
    maintenance :: Bool
  }

data CoreChatOpts = CoreChatOpts
  { dbFilePrefix :: String,
    dbKey :: ScrubbedBytes,
    smpServers :: [SMPServerWithAuth],
    xftpServers :: [XFTPServerWithAuth],
    networkConfig :: NetworkConfig,
    logLevel :: ChatLogLevel,
    logConnections :: Bool,
    logServerHosts :: Bool,
    logAgent :: Maybe LogLevel,
    logFile :: Maybe FilePath,
    tbqSize :: Natural,
    highlyAvailable :: Bool
  }

agentLogLevel :: ChatLogLevel -> LogLevel
agentLogLevel = \case
  CLLDebug -> LogDebug
  CLLInfo -> LogInfo
  CLLWarning -> LogWarn
  CLLError -> LogError
  CLLImportant -> LogInfo

coreChatOptsP :: FilePath -> FilePath -> Parser CoreChatOpts
coreChatOptsP appDir defaultDbFileName = do
  dbFilePrefix <-
    strOption
      ( long "database"
          <> short 'd'
          <> metavar "DB_FILE"
          <> help "Path prefix to chat and agent database files"
          <> value defaultDbFilePath
          <> showDefault
      )
  dbKey <-
    strOption
      ( long "key"
          <> short 'k'
          <> metavar "KEY"
          <> help "Database encryption key/pass-phrase"
          <> value ""
      )
  smpServers <-
    option
      parseProtocolServers
      ( long "server"
          <> short 's'
          <> metavar "SERVER"
          <> help
            ( ("Space-separated list of SMP server(s) to use (each server can have more than one hostname)." <> "\n")
                <> ("If you pass multiple servers, surround the entire list in quotes." <> "\n")
                <> "Examples: smp1.example.com, \"smp1.example.com smp2.example.com smp3.example.com\""
            )
          <> value []
      )
  xftpServers <-
    option
      parseProtocolServers
      ( long "xftp-server"
          <> metavar "SERVER"
          <> help
            ( ("Space-separated list of XFTP server(s) to use (each server can have more than one hostname)." <> "\n")
                <> ("If you pass multiple servers, surround the entire list in quotes." <> "\n")
                <> "Examples: xftp1.example.com, \"xftp1.example.com xftp2.example.com xftp3.example.com\""
            )
          <> value []
      )
  socksProxy <-
    flag' (Just defaultSocksProxy) (short 'x' <> help "Use local SOCKS5 proxy at :9050")
      <|> option
        parseSocksProxy
        ( long "socks-proxy"
            <> metavar "SOCKS5"
            <> help "Use SOCKS5 proxy at `ipv4:port` or `:port`"
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
  logLevel <-
    option
      parseLogLevel
      ( long "log-level"
          <> short 'l'
          <> metavar "LEVEL"
          <> help "Log level: debug, info, warn, error, important (default)"
          <> value CLLImportant
      )
  logTLSErrors <-
    switch
      ( long "log-tls-errors"
          <> help "Log TLS errors (also enabled with `-l debug`)"
      )
  logConnections <-
    switch
      ( long "connections"
          <> short 'c'
          <> help "Log every contact and group connection on start (also with `-l info`)"
      )
  logServerHosts <-
    switch
      ( long "log-hosts"
          <> help "Log connections to servers (also with `-l info`)"
      )
  logAgent <-
    switch
      ( long "log-agent"
          <> help "Enable logs from SMP agent (also with `-l debug`)"
      )
  logFile <-
    optional $
      strOption
        ( long "log-file"
            <> help "Log to specified file / device"
        )
  tbqSize <-
    option
      auto
      ( long "queue-size"
          <> short 'q'
          <> metavar "SIZE"
          <> help "Internal queue size"
          <> value 1024
          <> showDefault
      )
  highlyAvailable <-
    switch
      ( long "ha"
          <> help "Run as a highly available client (this may increase traffic in groups)"
      )
  pure
    CoreChatOpts
      { dbFilePrefix,
        dbKey,
        smpServers,
        xftpServers,
        networkConfig = fullNetworkConfig socksProxy (useTcpTimeout socksProxy t) (logTLSErrors || logLevel == CLLDebug),
        logLevel,
        logConnections = logConnections || logLevel <= CLLInfo,
        logServerHosts = logServerHosts || logLevel <= CLLInfo,
        logAgent = if logAgent || logLevel == CLLDebug then Just $ agentLogLevel logLevel else Nothing,
        logFile,
        tbqSize,
        highlyAvailable
      }
  where
    useTcpTimeout p t = 1000000 * if t > 0 then t else maybe 5 (const 10) p
    defaultDbFilePath = combine appDir defaultDbFileName

chatOptsP :: FilePath -> FilePath -> Parser ChatOpts
chatOptsP appDir defaultDbFileName = do
  coreOptions <- coreChatOptsP appDir defaultDbFileName
  deviceName <-
    optional $
      strOption
        ( long "device-name"
            <> metavar "DEVICE"
            <> help "Device name to use in connections with remote hosts and controller"
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
  optFilesFolder <-
    optional $
      strOption
        ( long "files-folder"
            <> metavar "FOLDER"
            <> help "Folder to use for sent and received files"
        )
  showReactions <-
    switch
      ( long "reactions"
          <> help "Show message reactions"
      )
  allowInstantFiles <-
    switch
      ( long "allow-instant-files"
          <> short 'f'
          <> help "Send and receive instant files without acceptance"
      )
  autoAcceptFileSize <-
    flag' (mb 1) (short 'a' <> help "Automatically accept files up to 1MB")
      <|> option
        auto
        ( long "auto-accept-files"
            <> metavar "FILE_SIZE"
            <> help "Automatically accept files up to specified size"
            <> value 0
        )
  muteNotifications <-
    switch
      ( long "mute"
          <> help "Mute notifications"
      )
  markRead <-
    switch
      ( long "mark-read"
          <> short 'r'
          <> help "Mark shown messages as read"
      )
  maintenance <-
    switch
      ( long "maintenance"
          <> short 'm'
          <> help "Run in maintenance mode (/_start to start chat)"
      )
  pure
    ChatOpts
      { coreOptions,
        deviceName,
        chatCmd,
        chatCmdDelay,
        chatServerPort,
        optFilesFolder,
        showReactions,
        allowInstantFiles,
        autoAcceptFileSize,
        muteNotifications,
        markRead,
        maintenance
      }

fullNetworkConfig :: Maybe SocksProxy -> Int -> Bool -> NetworkConfig
fullNetworkConfig socksProxy tcpTimeout logTLSErrors =
  let tcpConnectTimeout = (tcpTimeout * 3) `div` 2
   in defaultNetworkConfig {socksProxy, tcpTimeout, tcpConnectTimeout, logTLSErrors}

parseProtocolServers :: ProtocolTypeI p => ReadM [ProtoServerWithAuth p]
parseProtocolServers = eitherReader $ parseAll protocolServersP . B.pack

parseSocksProxy :: ReadM (Maybe SocksProxy)
parseSocksProxy = eitherReader $ parseAll strP . B.pack

parseServerPort :: ReadM (Maybe String)
parseServerPort = eitherReader $ parseAll serverPortP . B.pack

serverPortP :: A.Parser (Maybe String)
serverPortP = Just . B.unpack <$> A.takeWhile A.isDigit

protocolServersP :: ProtocolTypeI p => A.Parser [ProtoServerWithAuth p]
protocolServersP = strP `A.sepBy1` A.char ' '

parseLogLevel :: ReadM ChatLogLevel
parseLogLevel = eitherReader $ \case
  "debug" -> Right CLLDebug
  "info" -> Right CLLInfo
  "warn" -> Right CLLWarning
  "error" -> Right CLLError
  "important" -> Right CLLImportant
  _ -> Left "Invalid log level"

getChatOpts :: FilePath -> FilePath -> IO ChatOpts
getChatOpts appDir defaultDbFileName =
  execParser $
    info
      (helper <*> versionOption <*> chatOptsP appDir defaultDbFileName)
      (header versionStr <> fullDesc <> progDesc "Start chat with DB_FILE file and use SERVER as SMP server")
  where
    versionStr = versionString versionNumber
    versionOption = infoOption versionAndUpdate (long "version" <> short 'v' <> help "Show version")
    versionAndUpdate = versionStr <> "\n" <> updateStr
