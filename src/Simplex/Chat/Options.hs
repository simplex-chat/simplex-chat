{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Options
  ( ChatOpts (..),
    getChatOpts,
    defaultSMPServers,
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Options.Applicative
import Simplex.Chat.Controller (updateStr, versionStr)
import Simplex.Messaging.Agent.Protocol (SMPServer (..))
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (parseAll)
import System.FilePath (combine)

data ChatOpts = ChatOpts
  { dbFilePrefix :: String,
    smpServers :: NonEmpty SMPServer,
    logConnections :: Bool,
    logAgent :: Bool
  }

defaultSMPServers :: NonEmpty SMPServer
defaultSMPServers =
  L.fromList
    [ "smp://u2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU=@smp4.simplex.im",
      "smp://hpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg=@smp5.simplex.im",
      "smp://PQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo=@smp6.simplex.im"
      -- "smp://Tn1b3Rr7_gErbVt2v50Y_T-PvUAi1BYAMS-62w-k9CI=@139.162.240.237"
    ]

chatOpts :: FilePath -> Parser ChatOpts
chatOpts appDir =
  ChatOpts
    <$> strOption
      ( long "database"
          <> short 'd'
          <> metavar "DB_FILE"
          <> help "Path prefix to chat and agent database files"
          <> value defaultDbFilePath
          <> showDefault
      )
    <*> option
      parseSMPServer
      ( long "server"
          <> short 's'
          <> metavar "SERVER"
          <> help
            "Comma separated list of SMP server(s) to use \
            \(default: smp4.simplex.im,smp5.simplex.im,smp6.simplex.im)"
          <> value defaultSMPServers
      )
    <*> switch
      ( long "connections"
          <> short 'c'
          <> help "Log every contact and group connection on start"
      )
    <*> switch
      ( long "log-agent"
          <> short 'l'
          <> help "Enable logs from SMP agent"
      )
  where
    defaultDbFilePath = combine appDir "simplex_v1"

parseSMPServer :: ReadM (NonEmpty SMPServer)
parseSMPServer = eitherReader $ parseAll servers . B.pack
  where
    servers = L.fromList <$> strP `A.sepBy1` A.char ','

getChatOpts :: FilePath -> IO ChatOpts
getChatOpts appDir =
  execParser $
    info
      (helper <*> versionOption <*> chatOpts appDir)
      (header versionStr <> fullDesc <> progDesc "Start chat with DB_FILE file and use SERVER as SMP server")
  where
    versionOption = infoOption versionAndUpdate (long "version" <> short 'v' <> help "Show version")
    versionAndUpdate = versionStr <> "\n" <> updateStr
