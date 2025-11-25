{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Bots.BroadcastTests
import Bots.DirectoryTests
import ChatClient
import ChatTests
import ChatTests.DBUtils
import ChatTests.Utils (xdescribe'')
import Control.Logger.Simple
import Data.Time.Clock.System
import JSONTests
import MarkdownTests
import MemberRelationsTests
import MessageBatching
import ProtocolTests
import OperatorTests
import RandomServers
import RemoteTests
import Test.Hspec hiding (it)
import UnliftIO.Temporary (withTempDirectory)
import ValidNames
import ViewTests
#if defined(dbPostgres)
import Control.Exception (bracket_)
import PostgresSchemaDump
import Simplex.Chat.Store.Postgres.Migrations (migrations)
import Simplex.Messaging.Agent.Store.Postgres.Util (createDBAndUserIfNotExists, dropDatabaseAndUser)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
#else
import APIDocs
import qualified Simplex.Messaging.TMap as TM
import MobileTests
import SchemaDump
import WebRTCTests
#endif

main :: IO ()
main = do
  setLogLevel LogError
#if !defined(dbPostgres)
  chatQueryStats <- TM.emptyIO
  agentQueryStats <- TM.emptyIO
#endif
  withGlobalLogging logCfg . hspec
    $ do
#if defined(dbPostgres)
      createdDropDb . around_ (bracket_ (createDirectoryIfMissing False "tests/tmp") (removePathForcibly "tests/tmp")) $
        describe "Postgres schema dump" $
          postgresSchemaDumpTest
            migrations
            schemaDumpDBOpts
            "src/Simplex/Chat/Store/Postgres/Migrations/chat_schema.sql"
#else
      describe "Schema dump" schemaDumpTest
      describe "Bot API docs" apiDocsTest
      around tmpBracket $ describe "WebRTC encryption" webRTCTests
#endif
      describe "SimpleX chat markdown" markdownTests
      describe "JSON Tests" jsonTests
      describe "Member relations" memberRelationsTests
      describe "SimpleX chat view" viewTests
      describe "SimpleX chat protocol" protocolTests
      describe "Valid names" validNameTests
      describe "Message batching" batchingTests
      describe "Operators" operatorTests
      describe "Random servers" randomServersTests
#if defined(dbPostgres)
      createdDropDb . around testBracket
#else
      around (testBracket chatQueryStats agentQueryStats)
#endif
        $ do
#if !defined(dbPostgres)
          describe "Mobile API Tests" mobileTests
#endif
          describe "SimpleX chat client" chatTests
          xdescribe'' "SimpleX Broadcast bot" broadcastBotTests
          xdescribe'' "SimpleX Directory service bot" directoryServiceTests
          describe "Remote session" remoteTests
#if !defined(dbPostgres)
          xdescribe'' "Save query plans" saveQueryPlans
#endif
  where
#if defined(dbPostgres)
    createdDropDb =
      before_ (dropDatabaseAndUser testDBConnectInfo >> createDBAndUserIfNotExists testDBConnectInfo)
        . after_ (dropDatabaseAndUser testDBConnectInfo)
    testBracket test = withSmpServer $ tmpBracket $ \tmpPath -> test TestParams {tmpPath, printOutput = False}
#else
    testBracket chatQueryStats agentQueryStats test =
      withSmpServer $ tmpBracket $ \tmpPath -> test TestParams {tmpPath, chatQueryStats, agentQueryStats, printOutput = False}
#endif
    tmpBracket test = do
      t <- getSystemTime
      let ts = show (systemSeconds t) <> show (systemNanoseconds t)
      withTmpFiles $ withTempDirectory "tests/tmp" ts test

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}
