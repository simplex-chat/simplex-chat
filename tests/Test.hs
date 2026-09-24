{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Bots.BadgeServiceTests
import Bots.BroadcastTests
import Bots.DirectoryTests
import ChatClient
import ChatTests
import ChatTests.DBUtils
import ChatTests.Names (chatNamesTests)
import ChatTests.Utils (xdescribe'')
import Control.Logger.Simple
import Data.Time.Clock.System
import BadgeTests
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
import Control.Exception (bracket_, finally)
import PostgresSchemaDump
import Simplex.Chat.Store.Postgres.Migrations (migrations)
import Simplex.Messaging.Agent.Store.Postgres.Util (createDBAndUserIfNotExists, dropDatabaseAndUser)
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
  portBases <- newPortBases
  withTestDB . withTmpFiles . withGlobalLogging logCfg . hspec . parallel
    $ do
#if defined(dbPostgres)
      sequential $
        describe "Postgres schema dump" $
          postgresSchemaDumpTest
            migrations
            schemaDumpDBOpts
            "src/Simplex/Chat/Store/Postgres/Migrations/chat_schema.sql"
#else
      sequential $ describe "Schema dump" schemaDumpTest
#if MIN_VERSION_base(4,18,0)
      sequential $ describe "Bot API docs" apiDocsTest
#endif
      around tmpBracket $ describe "WebRTC encryption" webRTCTests
#endif
      describe "Supporter badges" badgeTests
      describe "SimpleX chat markdown" markdownTests
      describe "JSON Tests" jsonTests
      describe "Member relations" memberRelationsTests
      describe "SimpleX chat view" viewTests
      describe "SimpleX chat protocol" protocolTests
      describe "Valid names" validNameTests
      describe "Message batching" batchingTests
      describe "Operators" operatorTests
      describe "Random servers" randomServersTests
#if !defined(dbPostgres)
      around (tmpTestBracket chatQueryStats agentQueryStats portBases) $ describe "names tests" chatNamesTests
      around (tmpTestBracket chatQueryStats agentQueryStats portBases) $ xdescribe'' "SimpleX Directory names" directoryNameTests
#endif
#if defined(dbPostgres)
      around (testBracket portBases)
#else
      around (testBracket chatQueryStats agentQueryStats portBases)
#endif
        $ do
#if !defined(dbPostgres)
          describe "Mobile API Tests" mobileTests
#endif
          describe "SimpleX chat client" chatTests
          xdescribe'' "SimpleX Broadcast bot" broadcastBotTests
          xdescribe'' "SimpleX Directory service bot" directoryServiceTests
          xdescribe'' "SimpleX Badge service bot" badgeServiceTests
          describe "Remote session" remoteTests
#if !defined(dbPostgres)
          sequential $ xdescribe'' "Save query plans" saveQueryPlans
#endif
  where
#if defined(dbPostgres)
    withTestDB =
      bracket_
        (dropDatabaseAndUser testDBConnectInfo >> createDBAndUserIfNotExists testDBConnectInfo)
        (dropDatabaseAndUser testDBConnectInfo)
    testBracket portBases test =
      withPortBase portBases $ \portBase -> tmpBracket $ \tmpPath -> do
        let ps = TestParams {tmpPath, portBase, printOutput = False}
        withSmpServer ps (test ps) `finally` dropTestSchemas ps
#else
    withTestDB = id
    testBracket chatQueryStats agentQueryStats portBases test =
      tmpTestBracket chatQueryStats agentQueryStats portBases $ \ps -> withSmpServer ps $ test ps
    tmpTestBracket chatQueryStats agentQueryStats portBases test =
      withPortBase portBases $ \portBase -> tmpBracket $ \tmpPath -> test TestParams {tmpPath, portBase, chatQueryStats, agentQueryStats, printOutput = False}
#endif
    tmpBracket test = do
      t <- getSystemTime
      let ts = show (systemSeconds t) <> show (systemNanoseconds t)
      withTempDirectory "tests/tmp" ts test

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}
