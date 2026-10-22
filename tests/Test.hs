{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Bots.StoreService.BTCPayTests
import Bots.StoreService.BotTests
import Bots.StoreService.CatalogTests
import Bots.StoreService.ConfigTests
import Bots.StoreService.GroupIntegrationTests
import Bots.StoreService.GroupTests
import Bots.StoreService.NameRegistrationTests
import Bots.StoreService.NamesBalancesTests
import Bots.StoreService.NamesLateCreditTests
import Bots.StoreService.NamesChainTests
import Bots.StoreService.NamesCodesTests
import Bots.StoreService.NamesContractsTests
import Bots.StoreService.NamesLabelTests
import Bots.StoreService.NamesLinksTests
import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.NamesOwnerTests
import Bots.StoreService.NamesPhasesTests
import Bots.StoreService.NamesProfilesTests
import Bots.StoreService.NamesRecordsTests
import Bots.StoreService.NamesRevealTests
import Bots.StoreService.NamesSearchTests
import Bots.StoreService.NamesSenderTests
import Bots.StoreService.NamesSignatureTests
import Bots.StoreService.NamesSpendTests
import Bots.StoreService.NamesStoreTests
import Bots.StoreService.NamesWiringTests
import Bots.StoreService.NamesWorkerTests
import Bots.StoreService.StripeTests
import Bots.StoreService.WaitersTests
import Bots.StoreService.WebTests
import Bots.BroadcastTests
import Bots.DirectoryTests
import ChatClient
import ChatTests
import ChatTests.DBUtils
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
#if !MIN_VERSION_hspec(2,10,10)
import Test.Hspec.Core.Spec (sequential)
#endif
import UnliftIO.Temporary (withTempDirectory)
import ValidNames
import ViewTests
import WalletTests
#if defined(dbPostgres)
import Control.Exception (bracket_, finally)
import PostgresSchemaDump
import Simplex.Chat.Store.Postgres.Migrations (migrations)
import Simplex.Messaging.Agent.Store.Postgres.Util (createDBAndUserIfNotExists, dropDatabaseAndUser)
#else
import APIDocs
import ChatTests.Names (chatNamesTests)
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
      describe "SimpleX store service" $ do
        badgeConfigTests
        badgeWebTests
        badgeCatalogTests
        badgeGroupTests
        badgeWaitersTests
        badgeBTCPayTests
        badgeStripeTests
        badgeNameRegistrationTests
      namesConfigTests
      namesBalancesTests
      namesLateCreditTests
      namesChainTests
      namesCodesTests
      namesContractsTests
      namesLabelTests
      namesLinksTests
      namesOrdersTests
      namesOwnerTests
      namesPhasesTests
      namesProfilesTests
      namesRecordsTests
      namesRevealTests
      namesSearchTests
      namesSenderTests
      namesSignatureTests
      namesSpendTests
      namesStoreTests
      namesWiringTests
      namesWorkerTests
      describe "SimpleX chat markdown" markdownTests
      describe "JSON Tests" jsonTests
      describe "Member relations" memberRelationsTests
      describe "SimpleX chat view" viewTests
      describe "Wallet derivation" walletDerivationTests
      describe "SimpleX chat protocol" protocolTests
      describe "Valid names" validNameTests
      describe "Message batching" batchingTests
      describe "Operators" operatorTests
      describe "Random servers" randomServersTests
#if !defined(dbPostgres)
      around (tmpTestBracket chatQueryStats agentQueryStats portBases) $ describe "names tests" chatNamesTests
      around (tmpTestBracket chatQueryStats agentQueryStats portBases) $ describe "SimpleX Directory names" directoryNameTests
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
          describe "Wallet" walletTests
          xdescribe'' "SimpleX Broadcast bot" broadcastBotTests
          describe "SimpleX Directory service bot" directoryServiceTests
          describe "SimpleX store service e2e" $ do
            storeServiceTests
            describe "managed group" badgeGroupIntegrationTests
          describe "StoreService names: boot" namesBootTests
          describe "StoreService names: worker" namesWorkerBootTests
          namesCodesChatTests
          namesProfilesChatTests
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
