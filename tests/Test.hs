{-# LANGUAGE CPP #-}

import Bots.BroadcastTests
import Bots.DirectoryTests
import ChatClient
import ChatTests
import ChatTests.Utils (xdescribe'')
import Control.Logger.Simple
import Data.Time.Clock.System
import JSONTests
import MarkdownTests
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
import Simplex.Messaging.Agent.Store.Postgres.Util (createDBAndUserIfNotExists, dropAllSchemasExceptSystem, dropDatabaseAndUser)
#else
import MobileTests
import SchemaDump
import WebRTCTests
#endif

main :: IO ()
main = do
  setLogLevel LogError
  withGlobalLogging logCfg . hspec
#if defined(dbPostgres)
    . beforeAll_ (dropDatabaseAndUser testDBConnectInfo >> createDBAndUserIfNotExists testDBConnectInfo)
    . afterAll_ (dropDatabaseAndUser testDBConnectInfo)
#endif
    $ do
#if !defined(dbPostgres)
      describe "Schema dump" schemaDumpTest
      around tmpBracket $ describe "WebRTC encryption" webRTCTests
#endif
      describe "SimpleX chat markdown" markdownTests
      describe "JSON Tests" jsonTests
      describe "SimpleX chat view" viewTests
      describe "SimpleX chat protocol" protocolTests
      describe "Valid names" validNameTests
      describe "Message batching" batchingTests
      describe "Operators" operatorTests
      describe "Random servers" randomServersTests
      around testBracket
#if defined(dbPostgres)
        . after_ (dropAllSchemasExceptSystem testDBConnectInfo)
#endif
        $ do
#if !defined(dbPostgres)
          describe "Mobile API Tests" mobileTests
#endif
          describe "SimpleX chat client" chatTests
          xdescribe'' "SimpleX Broadcast bot" broadcastBotTests
          xdescribe'' "SimpleX Directory service bot" directoryServiceTests
          describe "Remote session" remoteTests
  where
    testBracket test = withSmpServer $ tmpBracket test
    tmpBracket test = do
      t <- getSystemTime
      let ts = show (systemSeconds t) <> show (systemNanoseconds t)
      withTmpFiles $ withTempDirectory "tests/tmp" ts test

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}
