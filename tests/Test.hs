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
import MobileTests
import ProtocolTests
import RemoteTests
import SchemaDump
import Test.Hspec
import UnliftIO.Temporary (withTempDirectory)
import ValidNames
import ViewTests
import WebRTCTests

main :: IO ()
main = do
  setLogLevel LogError
  withGlobalLogging logCfg . hspec $ do
    describe "Schema dump" schemaDumpTest
    describe "SimpleX chat markdown" markdownTests
    describe "JSON Tests" jsonTests
    describe "SimpleX chat view" viewTests
    describe "SimpleX chat protocol" protocolTests
    around tmpBracket $ describe "WebRTC encryption" webRTCTests
    describe "Valid names" validNameTests
    describe "Message batching" batchingTests
    around testBracket $ do
      describe "Mobile API Tests" mobileTests
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
