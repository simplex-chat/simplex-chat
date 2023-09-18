import ChatClient
import ChatTests
import Control.Logger.Simple
import Data.Time.Clock.System
import MarkdownTests
import MobileTests
import ProtocolTests
import SchemaDump
import Test.Hspec
import UnliftIO.Temporary (withTempDirectory)
import ViewTests
import WebRTCTests

main :: IO ()
main = do
  setLogLevel LogError -- LogDebug
  withGlobalLogging logCfg . hspec $ do
    describe "SimpleX chat markdown" markdownTests
    describe "SimpleX chat view" viewTests
    describe "SimpleX chat protocol" protocolTests
    describe "WebRTC encryption" webRTCTests
    describe "Schema dump" schemaDumpTest
    around testBracket $ do
      describe "Mobile API Tests" mobileTests
      describe "SimpleX chat client" chatTests
  where
    testBracket test = do
      t <- getSystemTime
      let ts = show (systemSeconds t) <> show (systemNanoseconds t)
      withSmpServer $ withTmpFiles $ withTempDirectory "tests/tmp" ts test

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}
