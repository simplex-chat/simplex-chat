import ChatClient
import ChatTests
-- import Control.Logger.Simple
import Control.Concurrent (threadDelay)
import MarkdownTests
import MobileTests
import ProtocolTests
import SchemaDump
import Test.Hspec

main :: IO ()
main = do
  -- setLogLevel LogDebug -- LogError
  -- withGlobalLogging logCfg $
  withSmpServer . hspec $ do
    describe "SimpleX chat markdown" markdownTests
    describe "SimpleX chat protocol" protocolTests
    describe "Mobile API Tests" mobileTests
    -- Workaround for SQLite IO error in first test after mobile tests on Mac
    it "Delay after Mobile API Tests" $ threadDelay 100000
    describe "SimpleX chat client" chatTests
    describe "Schema dump" schemaDumpTest

-- logCfg :: LogConfig
-- logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}
