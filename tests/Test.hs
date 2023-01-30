import ChatClient
-- import Control.Logger.Simple
import ChatTests
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
  hspec $ do
    describe "SimpleX chat markdown" markdownTests
    describe "SimpleX chat protocol" protocolTests
    describe "Schema dump" schemaDumpTest
    around_ withTmpFiles . around_ withSmpServer $ do
      describe "Mobile API Tests" mobileTests
      -- -- Workaround for SQLite IO error in first test after mobile tests on Mac
      -- it "Delay after Mobile API Tests" $ threadDelay 100000
      describe "SimpleX chat client" chatTests

-- logCfg :: LogConfig
-- logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}
