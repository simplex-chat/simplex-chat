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
    after_ (threadDelay 2000000) . around_ withTmpFiles . around_ withSmpServer $ do
      describe "Mobile API Tests" mobileTests
      describe "SimpleX chat client" chatTests

-- logCfg :: LogConfig
-- logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}
