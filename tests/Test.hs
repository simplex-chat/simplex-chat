import ChatClient
-- import Control.Logger.Simple
import ChatTests
import Data.Time.Clock.System
import MarkdownTests
import MobileTests
import ProtocolTests
import SchemaDump
import Test.Hspec
import UnliftIO.Temporary (withTempDirectory)

main :: IO ()
main = do
  -- setLogLevel LogDebug -- LogError
  -- withGlobalLogging logCfg $
  hspec $ do
    describe "SimpleX chat markdown" markdownTests
    describe "SimpleX chat protocol" protocolTests
    describe "Schema dump" schemaDumpTest
    around testBracket $ do
      describe "Mobile API Tests" mobileTests
      describe "SimpleX chat client" chatTests
  where
    testBracket test = do
      t <- getSystemTime
      let ts = show $ systemSeconds t * 1E3 + fromIntegral (systemNanoseconds t `div` 1E6)
      withSmpServer $ withTmpFiles $ withTempDirectory "tests" ("tmp" <> ts) test

-- logCfg :: LogConfig
-- logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}
