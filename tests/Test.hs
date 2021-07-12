import ChatTests
import MarkdownTests
import ProtocolTests
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import Test.Hspec

main :: IO ()
main = do
  createDirectoryIfMissing False "tests/tmp"
  hspec $ do
    describe "SimpleX chat markdown" markdownTests
    describe "SimpleX chat protocol" protocolTests
    xdescribe "SimpleX chat client" testAddContact
  removeDirectoryRecursive "tests/tmp"
