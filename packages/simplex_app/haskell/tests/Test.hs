import ChatClient
import ChatTests
import MarkdownTests
import ProtocolTests
import Test.Hspec

main :: IO ()
main = withSmpServer . hspec $ do
  describe "SimpleX chat markdown" markdownTests
  describe "SimpleX chat protocol" protocolTests
  describe "SimpleX chat client" chatTests
