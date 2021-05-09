import MarkdownTests
import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    describe "SimpleX chat markdown" markdownTests
