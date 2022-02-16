module Main where

import Simplex.StressTest
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import Test.Hspec

main :: IO ()
main = do
  createDirectoryIfMissing False "tests/tmp"
  hspec $ describe "SimpleX chat client" chatTests
  removeDirectoryRecursive "tests/tmp"
