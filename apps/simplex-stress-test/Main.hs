module Main where

import Simplex.StressTest
import System.Directory
import Test.Hspec
import Control.Monad
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dirExists <- doesDirectoryExist "test"
  when dirExists $ removeDirectoryRecursive "test"
  createDirectoryIfMissing True "test"
  hspec $ describe "SimpleX chat client" chatTests
