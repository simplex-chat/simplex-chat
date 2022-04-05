{-# LANGUAGE OverloadedStrings #-}

module SchemaDump where

import ChatClient (withTmpFiles)
import Control.Monad (void)
import Simplex.Chat.Store (createStore)
import System.Process (readCreateProcess, shell)
import Test.Hspec

testDB :: FilePath
testDB = "tests/tmp/test_chat.db"

schema :: FilePath
schema = "chat_schema.sql"

schemaDumpTest :: Spec
schemaDumpTest =
  it "verify and overwrite schema dump" testVerifySchemaDump

testVerifySchemaDump :: IO ()
testVerifySchemaDump =
  withTmpFiles $ do
    void $ createStore testDB 1 False
    void $ readCreateProcess (shell $ "touch " <> schema) ""
    savedSchema <- readFile schema
    savedSchema `seq` pure ()
    void $ readCreateProcess (shell $ "sqlite3 " <> testDB <> " .schema > " <> schema) ""
    currentSchema <- readFile schema
    savedSchema `shouldBe` currentSchema
