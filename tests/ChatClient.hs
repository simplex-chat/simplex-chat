{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatClient where

import Control.Concurrent.Async
import Control.Concurrent.STM (retry)
import Control.Exception (bracket_)
import Control.Monad.Except
import Simplex.Chat
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Chat.Options
import Simplex.Chat.Store
import Simplex.Chat.Types (Profile)
import Simplex.Messaging.Agent.Env.SQLite
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import qualified System.Terminal as C
import System.Terminal.Internal (VirtualTerminal, VirtualTerminalSettings (..), withVirtualTerminal)

testDBPrefix :: FilePath
testDBPrefix = "tests/tmp/test"

opts :: ChatOpts
opts =
  ChatOpts
    { dbFile = undefined,
      smpServers = ["localhost:5223"]
    }

termSettings :: VirtualTerminalSettings
termSettings =
  VirtualTerminalSettings
    { virtualType = "xterm",
      virtualWindowSize = pure C.Size {height = 24, width = 1000},
      virtualEvent = retry,
      virtualInterrupt = retry
    }

data TestCC = TestCC ChatController VirtualTerminal (Async ())

aCfg :: AgentConfig
aCfg = agentConfig defaultChatConfig

cfg :: ChatConfig
cfg =
  defaultChatConfig
    { agentConfig =
        aCfg {retryInterval = (retryInterval aCfg) {initialInterval = 50000}}
    }

virtualSimplexChat :: FilePath -> Profile -> IO TestCC
virtualSimplexChat dbFile profile = do
  st <- createStore (dbFile <> ".chat.db") 1
  void . runExceptT $ createUser st profile True
  t <- withVirtualTerminal termSettings pure
  cc <- newChatController cfg opts {dbFile} t . const $ pure () -- no notifications
  a <- async $ runSimplexChat cc
  pure (TestCC cc t a)

testChatN :: [Profile] -> ([TestCC] -> IO ()) -> IO ()
testChatN ps test =
  bracket_
    (createDirectoryIfMissing False "tests/tmp")
    (removeDirectoryRecursive "tests/tmp")
    $ do
      let envs = zip ps $ map ((testDBPrefix <>) . show) [(1 :: Int) ..]
      tcs <- getTestCCs envs []
      test tcs
  where
    getTestCCs [] tcs = pure tcs
    getTestCCs ((p, db) : envs') tcs = (:) <$> virtualSimplexChat db p <*> getTestCCs envs' tcs

testChat2 :: Profile -> Profile -> (TestCC -> TestCC -> IO ()) -> IO ()
testChat2 p1 p2 test = testChatN [p1, p2] test_
  where
    test_ :: [TestCC] -> IO ()
    test_ [tc1, tc2] = test tc1 tc2
    test_ _ = error "expected 2 chat clients"

testChat3 :: Profile -> Profile -> Profile -> (TestCC -> TestCC -> TestCC -> IO ()) -> IO ()
testChat3 p1 p2 p3 test = testChatN [p1, p2, p3] test_
  where
    test_ :: [TestCC] -> IO ()
    test_ [tc1, tc2, tc3] = test tc1 tc2 tc3
    test_ _ = error "expected 3 chat clients"

testChat4 :: Profile -> Profile -> Profile -> Profile -> (TestCC -> TestCC -> TestCC -> TestCC -> IO ()) -> IO ()
testChat4 p1 p2 p3 p4 test = testChatN [p1, p2, p3, p4] test_
  where
    test_ :: [TestCC] -> IO ()
    test_ [tc1, tc2, tc3, tc4] = test tc1 tc2 tc3 tc4
    test_ _ = error "expected 4 chat clients"

concurrentlyN_ :: [IO a] -> IO ()
concurrentlyN_ = mapConcurrently_ id
