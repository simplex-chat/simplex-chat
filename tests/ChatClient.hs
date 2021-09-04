{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ChatClient where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket, bracket_)
import Control.Monad.Except
import Data.List (dropWhileEnd)
import Network.Socket
import Simplex.Chat
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..))
import Simplex.Chat.Options
import Simplex.Chat.Store
import Simplex.Chat.Types (Profile)
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.RetryInterval
import Simplex.Messaging.Server (runSMPServerBlocking)
import Simplex.Messaging.Server.Env.STM
import Simplex.Messaging.Transport
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import qualified System.Terminal as C
import System.Terminal.Internal (VirtualTerminal (..), VirtualTerminalSettings (..), withVirtualTerminal)
import System.Timeout (timeout)

testDBPrefix :: FilePath
testDBPrefix = "tests/tmp/test"

serverPort :: ServiceName
serverPort = "5000"

opts :: ChatOpts
opts =
  ChatOpts
    { dbFile = undefined,
      smpServers = ["localhost:5000"]
    }

termSettings :: VirtualTerminalSettings
termSettings =
  VirtualTerminalSettings
    { virtualType = "xterm",
      virtualWindowSize = pure C.Size {height = 24, width = 1000},
      virtualEvent = retry,
      virtualInterrupt = retry
    }

data TestCC = TestCC
  { chatController :: ChatController,
    virtualTerminal :: VirtualTerminal,
    chatAsync :: Async (),
    termAsync :: Async (),
    termQ :: TQueue String
  }

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
  chatAsync <- async $ runSimplexChat cc
  termQ <- newTQueueIO
  termAsync <- async $ readTerminalOutput t termQ
  pure TestCC {chatController = cc, virtualTerminal = t, chatAsync, termAsync, termQ}

readTerminalOutput :: VirtualTerminal -> TQueue String -> IO ()
readTerminalOutput t termQ = do
  let w = virtualWindow t
  winVar <- atomically $ newTVar . init =<< readTVar w
  forever . atomically $ do
    win <- readTVar winVar
    win' <- init <$> readTVar w
    if win' == win
      then retry
      else do
        let diff = getDiff win' win
        forM_ diff $ writeTQueue termQ
        writeTVar winVar win'
  where
    getDiff :: [String] -> [String] -> [String]
    getDiff win win' = getDiff_ 1 (length win) win win'
    getDiff_ :: Int -> Int -> [String] -> [String] -> [String]
    getDiff_ n len win' win =
      let diff = drop (len - n) win'
       in if drop n win <> diff == win'
            then map (dropWhileEnd (== ' ')) diff
            else getDiff_ (n + 1) len win' win

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

serverCfg :: ServerConfig
serverCfg =
  ServerConfig
    { transports = [(serverPort, transport @TCP)],
      tbqSize = 1,
      msgQueueQuota = 4,
      queueIdBytes = 12,
      msgIdBytes = 6,
      storeLog = Nothing,
      blockSize = 4096,
      serverPrivateKey =
        -- full RSA private key (only for tests)
        "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\
        \TmKzSAw7iVWwEUZR/PeiEKazqrpp9VU="
    }

withSmpServer :: IO a -> IO a
withSmpServer = serverBracket (`runSMPServerBlocking` serverCfg) (pure ()) . const

serverBracket :: (TMVar Bool -> IO ()) -> IO () -> (ThreadId -> IO a) -> IO a
serverBracket process afterProcess f = do
  started <- newEmptyTMVarIO
  bracket
    (forkIOWithUnmask ($ process started))
    (\t -> killThread t >> afterProcess >> waitFor started "stop")
    (\t -> waitFor started "start" >> f t)
  where
    waitFor started s =
      5000000 `timeout` atomically (takeTMVar started) >>= \case
        Nothing -> error $ "server did not " <> s
        _ -> pure ()
