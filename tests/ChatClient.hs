{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatClient where

import Control.Concurrent.Async
import Control.Concurrent.STM (retry)
import Control.Monad.Except
import Simplex.Chat
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Chat.Options
import Simplex.Chat.Store
import Simplex.Chat.Types (Profile)
import qualified System.Terminal as C
import System.Terminal.Internal (VirtualTerminalSettings (..), withVirtualTerminal)

testDB1 :: FilePath
testDB1 = "tests/tmp/test1"

testDB2 :: FilePath
testDB2 = "tests/tmp/test2"

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

virtualSimplexChat :: FilePath -> Profile -> IO (ChatController, Async ())
virtualSimplexChat dbFile profile = do
  st <- createStore (dbFile <> ".chat.db") 1
  void . runExceptT $ createUser st profile True
  t <- withVirtualTerminal termSettings pure
  cc <- newChatController opts {dbFile} t . const $ pure () -- no notifications
  a <- async $ runSimplexChat cc
  pure (cc, a)

testChat2 :: Profile -> Profile -> (ChatController -> ChatController -> IO ()) -> IO ()
testChat2 p1 p2 test = do
  (cc1, _) <- virtualSimplexChat testDB1 p1
  (cc2, _) <- virtualSimplexChat testDB2 p2
  test cc1 cc2
