module Simplex.Controller where

import Control.Monad (forever)
import Simplex.Chat
import Simplex.Input
import Simplex.View
import UnliftIO (MonadUnliftIO)
import UnliftIO.STM (TBQueue, atomically, readTBQueue)

chatSubscriber :: MonadUnliftIO m => TBQueue ChatEvent -> m ()
chatSubscriber chatQ = runEvents chatQ processChatEvent

inputSubscriber :: MonadUnliftIO m => TBQueue InputEvent -> m ()
inputSubscriber inputQ = runEvents inputQ processInputEvent

processChatEvent :: MonadUnliftIO m => ChatEvent -> m ()
processChatEvent _ = someViewUpdate

processInputEvent :: MonadUnliftIO m => InputEvent -> m ()
processInputEvent _ = someChatAction

runEvents :: MonadUnliftIO m => TBQueue a -> (a -> m ()) -> m ()
runEvents q f = forever $ atomically (readTBQueue q) >>= f
