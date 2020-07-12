{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Simplex.Messaging.PrintScenario where

import Control.Monad.Writer
import Control.Protocol (interpret)
import Data.Singletons
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types

printScenario :: SimplexProtocol s s' a -> IO ()
printScenario scn = ps 1 "" $ execWriter $ logScenario scn
  where
    ps :: Int -> String -> [(String, String)] -> IO ()
    ps _ _ [] = return ()
    ps i p ((p', l) : ls)
      | p' /= p = part (i + 1) $ show i <> ". " <> p' <> ":\n" <> prefix l
      | otherwise = part i $ prefix l
      where
        part i' s = putStrLn s >> ps i' p' ls
        prefix s = "   - " <> s

logScenario :: MonadWriter [(String, String)] m => SimplexProtocol s s' a -> m a
logScenario = interpret $ \from to cmd -> do
  tell [(party from, commandStr cmd <> " " <> party to)]
  mockCommand cmd

commandStr :: SimplexCommand from to a -> String
commandStr = \case
  CreateConn _ -> "creates connection in"
  Subscribe cid -> "subscribes to connection " <> show cid <> " in"
  Unsubscribe cid -> "unsubscribes from connection " <> show cid <> " in"
  SendInvite _ -> "sends out-of band invitation to "
  ConfirmConn cid _ -> "confirms connection " <> show cid <> " in"
  PushConfirm cid _ -> "pushes confirmation for " <> show cid <> " to"
  SecureConn cid _ -> "secures connection " <> show cid <> " in"
  SendMsg cid _ -> "sends message to connection " <> show cid <> " in"
  PushMsg cid _ -> "pushes message from connection " <> show cid <> " to"
  DeleteMsg cid _ -> "deletes message from connection " <> show cid <> " in"

mockCommand :: Monad m => SimplexCommand from to a -> m a
mockCommand = \case
  (CreateConn _) ->
    return
      CreateConnResponse
        { recipientId = "Qxz93A",
          senderId = "N9pA3g"
        }
  Subscribe _ -> return ()
  Unsubscribe _ -> return ()
  SendInvite _ -> return ()
  ConfirmConn _ _ -> return ()
  PushConfirm _ _ -> return ()
  SecureConn _ _ -> return ()
  SendMsg _ _ -> return ()
  PushMsg _ _ -> return ()
  DeleteMsg _ _ -> return ()

party :: Sing (p :: Party) -> String
party = \case
  SRecipient -> "Alice (recipient)"
  SBroker -> "Alice's server (broker)"
  SSender -> "Bob (sender)"
