{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Simplex.Messaging.PrintScenario where

import Control.Monad.Writer
import Control.Protocol
import Control.XFreer
import Data.Singletons
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types

printScenario :: SimplexProtocol s s' a -> IO ()
printScenario scn = ps 1 "" $ execWriter $ logScenario scn
  where
    ps :: Int -> String -> [(String, String)] -> IO ()
    ps _ _ [] = return ()
    ps i p ((p', l) : ls)
      | p' == "" = prt i $ "## " <> l <> "\n"
      | p' /= p = prt (i + 1) $ show i <> ". " <> p' <> ":\n" <> l'
      | otherwise = prt i l'
      where
        prt i' s = putStrLn s >> ps i' p' ls
        l' = "   - " <> l

logScenario :: SimplexProtocol s s' a -> Writer [(String, String)] a
logScenario (Pure x) = return x
logScenario (Bind p f) = logProtocol p >>= \x -> logScenario (f x)

logProtocol :: ProtocolCmd SimplexCommand '[Recipient, Broker, Sender] s s' a -> Writer [(String, String)] a
logProtocol (Comment s) = tell [("", s)]
logProtocol (ProtocolCmd from to cmd) = do
  tell [(party from, commandStr cmd <> " " <> party to)]
  mockCommand cmd

commandStr :: SimplexCommand from to a -> String
commandStr (CreateConn _) = "creates connection in"
commandStr (Subscribe cid) = "subscribes to connection " <> show cid <> " in"
commandStr (Unsubscribe cid) = "unsubscribes from connection " <> show cid <> " in"
commandStr (SendInvite _) = "sends out-of band invitation to "
commandStr (ConfirmConn cid _) = "confirms connection " <> show cid <> " in"
commandStr (PushConfirm cid _) = "pushes confirmation for " <> show cid <> " to"
commandStr (SecureConn cid _) = "secures connection " <> show cid <> " in"
commandStr (SendMsg cid _) = "sends message to connection " <> show cid <> " in"
commandStr (PushMsg cid _) = "pushes message from connection " <> show cid <> " to"
commandStr (DeleteMsg cid _) = "deletes message from connection " <> show cid <> " in"

mockCommand :: Monad m => SimplexCommand from to a -> m a
mockCommand (CreateConn _) =
  return
    CreateConnResponse
      { recipientId = "Qxz93A",
        senderId = "N9pA3g"
      }
mockCommand (Subscribe _) = return ()
mockCommand (Unsubscribe _) = return ()
mockCommand (SendInvite _) = return ()
mockCommand (ConfirmConn _ _) = return ()
mockCommand (PushConfirm _ _) = return ()
mockCommand (SecureConn _ _) = return ()
mockCommand (SendMsg _ _) = return ()
mockCommand (PushMsg _ _) = return ()
mockCommand (DeleteMsg _ _) = return ()

party :: Sing (p :: Party) -> String
party SRecipient = "Alice (recipient)"
party SBroker = "Alice's server (broker)"
party SSender = "Bob (sender)"
