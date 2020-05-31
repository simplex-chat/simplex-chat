{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Simplex.Messaging.PrintScenario where

import Control.Monad.Writer
import Data.Singletons
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types

printScenario :: Protocol rs bs ss rs' bs' ss' a -> IO ()
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

logScenario :: Protocol rs bs ss rs' bs' ss' a -> Writer [(String, String)] a
logScenario (Start s) = tell [("", s)]
logScenario (p :>> c) = logScenario p >> logCommand c
logScenario (p :>>= f) = logScenario p >>= \x -> logCommand (f x)

logCommand :: PartiesCommand from fs fs' to ts ts' a -> Writer [(String, String)] a
logCommand ((:->) from to cmd) = do
  tell [(party from, commandStr cmd <> " " <> party to)]
  mockCommand cmd

commandStr :: Command from fs fs' to ts ts' a -> String
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

mockCommand :: Monad m => Command from fs fs' to ts ts' a -> m a
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
