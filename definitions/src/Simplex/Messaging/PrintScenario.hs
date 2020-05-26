{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Simplex.Messaging.PrintScenario where

import Control.Monad.Writer
import Data.Singletons
import Simplex.Messaging.Protocol

printScenario :: Protocol rs bs ss rs' bs' ss' a -> IO ()
printScenario = putStrLn . unlines . execWriter . logScenario

logScenario :: Protocol rs bs ss rs' bs' ss' a -> Writer [String] ()
logScenario (Start s) = tell [s]
logScenario (p :>> c) = logScenario p >> logCommand c

logCommand :: PartiesCommand from fs fs' to ts ts' a -> Writer [String] ()
logCommand ((:->) from to cmd) = tell [unwords [party from, commandStr cmd, party to]]

commandStr :: Command from fs fs' to ts ts' a -> String
commandStr (CreateConn _) = "creates connection in"
commandStr (Subscribe _) = "subscribes to connection in"
commandStr (Unsubscribe _) = "unsubscribes from connection in"
commandStr (SendInvite _) = "sends out-of band invitation to"
commandStr (ConfirmConn _ _) = "confirms connection in"
commandStr (PushConfirm _ _) = "pushes confirmation to"
commandStr (SecureConn _ _) = "secures connection in"
commandStr (SendMsg _ _) = "sends message to"
commandStr (PushMsg _ _) = "pushes message to"
commandStr (DeleteMsg _ _) = "deletes message from"

party :: Sing (p :: Party) -> String
party SRecipient = "Alice (recipient)"
party SBroker = "Alice's server (broker)"
party SSender = "Bob (sender)"
