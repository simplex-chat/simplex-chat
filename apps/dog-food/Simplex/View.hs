{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.View
  ( printToView,
    showInvitation,
    showAgentError,
    ttyContact,
    ttyFromContact,
    ttyGroup,
    ttyFromGroup,
  )
where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Composition ((.:))
import Simplex.Chat.Controller
import Simplex.Chat.Markdown
import Simplex.Chat.Styled
import Simplex.Messaging.Agent.Protocol
import Simplex.Terminal (printToTerminal)
import System.Console.ANSI.Types
import Types

showInvitation :: (MonadUnliftIO m, MonadReader ChatController m) => Contact -> SMPQueueInfo -> m ()
showInvitation = printToView .: invitation

showAgentError :: (MonadUnliftIO m, MonadReader ChatController m) => Contact -> AgentErrorType -> m ()
showAgentError = printToView .: agentError

invitation :: Contact -> SMPQueueInfo -> [StyledString]
invitation c qInfo =
  [ "pass this invitation to your contact " <> ttyContact c <> " (via any channel): ",
    "",
    (bPlain . serializeSmpQueueInfo) qInfo,
    "",
    "and ask them to connect: /c <name_for_you> <invitation_above>"
  ]

agentError :: Contact -> AgentErrorType -> [StyledString]
agentError c = \case
  CONN e -> case e of
    NOT_FOUND -> ["no contact " <> ttyContact c]
    DUPLICATE -> ["contact " <> ttyContact c <> " already exists"]
    SIMPLEX -> ["contact " <> ttyContact c <> " did not accept invitation yet"]
  e -> ["chat error: " <> plain (show e)]

printToView :: (MonadUnliftIO m, MonadReader ChatController m) => [StyledString] -> m ()
printToView s = asks chatTerminal >>= liftIO . (`printToTerminal` s)

ttyContact :: Contact -> StyledString
ttyContact (Contact a) = styled (Colored Green) a

ttyFromContact :: Contact -> StyledString
ttyFromContact (Contact a) = styled (Colored Yellow) $ a <> "> "

ttyGroup :: Group -> StyledString
ttyGroup (Group g) = styled (Colored Blue) $ "#" <> g

ttyFromGroup :: Group -> Contact -> StyledString
ttyFromGroup (Group g) (Contact a) = styled (Colored Yellow) $ "#" <> g <> " " <> a <> "> "
