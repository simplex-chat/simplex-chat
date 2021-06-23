{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat where

import Control.Applicative ((<|>))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import Data.Functor (($>))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Controller
import Simplex.Chat.Styled (plain)
import Simplex.Help
import Simplex.Input
import Simplex.Messaging.Agent (createConnection)
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Parsers (parseAll)
import Simplex.View
import Types
import UnliftIO.STM

data ChatCommand
  = ChatHelp
  | MarkdownHelp
  | AddContact Contact
  | Connect Contact SMPQueueInfo
  | DeleteContact Contact
  | SendMessage Contact ByteString
  deriving (Show)

runChatController :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
runChatController = do
  inputSubscriber

inputSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
inputSubscriber = do
  q <- asks inputEventQ
  agent <- asks smpAgent
  forever $
    atomically (readTBQueue q) >>= \case
      InputControl _ -> pure ()
      InputCommand s -> case (parseAll chatCommandP . encodeUtf8 . T.pack) s of
        Left e -> printToView ["invalid input: " <> plain e]
        Right cmd -> case cmd of
          ChatHelp -> printToView chatHelpInfo
          MarkdownHelp -> printToView markdownInfo
          AddContact c@(Contact a) -> do
            runExceptT (createConnection agent $ Just a) >>= \case
              Left e -> showAgentError c e
              Right (_, qInfo) -> showInvitation c qInfo
          Connect _contact _sMPQueueInfo -> pure ()
          DeleteContact _contact -> pure ()
          SendMessage _contact _byteString -> pure ()

chatCommandP :: Parser ChatCommand
chatCommandP =
  ("/help" <|> "/h") $> ChatHelp
    <|> ("/add " <|> "/a ") *> (AddContact <$> contact)
    <|> ("/connect " <|> "/c ") *> (Connect <$> contact <* A.space <*> smpQueueInfoP)
    <|> ("/delete " <|> "/d ") *> (DeleteContact <$> contact)
    <|> A.char '@' *> (SendMessage <$> contact <* A.space <*> A.takeByteString)
    <|> ("/markdown" <|> "/m") $> MarkdownHelp
  where
    contact = Contact <$> A.takeTill (== ' ')
