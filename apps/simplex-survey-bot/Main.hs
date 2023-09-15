{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Text as T
import Options
import Simplex.Chat.Bot
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Options
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Types
import Survey
import System.Directory (getAppUserDataDirectory)

main :: IO ()
main = do
  opts <- welcomeGetOpts
  state <- readSurveyState opts
  simplexChatCore terminalChatConfig (mkChatOpts opts) Nothing $ surveyBot state opts

welcomeGetOpts :: IO SurveyBotOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@SurveyBotOpts {coreOptions = CoreChatOpts {dbFilePrefix}} <- getSurveyBotOpts appDir "simplex_survey_bot"
  putStrLn $ "SimpleX Chat Survey Bot v" ++ versionNumber
  putStrLn $ "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"
  pure opts

surveyBot :: SurveyState -> SurveyBotOpts -> User -> ChatController -> IO ()
surveyBot SurveyState {survey = SurveyDescription {welcomeMessage}} SurveyBotOpts {surveyResults} _user cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, resp) <- atomically . readTBQueue $ outputQ cc
    case resp of
      CRContactConnected _ ct _ -> do
        contactConnected ct
        sendMessage cc ct welcomeMessage
    --   CRNewChatItem _ (AChatItem _ SMDRcv (DirectChat ct) ci@ChatItem {content = CIRcvMsgContent mc})
    --     | publisher `elem` publishers ->
    --       if allowContent mc
    --         then do
    --           sendChatCmd cc "/contacts" >>= \case
    --             CRContactsList _ cts -> void . forkIO $ do
    --               let cts' = filter broadcastTo cts
    --               forM_ cts' $ \ct' -> sendComposedMessage cc ct' Nothing mc
    --               sendReply $ "Forwarded to " <> show (length cts') <> " contact(s)"
    --             r -> putStrLn $ "Error getting contacts list: " <> show r
    --         else sendReply "!1 Message is not supported!"
    --     | otherwise -> do
    --       sendReply prohibitedMessage
    --       deleteMessage cc ct $ chatItemId' ci
    --     where
    --       sendReply = sendComposedMessage cc ct (Just $ chatItemId' ci) . textMsgContent
    --       publisher = Publisher {contactId = contactId' ct, localDisplayName = localDisplayName' ct}
    --       allowContent = \case
    --         MCText _ -> True
    --         MCLink {} -> True
    --         MCImage {} -> True
    --         _ -> False
    --       broadcastTo ct'@Contact {activeConn = conn@Connection {connStatus}} =
    --         (connStatus == ConnSndReady || connStatus == ConnReady)
    --           && not (connDisabled conn)
    --           && contactId' ct' /= contactId' ct
      _ -> pure ()
  where
    contactConnected ct = putStrLn $ T.unpack (localDisplayName' ct) <> " connected"
