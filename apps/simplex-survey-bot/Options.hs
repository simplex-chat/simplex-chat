{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Options where

import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative
import Simplex.Chat.Controller (updateStr, versionNumber, versionString)
import Simplex.Chat.Options (ChatOpts (..), CoreChatOpts, coreChatOptsP)
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (safeDecodeUtf8)

data SurveyBotOpts = SurveyBotOpts
  { coreOptions :: CoreChatOpts,
    surveyDescription :: FilePath,
    surveyLog :: FilePath,
    surveyResults :: FilePath
  }

surveyBotOpts :: FilePath -> FilePath -> Parser SurveyBotOpts
surveyBotOpts appDir defaultDbFileName = do
  coreOptions <- coreChatOptsP appDir defaultDbFileName
  surveyDescription <-
    strOption
      ( long "survey"
          <> metavar "SURVEY"
          <> help "Survey description file"
      )
  surveyLog <-
    strOption
      ( long "answers"
          <> metavar "ANSWERS"
          <> help "Survey answers file"
      )
  surveyResults <-
    strOption
      ( long "results"
          <> metavar "RESULTS"
          <> help "Survey results file"
      )
  pure
    SurveyBotOpts
      { coreOptions,
        surveyDescription,
        surveyLog,
        surveyResults
      }

getSurveyBotOpts :: FilePath -> FilePath -> IO SurveyBotOpts
getSurveyBotOpts appDir defaultDbFileName =
  execParser $
    info
      (helper <*> versionOption <*> surveyBotOpts appDir defaultDbFileName)
      (header versionStr <> fullDesc <> progDesc "Start survey bot with DB_FILE, SURVEY and RESULTS files, and use SERVER as SMP server")
  where
    versionStr = versionString versionNumber
    versionOption = infoOption versionAndUpdate (long "version" <> short 'v' <> help "Show version")
    versionAndUpdate = versionStr <> "\n" <> updateStr

mkChatOpts :: SurveyBotOpts -> ChatOpts
mkChatOpts SurveyBotOpts {coreOptions} =
  ChatOpts
    { coreOptions,
      chatCmd = "",
      chatCmdDelay = 3,
      chatServerPort = Nothing,
      optFilesFolder = Nothing,
      showReactions = False,
      allowInstantFiles = True,
      autoAcceptFileSize = 0,
      muteNotifications = True,
      maintenance = False
    }
