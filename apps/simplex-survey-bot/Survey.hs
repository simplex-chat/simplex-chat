{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Survey where

import Control.Concurrent.STM
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time.Clock (UTCTime)
import qualified Data.Yaml as Y
import GHC.Generics (Generic)
import Options
import Simplex.Messaging.Parsers (taggedObjectJSON, dropPrefix)
import Simplex.Messaging.TMap (TMap)

data SurveyDescription = SurveyDescription
  { welcomeMessage :: String,
    surveyQuestions :: NonEmpty SurveyQuestion,
    thankYouMessage :: String
  }
  deriving (Eq, Show, Generic, FromJSON)

data SurveyQuestion
  = SQText {question :: String}
  | SQChoice {question :: String, choices :: NonEmpty String, multiple :: Bool, allowOther :: Bool}
  deriving (Eq, Show, Generic)

instance FromJSON SurveyQuestion where
  parseJSON = J.genericParseJSON . taggedObjectJSON $ dropPrefix "SQ"

data SurveyAnswer = SurveyAnswer
  { answerText :: String,
    answeredAt :: UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON SurveyAnswer where toEncoding = J.genericToEncoding J.defaultOptions

data ContactQuestion = ContactQuestion
  { contactId :: Int64,
    contactName :: String,
    questionNo :: Int
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON ContactQuestion where toEncoding = J.genericToEncoding J.defaultOptions

data SurveyLogRecord
  = SLQuestion {contactQuestion :: ContactQuestion, question :: SentQuestion}
  | SLAnswer {contactQuestion :: ContactQuestion, answer :: SurveyAnswer}
  deriving (Eq, Show, Generic)

data SentQuestion = SentQuestion
  { questionNo :: Int,
    questionText :: String,
    sentAt :: UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON SentQuestion where toEncoding = J.genericToEncoding J.defaultOptions

instance ToJSON SurveyLogRecord where
  toEncoding = J.genericToEncoding . taggedObjectJSON $ dropPrefix "SL"
  toJSON = J.genericToJSON . taggedObjectJSON $ dropPrefix "SL"

instance FromJSON SurveyLogRecord where
  parseJSON = J.genericParseJSON . taggedObjectJSON $ dropPrefix "SL"

data SurveyContactState = SurveyContactState
  { answers :: [SurveyAnswer],
    sentQuestion :: Maybe SentQuestion,
    completed :: Bool
  }

data SurveyState = SurveyState
  { survey :: SurveyDescription,
    respondents :: TMap Int64 SurveyContactState
  }

readSurveyState :: SurveyBotOpts -> IO SurveyState
readSurveyState SurveyBotOpts {surveyDescription, surveyLog} = do
  survey <- (Y.decodeEither' <$> B.readFile surveyDescription) >>= either (fail . Y.prettyPrintParseException) pure
  rs <- map J.eitherDecodeStrict . B.lines <$> B.readFile surveyLog
  let rs' = foldl' addAnswer M.empty rs
  respondents <- newTVarIO rs'
  pure SurveyState {survey, respondents}
  where
    addAnswer :: Map Int64 SurveyContactState -> Either String SurveyLogRecord -> Map Int64 SurveyContactState
    addAnswer m = \case
      Right r -> M.alter (Just . add) (contactId cq) m
        where
          cq = contactQuestion r
          add = \case
            Just s@SurveyContactState {answers} -> case r of
              SLQuestion {question} -> s {sentQuestion = Just question}
              SLAnswer {answer} -> s {answers = answers <> [answer]}
            Nothing -> case r of
              SLQuestion {question} -> SurveyContactState {answers = [], sentQuestion = Just question, completed = False}
              SLAnswer {answer} -> SurveyContactState {answers = [answer], sentQuestion = Nothing, completed = False}
      Left e -> error e
