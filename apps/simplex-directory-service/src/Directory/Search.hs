{-# LANGUAGE TemplateHaskell #-}

module Directory.Search where

import qualified Data.Aeson.TH as JQ
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Types
import Simplex.Messaging.Parsers (dropPrefix, taggedObjectJSON)

data SearchRequest = SearchRequest
  { searchType :: SearchType,
    searchTime :: UTCTime,
    searchCursor :: SearchCursor
  }

data SearchCursor
  = SCMembers {lastMembers :: Int64, lastGroupId :: GroupId}
  | SCRecent {lastCreatedAt :: UTCTime, lastGroupId :: GroupId}

data SearchType = STAll | STRecent | STSearch Text

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "SC") ''SearchCursor)
