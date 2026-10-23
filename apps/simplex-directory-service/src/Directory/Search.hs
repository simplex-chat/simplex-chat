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

-- Where the last page ended: the value the search mode sorts by, and the group ID that breaks ties.
-- The service RPC sends it to anyone, so it carries nothing else: text search must not reveal registration times.
data SearchCursor
  = SCMembers {lastMembers :: Int64, lastGroupId :: GroupId}
  | SCRecent {lastCreatedAt :: UTCTime, lastGroupId :: GroupId}

data SearchType = STAll | STRecent | STSearch Text

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "SC") ''SearchCursor)
