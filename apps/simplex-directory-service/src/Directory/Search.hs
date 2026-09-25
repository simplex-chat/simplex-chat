{-# LANGUAGE TemplateHaskell #-}

module Directory.Search where

import qualified Data.Aeson.TH as JQ
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Types
import Simplex.Messaging.Parsers (defaultJSON)

data SearchRequest = SearchRequest
  { searchType :: SearchType,
    searchTime :: UTCTime,
    searchCursor :: SearchCursor
  }

-- Where the last page ended: each search mode reads the field it sorts by, and the group ID
-- breaks ties, because member counts and timestamps are not unique.
data SearchCursor = SearchCursor
  { lastMembers :: Int64,
    lastCreatedAt :: UTCTime,
    lastGroupId :: GroupId
  }

data SearchType = STAll | STRecent | STSearch Text

$(JQ.deriveJSON defaultJSON ''SearchCursor)
