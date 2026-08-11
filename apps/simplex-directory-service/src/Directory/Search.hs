module Directory.Search where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Types

data SearchRequest = SearchRequest
  { searchType :: SearchType,
    searchTime :: UTCTime,
    searchCursor :: SearchCursor
  }

-- Position of the last sent row in the sort order of its search type. Each mode
-- reads the value it sorts by; the group ID breaks ties, as neither sort key is unique.
data SearchCursor = SearchCursor
  { lastMembers :: Int64,
    lastCreatedAt :: UTCTime,
    lastGroupId :: GroupId
  }

data SearchType = STAll | STRecent | STSearch Text
