module Directory.Search where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Types

data SearchRequest = SearchRequest
  { searchType :: SearchType,
    searchTime :: UTCTime,
    lastGroup :: GroupId -- cursor for search
  }

data SearchType = STAll | STRecent | STSearch Text
