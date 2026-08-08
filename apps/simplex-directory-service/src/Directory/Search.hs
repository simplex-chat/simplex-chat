module Directory.Search where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Types

data SearchRequest = SearchRequest
  { target :: SearchTarget,
    searchType :: SearchType,
    searchTime :: UTCTime,
    lastId :: Int64 -- cursor for search: group_id or contact_reg_id, per target
  }

data SearchTarget = TGroups | TContacts ChatPeerType

data SearchType = STAll | STRecent | STSearch Text
