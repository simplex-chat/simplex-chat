{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Directory.Search where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Types

data SearchRequest = SearchRequest
  { searchType :: SearchType,
    searchTime :: UTCTime,
    sentGroups :: Set GroupId
  }

data SearchType = STAll | STRecent | STSearch Text

takeTop :: Int -> [(GroupInfo, GroupSummary)] -> [(GroupInfo, GroupSummary)]
takeTop n = take n . sortOn (Down . currentMembers . snd)

takeRecent :: Int -> [(GroupInfo, GroupSummary)] -> [(GroupInfo, GroupSummary)]
takeRecent n = take n . sortOn (Down . (\GroupInfo {createdAt} -> createdAt) . fst)

groupIds :: [(GroupInfo, GroupSummary)] -> Set GroupId
groupIds = S.fromList . map (\(GroupInfo {groupId}, _) -> groupId)

filterNotSent :: Set GroupId -> [(GroupInfo, GroupSummary)] -> [(GroupInfo, GroupSummary)]
filterNotSent sentGroups = filter (\(GroupInfo {groupId}, _) -> groupId `S.notMember` sentGroups)
