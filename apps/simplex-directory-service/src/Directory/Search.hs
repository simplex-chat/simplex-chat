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

takeTop :: Int -> [GroupInfoSummary] -> [GroupInfoSummary]
takeTop n = take n . sortOn (\(GIS _ GroupSummary {currentMembers} _) -> Down currentMembers)

takeRecent :: Int -> [GroupInfoSummary] -> [GroupInfoSummary]
takeRecent n = take n . sortOn (\(GIS GroupInfo {createdAt} _ _) -> Down createdAt)

groupIds :: [GroupInfoSummary] -> Set GroupId
groupIds = S.fromList . map (\(GIS GroupInfo {groupId} _ _) -> groupId)

filterNotSent :: Set GroupId -> [GroupInfoSummary] -> [GroupInfoSummary]
filterNotSent sentGroups = filter (\(GIS GroupInfo {groupId} _ _) -> groupId `S.notMember` sentGroups)
