{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Directory.Rpc where

import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.TH as JQ
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Either (isRight)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Directory.Listing
import Directory.Search
import Directory.Store
import Simplex.Chat.Library.Commands (maxProfileImageSize)
import Simplex.Chat.Protocol (compressServiceBody)
import Simplex.Chat.Types
import Simplex.Messaging.SimplexName (SimplexNameInfo (..), SimplexNameType (..), shortNameInfoStr)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, taggedObjectJSON)

data DirectoryRequest = DRSearch
  { searchText :: Text,
    -- opaque to the client: it is stored and echoed back, never inspected
    searchCursor :: Maybe SearchCursor
  }

data DirectorySearchEntry = DirectorySearchEntry
  { entryType :: DirectoryEntryType,
    displayName :: Text,
    simplexName :: Maybe Text,
    groupLink :: PublicLink,
    -- stored text, not DirectoryEntry's MarkdownList: the apps parse markdown locally
    shortDescr :: Maybe Text,
    image :: Maybe ImageData,
    activeAt :: Maybe UTCTime,
    createdAt :: Maybe UTCTime
  }

data DirectoryResponse
  = DRSearchResults
      { entries :: [DirectorySearchEntry],
        searchCursor :: Maybe SearchCursor -- Nothing when there are no more results
      }
  | DRError {errorMessage :: Text}

$(JQ.deriveJSON defaultJSON ''DirectorySearchEntry)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "DR") ''DirectoryRequest)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "DR") ''DirectoryResponse)

responseObject :: DirectoryResponse -> J.Object
responseObject resp = case J.toJSON resp of
  J.Object o -> o
  _ -> JM.fromList [("type", J.String "error"), ("errorMessage", J.String "internal error")]

-- The page is shrunk from the end until its compressed encoding fits the envelope, and the cursor is the
-- last row consumed. A lone entry that does not fit is retried without its image, then skipped, or paging would stall.
searchResultsPage :: (row -> SearchCursor) -> Bool -> [(row, Maybe DirectorySearchEntry)] -> DirectoryResponse
searchResultsPage rowCursor storeHasMore rows = fit entryRows
  where
    entryRows = [(row, e) | (row, Just e) <- rows]
    fit sent = case page sent of
      Just resp -> resp
      Nothing -> case sent of
        [(row, e)] -> fromMaybe (skipped row) $ page [(row, dropImage e)]
        _ -> fit (init sent)
    page sent
      | fits resp = Just resp
      | otherwise = Nothing
      where
        fittedAll = length sent == length entryRows
        cursorRow = if fittedAll then fst <$> lastMaybe rows else fst <$> lastMaybe sent
        more = not fittedAll || storeHasMore
        resp = DRSearchResults {entries = map snd sent, searchCursor = if more then rowCursor <$> cursorRow else Nothing}
    skipped row = DRSearchResults {entries = [], searchCursor = Just $ rowCursor row}
    fits = isRight . compressServiceBody . LB.toStrict . J.encode . responseObject
    dropImage :: DirectorySearchEntry -> DirectorySearchEntry
    dropImage e = e {image = Nothing}
    lastMaybe = foldl' (\_ x -> Just x) Nothing

searchEntry :: UTCTime -> GroupInfo -> Maybe GroupLink -> Maybe DirectorySearchEntry
searchEntry now g@GroupInfo {groupProfile, chatTs, createdAt = groupCreatedAt, groupSummary} gLink_ =
  entry <$> groupPublicLink g gLink_
  where
    GroupProfile {displayName, shortDescr, image, memberAdmission, publicGroup} = groupProfile
    entry link@PublicLink {connShortLink} =
      DirectorySearchEntry
        { entryType = DETGroup ((\PublicGroupProfile {groupType} -> groupType) <$> publicGroup) memberAdmission groupSummary,
          displayName,
          simplexName = shortNameInfoStr . SimplexNameInfo NTPublicGroup <$> verifiedGroupDomain g,
          -- the apps connect through the short link, and a full link is hundreds of bytes of the envelope
          groupLink = if isJust connShortLink then link {connFullLink = Nothing} else link,
          shortDescr,
          -- a profile received from its owner is not size-checked, so bound what is relayed
          -- rather than passing an arbitrarily large data URI on to the apps
          image = image >>= \img@(ImageData t) -> if T.length t > maxProfileImageSize then Nothing else Just img,
          activeAt = recentRoundedTime 900 now $ fromMaybe groupCreatedAt chatTs,
          createdAt = recentRoundedTime 86400 now groupCreatedAt
        }
