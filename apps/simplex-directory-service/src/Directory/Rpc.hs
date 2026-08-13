{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Directory.Rpc where

import qualified Data.Aeson.TH as JQ
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Directory.Listing
import Directory.Search
import Directory.Store
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

-- Entries without any link are dropped: there would be nothing to connect to.
searchEntry :: UTCTime -> GroupInfo -> Maybe GroupLink -> Maybe DirectorySearchEntry
searchEntry now g@GroupInfo {groupProfile, chatTs, createdAt = groupCreatedAt, groupSummary} gLink_ =
  entry <$> groupPublicLink g gLink_
  where
    GroupProfile {displayName, shortDescr, image, memberAdmission, publicGroup} = groupProfile
    entry groupLink =
      DirectorySearchEntry
        { entryType = DETGroup ((\PublicGroupProfile {groupType} -> groupType) <$> publicGroup) memberAdmission groupSummary,
          displayName,
          simplexName = shortNameInfoStr . SimplexNameInfo NTPublicGroup <$> verifiedGroupDomain g,
          groupLink,
          shortDescr,
          image,
          activeAt = recentRoundedTime 900 now $ fromMaybe groupCreatedAt chatTs,
          createdAt = recentRoundedTime 86400 now groupCreatedAt
        }
