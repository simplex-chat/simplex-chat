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
import Simplex.Chat.Library.Commands (maxProfileImageSize, maxShortDescrLength)
import Simplex.Chat.Protocol (compressServiceBody)
import Simplex.Chat.Types
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, taggedObjectJSON)

data DirectoryRequest = DRSearch
  { searchText :: Text,
    searchCursor :: Maybe SearchCursor
  }

data DirectorySearchEntry = DirectorySearchEntry
  { entryType :: DirectoryEntryType,
    displayName :: Text,
    simplexName :: Maybe Text,
    groupLink :: PublicLink,
    shortDescr :: Maybe Text,
    image :: Maybe ImageData,
    activeAt :: Maybe UTCTime,
    createdAt :: Maybe UTCTime
  }

data DirectoryResponse
  = DRSearchResults
      { entries :: [DirectorySearchEntry],
        searchCursor :: Maybe SearchCursor,
        moreCount :: Int
      }
  | DRError {errorMessage :: Text}

$(JQ.deriveJSON defaultJSON ''DirectorySearchEntry)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "DR") ''DirectoryRequest)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "DR") ''DirectoryResponse)

responseObject :: DirectoryResponse -> J.Object
responseObject resp = case J.toJSON resp of
  J.Object o -> o
  _ -> JM.fromList [("type", J.String "error"), ("errorMessage", J.String "internal error")]

searchResultsPage :: (row -> SearchCursor) -> Int -> [(row, Maybe DirectorySearchEntry)] -> DirectoryResponse
searchResultsPage rowCursor matched rows = fitFromEnd entryRows
  where
    numbered = [((n, row), e) | (n, (row, e)) <- zip [1 ..] rows]
    entryRows = [(row, e) | (row, Just e) <- numbered]
    fitFromEnd sent = case page sent of
      Just resp -> resp
      Nothing -> case sent of
        [(row, e)] -> fromMaybe (skipped row) $ page [(row, dropImage e)]
        [] -> results [] Nothing
        _ -> fitFromEnd (init sent)
    page sent
      | fits resp = Just resp
      | otherwise = Nothing
      where
        fittedAll = length sent == length entryRows
        cursorRow = if fittedAll then fst <$> lastMaybe numbered else fst <$> lastMaybe sent
        more = not fittedAll || matched > length rows
        resp = results sent $ if more then cursorRow else Nothing
    skipped row = results [] (Just row)
    results sent cursorRow =
      DRSearchResults
        { entries = map snd sent,
          searchCursor = rowCursor . snd <$> cursorRow,
          moreCount = maybe 0 (\(consumed, _) -> max 0 (matched - consumed)) cursorRow
        }
    fits = isRight . compressServiceBody . LB.toStrict . J.encode . responseObject
    dropImage :: DirectorySearchEntry -> DirectorySearchEntry
    dropImage e = e {image = Nothing}
    lastMaybe = foldl' (\_ x -> Just x) Nothing

searchEntry :: UTCTime -> GroupInfo -> Maybe GroupLink -> Maybe DirectorySearchEntry
searchEntry now g@GroupInfo {groupProfile} gLink_ =
  entry <$> groupPublicLink g gLink_
  where
    GroupProfile {displayName, shortDescr, image} = groupProfile
    entry link@PublicLink {connShortLink} =
      DirectorySearchEntry
        { entryType = groupEntryType g,
          displayName,
          simplexName = groupSimplexName g,
          groupLink = if isJust connShortLink then link {connFullLink = Nothing} else link,
          shortDescr = T.take maxShortDescrLength <$> shortDescr,
          image = image >>= \img@(ImageData t) -> if T.length t > maxProfileImageSize || T.null (safeImageData t) then Nothing else Just img,
          activeAt = groupActiveAt now g,
          createdAt = groupCreatedAt now g
        }
