{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Directory.Listing where

import Control.Applicative ((<|>))
import qualified Data.Aeson.TH as JQ
import Data.ByteString (ByteString)
import Data.ByteString.Base64 as B64
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Types
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, taggedObjectJSON)
import System.FilePath

listingFileName :: String
listingFileName = "listing.json"

promotedFileName :: String
promotedFileName = "promoted.json"

listingImageFolder :: String
listingImageFolder = "images"

data DirectoryEntryType = DETGroup
  { admission :: Maybe GroupMemberAdmission,
    summary :: GroupSummary
  }

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "DET") ''DirectoryEntryType)

data DirectoryEntry = DirectoryEntry
  { entryType :: DirectoryEntryType,
    displayName :: Text,
    shortDescr :: Maybe Text,
    welcomeMessage :: Maybe Text,
    imageFile :: Maybe String
  }

$(JQ.deriveJSON defaultJSON ''DirectoryEntry)

data DirectoryListing = DirectoryListing {entries :: [DirectoryEntry]}

$(JQ.deriveJSON defaultJSON ''DirectoryListing)

type ImageFileData = ByteString

groupDirectoryEntry :: GroupInfoSummary -> (DirectoryEntry, Maybe (FilePath, ImageFileData))
groupDirectoryEntry (GIS GroupInfo {groupId, groupProfile} summary) =
  let GroupProfile {displayName, shortDescr, description, image, memberAdmission} = groupProfile
      entryType = DETGroup memberAdmission summary
      imgData = imgFileData =<< image
   in (DirectoryEntry {entryType, displayName, shortDescr, welcomeMessage = description, imageFile = fst <$> imgData}, imgData)
  where
    imgFileData (ImageData img) =
      let (img', imgExt) =
            fromMaybe (img, ".jpg") $
              (,".jpg") <$> T.stripPrefix "data:image/jpg;base64," img
                <|> (,".png") <$> T.stripPrefix "data:image/png;base64," img
          imgFile = listingImageFolder </> show groupId <> imgExt
       in case B64.decode $ encodeUtf8 img' of
            Right img'' -> Just (imgFile, img'')
            Left _ -> Nothing
