{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Directory.Listing where

import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.ByteString (ByteString)
import Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Directory.Store
import Simplex.Chat.Markdown
import Simplex.Chat.Types
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, taggedObjectJSON)
import System.Directory
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
    groupLink :: CreatedLinkContact,
    shortDescr :: Maybe MarkdownList,
    welcomeMessage :: Maybe MarkdownList,
    imageFile :: Maybe String
  }

$(JQ.deriveJSON defaultJSON ''DirectoryEntry)

data DirectoryListing = DirectoryListing {entries :: [DirectoryEntry]}

$(JQ.deriveJSON defaultJSON ''DirectoryListing)

type ImageFileData = ByteString

groupDirectoryEntry :: GroupInfoSummary -> Maybe (DirectoryEntry, Maybe (FilePath, ImageFileData))
groupDirectoryEntry (GIS GroupInfo {groupId, groupProfile} summary gLink_) =
  let GroupProfile {displayName, shortDescr, description, image, memberAdmission} = groupProfile
      entryType = DETGroup memberAdmission summary
      imgData = imgFileData =<< image
      entry groupLink =
        DirectoryEntry
          { entryType,
            displayName,
            groupLink,
            shortDescr = toFormattedText <$> shortDescr,
            welcomeMessage = toFormattedText <$> description,
            imageFile = fst <$> imgData
          }
   in (\gLink -> (entry (connLinkContact gLink), imgData)) <$> gLink_
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

generateListing :: DirectoryStore -> FilePath -> [GroupInfoSummary] -> IO ()
generateListing st dir gs = do
  gs' <- filterListedGroups st gs
  removePathForcibly (dir </> listingImageFolder)
  createDirectoryIfMissing True (dir </> listingImageFolder)
  gs'' <-
    fmap catMaybes $ forM gs' $ \g@(GIS GroupInfo {groupId} _ _) ->
      forM (groupDirectoryEntry g) $ \(g', img) -> do
        forM_ img $ \(imgFile, imgData) -> B.writeFile (dir </> imgFile) imgData
        pure (groupId, g')
  saveListing listingFileName gs''
  saveListing promotedFileName =<< filterPromotedGroups st gs''
  where
    saveListing f = LB.writeFile (dir </> f) . J.encode . DirectoryListing . map snd

filterPromotedGroups :: DirectoryStore -> [(GroupId, DirectoryEntry)] -> IO [(GroupId, DirectoryEntry)]
filterPromotedGroups st gs = do
  pgs <- readTVarIO $ promotedGroups st
  pure $ filter (\g -> fst g `S.member` pgs) gs

toFormattedText :: Text -> MarkdownList
toFormattedText t = fromMaybe [FormattedText Nothing t] $ parseMaybeMarkdownList t
