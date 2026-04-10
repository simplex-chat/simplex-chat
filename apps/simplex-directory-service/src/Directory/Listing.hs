{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Directory.Listing where

import Control.Applicative ((<|>))
import Control.Monad
import Crypto.Hash (Digest, MD5)
import qualified Crypto.Hash as CH
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Format.ISO8601 (iso8601Show)
import Directory.Store
import Simplex.Chat.Markdown
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, taggedObjectJSON)
import System.Directory
import System.FilePath

directoryDataPath :: String
directoryDataPath = "data"

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
    imageFile :: Maybe String,
    activeAt :: Maybe UTCTime,
    createdAt :: Maybe UTCTime
  }

$(JQ.deriveJSON defaultJSON ''DirectoryEntry)

data DirectoryListing = DirectoryListing {entries :: [DirectoryEntry]}

$(JQ.deriveJSON defaultJSON ''DirectoryListing)

type ImageFileData = ByteString

newOrActive :: NominalDiffTime
newOrActive = 30 * nominalDay

recentRoundedTime :: Int64 -> UTCTime -> UTCTime -> Maybe UTCTime
recentRoundedTime roundTo now t
  | diffUTCTime now t > newOrActive = Nothing
  | otherwise =
      let secs = (systemSeconds (utcToSystemTime t) `div` roundTo) * roundTo
       in Just $ systemToUTCTime $ MkSystemTime secs 0

groupDirectoryEntry :: UTCTime -> GroupInfo -> Maybe GroupLink -> Maybe (DirectoryEntry, Maybe (FilePath, ImageFileData))
groupDirectoryEntry now GroupInfo {groupProfile, chatTs, createdAt, groupSummary} gLink_ =
  let GroupProfile {displayName, shortDescr, description, image, memberAdmission} = groupProfile
      entryType = DETGroup memberAdmission groupSummary
      entry groupLink =
        let de =
              DirectoryEntry
                { entryType,
                  displayName,
                  groupLink,
                  shortDescr = toFormattedText <$> shortDescr,
                  welcomeMessage = toFormattedText <$> description,
                  imageFile = fst <$> imgData,
                  activeAt = recentRoundedTime 900 now $ fromMaybe createdAt chatTs,
                  createdAt = recentRoundedTime 86400 now createdAt
                }
            imgData = imgFileData groupLink =<< image
         in (de, imgData)
   in (entry . connLinkContact) <$> gLink_
  where
    imgFileData :: CreatedConnLink 'CMContact -> ImageData -> Maybe (FilePath, ByteString)
    imgFileData groupLink (ImageData img) =
      let (img', imgExt) =
            fromMaybe (img, ".jpg") $
              (,".jpg") <$> T.stripPrefix "data:image/jpg;base64," img
                <|> (,".png") <$> T.stripPrefix "data:image/png;base64," img
          imgName = B.unpack $ B64URL.encodeUnpadded $ BA.convert $ (CH.hash :: ByteString -> Digest MD5) $ strEncode (connFullLink groupLink)
          imgFile = listingImageFolder </> imgName <> imgExt
       in case B64.decode $ encodeUtf8 img' of
            Right img'' -> Just (imgFile, img'')
            Left _ -> Nothing

generateListing :: FilePath -> [(GroupInfo, GroupReg, Maybe GroupLink)] -> IO ()
generateListing dir gs = do
  createDirectoryIfMissing True dir
  oldDirs <- filter ((directoryDataPath <> ".") `isPrefixOf`) <$> listDirectory dir
  ts <- getCurrentTime
  let newDirPath = directoryDataPath <> "." <> iso8601Show ts <> "/"
      newDir = dir </> newDirPath
  createDirectoryIfMissing True (newDir </> listingImageFolder)
  gs' <-
    fmap catMaybes $ forM gs $ \(g, gr, link_) ->
      forM (groupDirectoryEntry ts g link_) $ \(g', img) -> do
        forM_ img $ \(imgFile, imgData) -> B.writeFile (newDir </> imgFile) imgData
        pure (g', gr)
  saveListing newDir listingFileName gs'
  saveListing newDir promotedFileName $ filter (\(_, GroupReg {promoted}) -> promoted) gs'
  -- atomically update the link
  let newSymLink = newDir <> ".link"
      symLink = dir </> directoryDataPath
  createDirectoryLink newDirPath newSymLink
  renamePath newSymLink symLink
  mapM_ (removePathForcibly . (dir </>)) oldDirs
  where
    saveListing newDir f = LB.writeFile (newDir </> f) . J.encode . DirectoryListing . map fst

toFormattedText :: Text -> MarkdownList
toFormattedText t = fromMaybe [FormattedText Nothing t] $ parseMaybeMarkdownList t
