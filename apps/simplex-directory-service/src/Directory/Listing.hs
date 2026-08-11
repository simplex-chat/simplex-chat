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
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
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

data DirectoryEntryType
  = DETGroup
      { groupType :: Maybe GroupType,
        admission :: Maybe GroupMemberAdmission,
        summary :: GroupSummary
      }
  | DETContact {peerType :: ChatPeerType}

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "DET") ''DirectoryEntryType)

data PublicLink = PublicLink
  { connFullLink :: Maybe ConnReqContact,
    connShortLink :: Maybe ShortLinkContact
  }

$(JQ.deriveJSON defaultJSON ''PublicLink)

data DirectoryEntry = DirectoryEntry
  { entryType :: DirectoryEntryType,
    displayName :: Text,
    simplexName :: Maybe Text,
    groupLink :: PublicLink,
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
groupDirectoryEntry now g@GroupInfo {groupProfile, chatTs, createdAt, groupSummary} gLink_ =
  let GroupProfile {displayName, shortDescr, description, image, memberAdmission, publicGroup} = groupProfile
      gt = (\PublicGroupProfile {groupType} -> groupType) <$> publicGroup
      entryType = DETGroup gt memberAdmission groupSummary
      description' = case publicGroup of
        Just PublicGroupProfile {groupType = gt', groupLink = sLnk} ->
          let gtStr = case gt' of GTChannel -> "channel"; _ -> "group"
              linkLine = "Link to join the " <> gtStr <> " " <> displayName <> ": " <> decodeUtf8 (strEncode sLnk)
           in Just $ maybe linkLine (<> "\n\n" <> linkLine) description
        Nothing -> description
      entry groupLink =
        let de =
              DirectoryEntry
                { entryType,
                  displayName,
                  simplexName = shortNameInfoStr . SimplexNameInfo NTPublicGroup <$> verifiedGroupDomain g,
                  groupLink,
                  shortDescr = toFormattedText <$> shortDescr,
                  welcomeMessage = toFormattedText <$> description',
                  imageFile = fst <$> imgData,
                  activeAt = recentRoundedTime 900 now $ fromMaybe createdAt chatTs,
                  createdAt = recentRoundedTime 86400 now createdAt
                }
            imgData = imgFileData groupLink =<< image
         in (de, imgData)
   in case publicGroup of
        Just PublicGroupProfile {groupLink = sLnk} ->
          Just $ entry $ PublicLink Nothing (Just sLnk)
        Nothing ->
          entry . toPublicLink . connLinkContact <$> gLink_
  where
    toPublicLink (CCLink fullLink shortLink) = PublicLink (Just fullLink) shortLink

imgFileData :: PublicLink -> ImageData -> Maybe (FilePath, ByteString)
imgFileData PublicLink {connFullLink, connShortLink} (ImageData img) =
  let (img', imgExt) =
        fromMaybe (img, ".jpg") $
          (,".jpg") <$> T.stripPrefix "data:image/jpg;base64," img
            <|> (,".png") <$> T.stripPrefix "data:image/png;base64," img
      linkHash = case connFullLink of
        Just fl -> strEncode fl
        Nothing -> maybe "" strEncode connShortLink
      imgName = B.unpack $ B64URL.encodeUnpadded $ BA.convert $ (CH.hash :: ByteString -> Digest MD5) linkHash
      imgFile = listingImageFolder </> imgName <> imgExt
   in case B64.decode $ encodeUtf8 img' of
        Right img'' -> Just (imgFile, img'')
        Left _ -> Nothing

contactDirectoryEntry :: UTCTime -> Contact -> ChatPeerType -> Maybe (DirectoryEntry, Maybe (FilePath, ImageFileData))
contactDirectoryEntry now ct@Contact {profile = LocalProfile {displayName, shortDescr, description, image, contactLink}, createdAt, chatTs} peerType =
  case contactLink of
    Just cl ->
      let pubLink = toPublicLink cl
          imgData = imgFileData pubLink =<< image
          de =
            DirectoryEntry
              { entryType = DETContact peerType,
                displayName,
                simplexName = shortNameInfoStr . SimplexNameInfo NTContact <$> verifiedContactDomain ct,
                groupLink = pubLink,
                shortDescr = toFormattedText <$> shortDescr,
                welcomeMessage = toFormattedText <$> description,
                imageFile = fst <$> imgData,
                activeAt = recentRoundedTime 900 now $ fromMaybe createdAt chatTs,
                createdAt = recentRoundedTime 86400 now createdAt
              }
       in Just (de, imgData)
    Nothing -> Nothing
  where
    toPublicLink = \case
      CLFull fullLink -> PublicLink (Just fullLink) Nothing
      CLShort shortLink -> PublicLink Nothing (Just shortLink)

generateListing :: FilePath -> [(GroupInfo, GroupReg, Maybe GroupLink)] -> [(Contact, ContactReg)] -> IO ()
generateListing dir gs cs = do
  createDirectoryIfMissing True dir
  oldDirs <- filter ((directoryDataPath <> ".") `isPrefixOf`) <$> listDirectory dir
  ts <- getCurrentTime
  let newDirPath = directoryDataPath <> "." <> iso8601Show ts <> "/"
      newDir = dir </> newDirPath
  createDirectoryIfMissing True (newDir </> listingImageFolder)
  let writeEntry (e, img) = do
        forM_ img $ \(imgFile, imgData) -> B.writeFile (newDir </> imgFile) imgData
        pure e
  gEntries <-
    fmap catMaybes $ forM gs $ \(g, GroupReg {promoted}, link_) ->
      forM (groupDirectoryEntry ts g link_) $ \ei -> (,promoted) <$> writeEntry ei
  cEntries <-
    fmap catMaybes $ forM cs $ \(ct, ContactReg {peerType, contactPromoted}) ->
      forM (contactDirectoryEntry ts ct peerType) $ \ei -> (,contactPromoted) <$> writeEntry ei
  let entries = gEntries ++ cEntries
  saveListing newDir listingFileName entries
  saveListing newDir promotedFileName $ filter snd entries
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
