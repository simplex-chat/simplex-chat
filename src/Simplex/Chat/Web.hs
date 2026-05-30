{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Web
  ( WebChannelPreview (..),
    WebMessage (..),
    WebMemberProfile (..),
    WebFileInfo (..),
    CorsOrigin (..),
    renderWebPreviews,
    writeCorsConfig,
  )
where

import Control.Logger.Simple
import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.List (nubBy)
import Data.Map.Strict (Map)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), ChatPagination (..), WebPreviewConfig (..))
import Simplex.Chat.Markdown (MarkdownList)
import Simplex.Chat.Messages
  ( CChatItem (..),
    CIDirection (..),
    CIFile (..),
    CIMention,
    CIMeta (..),
    CIQDirection (..),
    CIQuote (..),
    CIReactionCount,
    Chat (..),
    ChatItem (..),
    ChatType (..),
  )
import Simplex.Chat.Messages.CIContent (ciMsgContent)
import Simplex.Chat.Protocol (MsgContent, MsgRef (..), QuotedMsg (..), isReport)
import Simplex.Chat.Store (StoreError)
import Simplex.Chat.Store.Groups (getRelayServedGroups)
import Simplex.Chat.Store.Messages (getGroupChat)
import Simplex.Chat.Types
  ( B64UrlByteString,
    GroupId,
    GroupInfo (..),
    GroupMember (..),
    GroupProfile (..),
    ImageData,
    LocalProfile (..),
    MemberId,
    MemberName,
    PublicGroupAccess (..),
    PublicGroupProfile (..),
    User,
  )
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Parsers (defaultJSON)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data WebFileInfo = WebFileInfo
  { fileName :: String,
    fileSize :: Integer
  }
  deriving (Show)

data WebMemberProfile = WebMemberProfile
  { memberId :: MemberId,
    displayName :: Text,
    image :: Maybe ImageData
  }
  deriving (Show)

data WebMessage = WebMessage
  { sender :: Maybe MemberId,
    ts :: UTCTime,
    content :: MsgContent,
    formattedText :: Maybe MarkdownList,
    file :: Maybe WebFileInfo,
    quote :: Maybe QuotedMsg,
    mentions :: Map MemberName CIMention,
    reactions :: [CIReactionCount],
    forward :: Maybe Bool,
    edited :: Bool
  }
  deriving (Show)

data WebChannelPreview = WebChannelPreview
  { channel :: GroupProfile,
    members :: [WebMemberProfile],
    messages :: [WebMessage],
    updatedAt :: UTCTime
  }
  deriving (Show)

$(JQ.deriveJSON defaultJSON ''WebFileInfo)

$(JQ.deriveJSON defaultJSON ''WebMemberProfile)

$(JQ.deriveJSON defaultJSON ''WebMessage)

$(JQ.deriveJSON defaultJSON ''WebChannelPreview)

renderWebPreviews :: WebPreviewConfig -> ChatController -> User -> IO ()
renderWebPreviews WebPreviewConfig {webJsonDir, webCorsFile} cc user = do
  groups <- withTransaction (chatStore cc) $ \db -> getRelayServedGroups db vr' user
  let publishable = filter hasPublicGroup groups
  corsEntries <- mapMaybe id <$> mapM (renderGroupPreview webJsonDir cc user) publishable
  forM_ webCorsFile $ writeCorsConfig corsEntries
  where
    vr' = chatVRange (config cc)
    hasPublicGroup GroupInfo {groupProfile = GroupProfile {publicGroup}} = case publicGroup of
      Just _ -> True
      _ -> False

renderGroupPreview :: FilePath -> ChatController -> User -> GroupInfo -> IO (Maybe (Text, CorsOrigin))
renderGroupPreview webJsonDir cc user GroupInfo {groupId = gId, groupProfile = gp@GroupProfile {publicGroup}} =
  case publicGroup of
    Just PublicGroupProfile {publicGroupId, publicGroupAccess} -> do
      let fName = publicGroupIdFileName publicGroupId <> ".json"
      result <- loadMessages cc user gId
      case result of
        Left e -> do
          logError $ "renderGroupPreview error for group " <> T.pack (show gId) <> ": " <> T.pack (show e)
          pure Nothing
        Right items -> do
          ts <- getCurrentTime
          let msgs = mapMaybe toWebMessage items
              senders = uniqueSenders items
              preview = WebChannelPreview
                { channel = gp,
                  members = senders,
                  messages = msgs,
                  updatedAt = ts
                }
          createDirectoryIfMissing True webJsonDir
          LB.writeFile (webJsonDir </> fName) (J.encode preview)
          pure $ corsEntry publicGroupId <$> publicGroupAccess
    Nothing -> pure Nothing

loadMessages :: ChatController -> User -> GroupId -> IO (Either StoreError [CChatItem 'CTGroup])
loadMessages cc user gId =
  withTransaction (chatStore cc) $ \db -> do
    let vr' = chatVRange (config cc)
    fmap (chatItems . fst) <$> runExceptT (getGroupChat db vr' user gId Nothing Nothing (CPLast 50) Nothing)

toWebMessage :: CChatItem 'CTGroup -> Maybe WebMessage
toWebMessage (CChatItem _ ChatItem {chatDir, meta = CIMeta {itemTs, itemDeleted, itemTimed, itemForwarded, itemEdited}, content, mentions, formattedText, quotedItem, reactions, file})
  | isJust itemDeleted = Nothing
  | isJust itemTimed = Nothing
  | otherwise = case ciMsgContent content of
      Just mc | not (isReport mc) ->
        let sender = case chatDir of
              CIGroupRcv GroupMember {memberId} -> Just memberId
              _ -> Nothing
         in Just WebMessage
              { sender,
                ts = itemTs,
                content = mc,
                formattedText,
                file = webFileInfo <$> file,
                quote = quotedItem >>= ciQuoteToQuotedMsg,
                mentions,
                reactions,
                forward = if isJust itemForwarded then Just True else Nothing,
                edited = itemEdited
              }
      _ -> Nothing

ciQuoteToQuotedMsg :: CIQuote c -> Maybe QuotedMsg
ciQuoteToQuotedMsg CIQuote {chatDir = qDir, sharedMsgId, sentAt, content = qContent} =
  Just QuotedMsg
    { msgRef = MsgRef
        { msgId = sharedMsgId,
          sentAt,
          sent = case qDir of
            CIQDirectSnd -> True
            CIQGroupSnd -> True
            _ -> False,
          memberId = case qDir of
            CIQGroupRcv (Just GroupMember {memberId}) -> Just memberId
            _ -> Nothing
        },
      content = qContent
    }

webFileInfo :: CIFile d -> WebFileInfo
webFileInfo CIFile {fileName, fileSize} = WebFileInfo {fileName, fileSize}

uniqueSenders :: [CChatItem 'CTGroup] -> [WebMemberProfile]
uniqueSenders = nubBy sameId . mapMaybe senderProfile
  where
    sameId (WebMemberProfile {memberId = a}) (WebMemberProfile {memberId = b}) = a == b
    senderProfile :: CChatItem 'CTGroup -> Maybe WebMemberProfile
    senderProfile (CChatItem _ ChatItem {chatDir}) = case chatDir of
      CIGroupRcv m -> Just $ memberToProfile m
      _ -> Nothing

memberToProfile :: GroupMember -> WebMemberProfile
memberToProfile GroupMember {memberId, memberProfile = LocalProfile {displayName, image}} =
  WebMemberProfile {memberId, displayName, image}

data CorsOrigin = CorsAny | CorsOrigins [Text]
  deriving (Show)

corsEntry :: B64UrlByteString -> PublicGroupAccess -> (Text, CorsOrigin)
corsEntry publicGroupId PublicGroupAccess {groupWebPage, allowEmbedding} =
  let fName = T.pack $ publicGroupIdFileName publicGroupId <> ".json"
      origin
        | allowEmbedding = CorsAny
        | isJust groupWebPage = CorsOrigins $ mapMaybe id [groupWebPage]
        | otherwise = CorsOrigins []
   in (fName, origin)

writeCorsConfig :: [(Text, CorsOrigin)] -> FilePath -> IO ()
writeCorsConfig entries path =
  TIO.writeFile path $ T.unlines $
    ["map {path} {cors_origin} {"]
    <> map corsLine entries
    <> [ "    default \"\"",
         "}",
         "header /preview/*.json Access-Control-Allow-Origin {cors_origin}",
         "header /preview/*.json Access-Control-Allow-Methods \"GET, OPTIONS\""
       ]
  where
    corsLine (fName, origin) = case origin of
      CorsAny -> "    /preview/" <> fName <> " \"*\""
      CorsOrigins origins -> case origins of
        [] -> "    # " <> fName <> " (no origin configured)"
        (o : _) -> "    /preview/" <> fName <> " \"" <> o <> "\""

publicGroupIdFileName :: B64UrlByteString -> String
publicGroupIdFileName = B.unpack . strEncode
