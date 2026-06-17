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
    webPreviewWorker,
    writeCorsConfig,
    removeStaleFiles,
    channelContentChanged,
    channelProfileUpdated,
    channelRemoved,
    extractOrigin,
  )
where

import Control.Concurrent.STM (check, flushTQueue)
import Control.Exception (SomeException, catch)
import Control.Logger.Simple
import Control.Monad (forM_, void, when)
import Control.Monad.Except (runExceptT)
import Data.Either (rights)
import Data.Int (Int64)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), CorsOrigin (..), PublishableGroup (..), WebPreviewConfig (..), WebPreviewState (..), mkStoreCxt)
import Simplex.Chat.Markdown (FormattedText (..), MarkdownList, parseMaybeMarkdownList)
import Simplex.Chat.Messages
  ( CChatItem (..),
    CIDirection (..),
    CIFile (..),
    CIMeta (..),
    CIQDirection (..),
    CIQuote (..),
    CIReactionCount,
    ChatItem (..),
    ChatType (..),
  )
import Simplex.Chat.Messages.CIContent (ciMsgContent)
import Simplex.Chat.Protocol (MsgContent, MsgRef (..), QuotedMsg (..), isReport)
import Simplex.Chat.Store.Groups (getGroupOwners, getRelayPublishableGroups)
import Simplex.Chat.Store.Messages (getGroupWebPreviewItems)
import Simplex.Chat.Store.Shared (getGroupInfo)
import Simplex.Chat.Types
  ( B64UrlByteString,
    GroupInfo (..),
    GroupMember (..),
    GroupProfile (..),
    GroupSummary (..),
    ImageData,
    LocalProfile (..),
    MemberId,
    PublicGroupAccess (..),
    PublicGroupProfile (..),
    User (..),
  )
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (safeDecodeUtf8)
import qualified URI.ByteString as U
import Simplex.Messaging.Parsers (defaultJSON)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile, renameFile)
import System.FilePath (dropExtension, takeExtension, (</>))
import UnliftIO.STM

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
    reactions :: [CIReactionCount],
    forward :: Maybe Bool,
    edited :: Bool
  }
  deriving (Show)

data WebChannelPreview = WebChannelPreview
  { channel :: GroupProfile,
    shortDescription :: Maybe MarkdownList,
    welcomeMessage :: Maybe MarkdownList,
    members :: [WebMemberProfile],
    subscribers :: Maybe Int64,
    messages :: [WebMessage],
    updatedAt :: UTCTime
  }
  deriving (Show)

$(JQ.deriveJSON defaultJSON ''WebFileInfo)

$(JQ.deriveJSON defaultJSON ''WebMemberProfile)

$(JQ.deriveJSON defaultJSON ''WebMessage)

$(JQ.deriveJSON defaultJSON ''WebChannelPreview)

webPreviewWorker :: WebPreviewConfig -> ChatController -> [User] -> IO ()
webPreviewWorker cfg@WebPreviewConfig {webJsonDir, webCorsFile, webUpdateInterval} cc users =
  forM_ (webPreviewState cc) $ \wps -> do
    createDirectoryIfMissing True webJsonDir
    initPublishableGroups wps
    cleanStaleFiles wps
    regenerateCors wps
    seedRoutinePending wps
    workerLoop wps
  where
    cxt = mkStoreCxt (config cc)

    workerLoop wps@WebPreviewState {priorityRender, filesToRemove, corsNeeded, routinePending, wakeSignal} = do
      drainRemovals
      drainPriority
      handleCors
      renderRoutine
      noRoutine <- atomically $ S.null <$> readTVar routinePending
      when noRoutine waitRefresh
      workerLoop wps
      where
        drainRemovals = atomically (tryReadTQueue filesToRemove) >>= \case
          Nothing -> pure ()
          Just f -> do
            removeFile (webJsonDir </> f) `catch` \(_ :: SomeException) -> pure ()
            drainRemovals

        -- flush the whole queue and render each group once: a burst of changes in one
        -- channel enqueues its id many times, but only needs a single render
        drainPriority = do
          gIds <- atomically $ flushTQueue priorityRender
          forM_ (S.fromList gIds) $ renderOneGroup wps

        handleCors = do
          needed <- atomically $ swapTVar corsNeeded False
          when needed $ regenerateCors wps

        -- render a single routine item; the main loop calls this once per iteration
        renderRoutine = do
          mGId <- atomically $ do
            pending <- readTVar routinePending
            case S.minView pending of
              Nothing -> pure Nothing
              Just (gId, rest) -> writeTVar routinePending rest >> pure (Just gId)
          forM_ mGId $ renderOneGroup wps

        -- routine list drained: wait for the refresh timer or a change signal; only the timer
        -- seeds the next full sweep, a change just returns to let the main loop service it
        waitRefresh = do
          delay <- registerDelay (webUpdateInterval * 1000000)
          timerFired <- atomically $
            (True <$ (readTVar delay >>= check)) `orElse` (False <$ takeTMVar wakeSignal)
          when timerFired $ seedRoutinePending wps

    initPublishableGroups WebPreviewState {publishableGroupIds} = do
      rows <- withTransaction (chatStore cc) $ \db ->
        concat <$> mapM (getRelayPublishableGroups db) users
      let gIds = M.fromList [(gId, toPublishableGroup pgId access) | (gId, pgId, access) <- rows]
      atomically $ writeTVar publishableGroupIds gIds

    cleanStaleFiles WebPreviewState {publishableGroupIds} = do
      ids <- readTVarIO publishableGroupIds
      let activeFiles = S.fromList $ map pgFileName $ M.elems ids
      removeStaleFiles webJsonDir activeFiles

    regenerateCors WebPreviewState {publishableGroupIds} = do
      ids <- readTVarIO publishableGroupIds
      let entries = mapMaybe pgCorsEntry $ M.elems ids
      forM_ webCorsFile $ writeCorsConfig entries

    seedRoutinePending WebPreviewState {publishableGroupIds, routinePending} =
      atomically $ M.keysSet <$> readTVar publishableGroupIds >>= writeTVar routinePending

    renderOneGroup WebPreviewState {publishableGroupIds} gId = do
      publishable <- atomically $ M.member gId <$> readTVar publishableGroupIds
      when publishable $
        renderOrRemoveStale `catch` \(e :: SomeException) ->
          logError $ "web preview: error rendering group " <> T.pack (show gId) <> ": " <> T.pack (show e)
      where
        renderOrRemoveStale = do
          r <- withTransaction (chatStore cc) $ \db ->
            findUser $ \u -> fmap (\g -> (u, g)) <$> runExceptT (getGroupInfo db cxt u gId)
          case r of
            Just (u, gInfo) | hasPublicGroup gInfo ->
              void $ renderGroupPreview cfg cc u gInfo
            _ -> do
              fName <- atomically $ do
                pg <- M.lookup gId <$> readTVar publishableGroupIds
                modifyTVar' publishableGroupIds (M.delete gId)
                pure $ pgFileName <$> pg
              forM_ fName $ \f ->
                removeFile (webJsonDir </> f) `catch` \(_ :: SomeException) -> pure ()
              logInfo $ "web preview: group " <> T.pack (show gId) <> " no longer publishable"

    findUser f = go users
      where
        go [] = pure Nothing
        go (u : us) = f u >>= \case
          Right a -> pure (Just a)
          Left _ -> go us

renderGroupPreview :: WebPreviewConfig -> ChatController -> User -> GroupInfo -> IO (Maybe (Text, CorsOrigin))
renderGroupPreview WebPreviewConfig {webJsonDir, webPreviewItemCount} cc user gInfo@GroupInfo {groupProfile = gp@GroupProfile {shortDescr = sd, description = wd, publicGroup}, groupSummary = GroupSummary {publicMemberCount}} =
  case publicGroup of
    Just PublicGroupProfile {publicGroupId, publicGroupAccess} -> do
      let fName = publicGroupIdFileName publicGroupId <> ".json"
      (items, owners) <- withTransaction (chatStore cc) $ \db -> do
        is <- getGroupWebPreviewItems db user gInfo webPreviewItemCount
        os <- getGroupOwners db cxt user gInfo
        pure (is, os)
      ts <- getCurrentTime
      let rendered = mapMaybe toRenderedItem $ rights items
          msgs = map fst rendered
          senders = collectSenders $ map memberToProfile owners <> concatMap snd rendered
          preview = WebChannelPreview
            { channel = gp,
              shortDescription = toFormattedText =<< sd,
              welcomeMessage = toFormattedText =<< wd,
              members = senders,
              subscribers = publicMemberCount,
              messages = msgs,
              updatedAt = ts
            }
      let destPath = webJsonDir </> fName
          tmpPath = destPath <> ".tmp"
      LB.writeFile tmpPath (J.encode preview)
      renameFile tmpPath destPath
      pure $ corsEntry publicGroupId <$> publicGroupAccess
    Nothing -> pure Nothing
  where
    cxt = mkStoreCxt (config cc)

channelContentChanged :: ChatController -> Int64 -> STM ()
channelContentChanged cc gId =
  forM_ (webPreviewState cc) $ \WebPreviewState {publishableGroupIds, priorityRender, routinePending, wakeSignal} -> do
    ids <- readTVar publishableGroupIds
    when (M.member gId ids) $ do
      writeTQueue priorityRender gId
      modifyTVar' routinePending (S.delete gId)
      void $ tryPutTMVar wakeSignal ()

channelProfileUpdated :: ChatController -> Int64 -> GroupProfile -> STM ()
channelProfileUpdated cc gId GroupProfile {publicGroup} =
  forM_ (webPreviewState cc) $ \WebPreviewState {publishableGroupIds, priorityRender, filesToRemove, corsNeeded, routinePending, wakeSignal} ->
    case publicGroup of
      Just PublicGroupProfile {publicGroupId, publicGroupAccess} -> do
        let pg = PublishableGroup
              { pgFileName = publicGroupIdFileName publicGroupId <> ".json",
                pgCorsEntry = corsEntry publicGroupId <$> publicGroupAccess
              }
        modifyTVar' publishableGroupIds (M.insert gId pg)
        writeTQueue priorityRender gId
        modifyTVar' routinePending (S.delete gId)
        writeTVar corsNeeded True
        void $ tryPutTMVar wakeSignal ()
      Nothing -> do
        ids <- readTVar publishableGroupIds
        forM_ (pgFileName <$> M.lookup gId ids) $ writeTQueue filesToRemove
        modifyTVar' publishableGroupIds (M.delete gId)
        modifyTVar' routinePending (S.delete gId)
        writeTVar corsNeeded True
        void $ tryPutTMVar wakeSignal ()

channelRemoved :: ChatController -> Int64 -> STM ()
channelRemoved cc gId =
  forM_ (webPreviewState cc) $ \WebPreviewState {publishableGroupIds, filesToRemove, corsNeeded, routinePending, wakeSignal} -> do
    ids <- readTVar publishableGroupIds
    forM_ (pgFileName <$> M.lookup gId ids) $ writeTQueue filesToRemove
    modifyTVar' publishableGroupIds (M.delete gId)
    modifyTVar' routinePending (S.delete gId)
    writeTVar corsNeeded True
    void $ tryPutTMVar wakeSignal ()

toRenderedItem :: CChatItem 'CTGroup -> Maybe (WebMessage, [WebMemberProfile])
toRenderedItem (CChatItem _ ChatItem {chatDir, meta = CIMeta {itemTs, itemTimed, itemForwarded, itemEdited}, content, formattedText, quotedItem, reactions, file})
  | isJust itemTimed = Nothing
  | otherwise = case ciMsgContent content of
      Just mc | not (isReport mc) ->
        let (sender, senderProfile) = case chatDir of
              CIGroupRcv m@GroupMember {memberId} -> (Just memberId, [memberToProfile m])
              _ -> (Nothing, [])
            quotedProfile = case quotedItem of
              Just CIQuote {chatDir = CIQGroupRcv (Just m)} -> [memberToProfile m]
              _ -> []
         in Just
              ( WebMessage
                  { sender,
                    ts = itemTs,
                    content = mc,
                    formattedText,
                    file = webFileInfo <$> file,
                    quote = quotedItem >>= ciQuoteToQuotedMsg,
                    reactions,
                    forward = if isJust itemForwarded then Just True else Nothing,
                    edited = itemEdited
                  },
                senderProfile <> quotedProfile
              )
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

collectSenders :: [WebMemberProfile] -> [WebMemberProfile]
collectSenders = M.elems . M.fromList . map (\p@WebMemberProfile {memberId} -> (memberId, p))

memberToProfile :: GroupMember -> WebMemberProfile
memberToProfile GroupMember {memberId, memberProfile = LocalProfile {displayName, image}} =
  WebMemberProfile {memberId, displayName, image}

toPublishableGroup :: B64UrlByteString -> Maybe PublicGroupAccess -> PublishableGroup
toPublishableGroup pgId access =
  PublishableGroup
    { pgFileName = publicGroupIdFileName pgId <> ".json",
      pgCorsEntry = corsEntry pgId <$> access
    }

corsEntry :: B64UrlByteString -> PublicGroupAccess -> (Text, CorsOrigin)
corsEntry publicGroupId PublicGroupAccess {groupWebPage, allowEmbedding} =
  let fName = T.pack $ publicGroupIdFileName publicGroupId <> ".json"
      origin
        | allowEmbedding = CorsAny
        | otherwise = CorsOrigins $ mapMaybe extractOrigin $ maybeToList groupWebPage
   in (fName, origin)

extractOrigin :: Text -> Maybe Text
extractOrigin url =
  case U.parseURI U.laxURIParserOptions (encodeUtf8 url) of
    Right uri@U.URI {uriScheme = U.Scheme sch, uriAuthority = Just _}
      | sch == "https" || sch == "http" ->
          let originUri = uri {U.uriPath = "", U.uriQuery = U.Query [], U.uriFragment = Nothing}
              origin = safeDecodeUtf8 $ U.serializeURIRef' originUri
           in if T.all safeOriginChar origin then Just origin else Nothing
    _ -> Nothing
  where
    -- percent-encoded bytes in the host (e.g. %22, %0a) are decoded by serializeURIRef',
    -- so reject any origin with characters that could break out of the Caddy CORS config or header
    safeOriginChar c =
      (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c `elem` (".-:/[]" :: [Char])

channelPath :: Text
channelPath = "/channel/"

writeCorsConfig :: [(Text, CorsOrigin)] -> FilePath -> IO ()
writeCorsConfig entries path =
  TIO.writeFile path $ T.unlines $
    ["map {path} {cors_origin} {"]
    <> map corsLine entries
    <> [ "    default \"\"",
         "}",
         "header " <> channelPath <> "*.json Access-Control-Allow-Origin {cors_origin}",
         "header " <> channelPath <> "*.json Access-Control-Allow-Methods \"GET, OPTIONS\""
       ]
  where
    corsLine (fName, origin) = case origin of
      CorsAny -> "    " <> channelPath <> fName <> " \"*\""
      CorsOrigins origins -> case origins of
        [] -> "    # " <> fName <> " (no origin configured)"
        (o : _) -> "    " <> channelPath <> fName <> " \"" <> o <> "\""

removeStaleFiles :: FilePath -> S.Set FilePath -> IO ()
removeStaleFiles dir activeFiles = do
  let -- matches "<base64url>.json" and leftover "<base64url>.json.tmp" from an interrupted write
      isPreviewFile f =
        let f' = if takeExtension f == ".tmp" then dropExtension f else f
            base = dropExtension f'
         in takeExtension f' == ".json" && not (null base) && all isBase64Url base
      isBase64Url c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-' || c == '_'
  allFiles <- S.filter isPreviewFile . S.fromList <$> listDirectory dir
  mapM_ (\f -> removeFile (dir </> f)) $ S.difference allFiles activeFiles

toFormattedText :: Text -> Maybe MarkdownList
toFormattedText t = case parseMaybeMarkdownList t of
  Just fts | any hasFormat fts -> Just fts
  _ -> Nothing
  where
    hasFormat (FormattedText fmt _) = isJust fmt

publicGroupIdFileName :: B64UrlByteString -> String
publicGroupIdFileName = B.unpack . strEncode

hasPublicGroup :: GroupInfo -> Bool
hasPublicGroup GroupInfo {groupProfile = GroupProfile {publicGroup}} = isJust publicGroup

