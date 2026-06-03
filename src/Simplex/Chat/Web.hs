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
    webPreviewWorker,
    channelChanged,
    channelRemoved,
  )
where

import Control.Concurrent.STM (check)
import Control.Exception (SomeException, catch)
import Control.Logger.Simple
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (runExceptT)
import Data.Either (rights)
import Data.Int (Int64)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.List (nubBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), WebPreviewConfig (..), WebPreviewState (..))
import Simplex.Chat.Markdown (FormattedText (..), MarkdownList, parseMaybeMarkdownList)
import Simplex.Chat.Messages
  ( CChatItem (..),
    CIDirection (..),
    CIFile (..),
    CIMention,
    CIMeta (..),
    CIQDirection (..),
    CIQuote (..),
    CIReactionCount,
    ChatItem (..),
    ChatType (..),
  )
import Simplex.Chat.Messages.CIContent (ciMsgContent)
import Simplex.Chat.Protocol (MsgContent, MsgRef (..), QuotedMsg (..), isReport)
import Simplex.Chat.Store.Groups (getGroupOwners, getRelayServedGroups)
import Simplex.Chat.Store.Messages (getGroupWebPreviewItems)
import Simplex.Chat.Store.Shared (getGroupInfo)
import Simplex.Chat.Types
  ( B64UrlByteString,
    GroupInfo (..),
    GroupMember (..),
    GroupProfile (..),
    ImageData,
    LocalProfile (..),
    MemberId,
    MemberName,
    PublicGroupAccess (..),
    PublicGroupProfile (..),
    User (..),
  )
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Parsers (defaultJSON)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath (takeExtension, (</>))
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
    mentions :: Map MemberName CIMention,
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
    messages :: [WebMessage],
    updatedAt :: UTCTime
  }
  deriving (Show)

$(JQ.deriveJSON defaultJSON ''WebFileInfo)

$(JQ.deriveJSON defaultJSON ''WebMemberProfile)

$(JQ.deriveJSON defaultJSON ''WebMessage)

$(JQ.deriveJSON defaultJSON ''WebChannelPreview)

webPreviewWorker :: WebPreviewConfig -> ChatController -> [User] -> IO ()
webPreviewWorker WebPreviewConfig {webJsonDir, webCorsFile, webUpdateInterval} cc users =
  forM_ (webPreviewState cc) $ \wps -> do
    createDirectoryIfMissing True webJsonDir
    initPublishableGroups wps
    seedRoutinePending wps
    workerLoop wps
  where
    vr' = chatVRange (config cc)

    workerLoop wps@WebPreviewState {priorityRender, filesToRemove, corsNeeded, routinePending, wakeSignal} = do
      drainRemovals
      drainPriority
      handleCors
      processOneRoutine
      sleepOrWake
      workerLoop wps
      where
        drainRemovals = atomically (tryReadTQueue filesToRemove) >>= \case
          Nothing -> pure ()
          Just f -> do
            removeFile (webJsonDir </> f) `catch` \(_ :: SomeException) -> pure ()
            drainRemovals

        drainPriority = atomically (tryReadTQueue priorityRender) >>= \case
          Nothing -> pure ()
          Just gId -> do
            renderOneGroup wps gId
            drainPriority

        handleCors = do
          needed <- atomically $ swapTVar corsNeeded False
          when needed regenerateCors

        processOneRoutine = do
          mGId <- atomically $ do
            pending <- readTVar routinePending
            case S.minView pending of
              Nothing -> pure Nothing
              Just (gId, rest) -> writeTVar routinePending rest >> pure (Just gId)
          forM_ mGId $ renderOneGroup wps

        sleepOrWake = do
          pending <- readTVarIO routinePending
          if S.null pending
            then do
              cleanStaleFiles
              seedRoutinePending wps
              interruptibleSleep
            else do
              hasPriority <- atomically $ not <$> isEmptyTQueue priorityRender
              unless hasPriority interruptibleSleep

        interruptibleSleep = do
          delay <- registerDelay (webUpdateInterval * 1000000)
          atomically $
            (readTVar delay >>= check)
              `orElse` takeTMVar wakeSignal

    initPublishableGroups WebPreviewState {publishableGroupIds} = do
      groups <- withTransaction (chatStore cc) $ \db ->
        concat <$> mapM (getRelayServedGroups db vr') users
      let gIds = M.fromList [(groupId, f) | g@GroupInfo {groupId} <- groups, Just f <- [publicGroupFileName g]]
      atomically $ writeTVar publishableGroupIds gIds

    seedRoutinePending WebPreviewState {publishableGroupIds, routinePending} =
      atomically $ M.keysSet <$> readTVar publishableGroupIds >>= writeTVar routinePending

    renderOneGroup WebPreviewState {publishableGroupIds} gId = do
      publishable <- atomically $ M.member gId <$> readTVar publishableGroupIds
      when publishable $ do
        r <- withTransaction (chatStore cc) $ \db ->
          findUser $ \u -> fmap (\g -> (u, g)) <$> runExceptT (getGroupInfo db vr' u gId)
        case r of
          Just (u, gInfo) | hasPublicGroup gInfo ->
            void $ renderGroupPreview webJsonDir cc u gInfo
          _ -> do
            fName <- atomically $ do
              ids <- readTVar publishableGroupIds
              modifyTVar' publishableGroupIds (M.delete gId)
              pure $ M.lookup gId ids
            forM_ fName $ \f ->
              removeFile (webJsonDir </> f) `catch` \(_ :: SomeException) -> pure ()
            logInfo $ "web preview: group " <> T.pack (show gId) <> " no longer publishable"

    findUser f = go users
      where
        go [] = pure Nothing
        go (u : us) = f u >>= \case
          Right a -> pure (Just a)
          Left _ -> go us

    regenerateCors = do
      groups <- withTransaction (chatStore cc) $ \db ->
        concat <$> mapM (getRelayServedGroups db vr') users
      let entries = mapMaybe groupCorsEntry groups
      forM_ webCorsFile $ writeCorsConfig entries

    groupCorsEntry GroupInfo {groupProfile = GroupProfile {publicGroup}} =
      publicGroup >>= \PublicGroupProfile {publicGroupId, publicGroupAccess} ->
        corsEntry publicGroupId <$> publicGroupAccess

    cleanStaleFiles = do
      groups <- withTransaction (chatStore cc) $ \db ->
        concat <$> mapM (getRelayServedGroups db vr') users
      let activeFiles = S.fromList $ mapMaybe publicGroupFileName [g | g <- groups, hasPublicGroup g]
      removeStaleFiles webJsonDir activeFiles

renderGroupPreview :: FilePath -> ChatController -> User -> GroupInfo -> IO (Maybe (Text, CorsOrigin))
renderGroupPreview webJsonDir cc user gInfo@GroupInfo {groupProfile = gp@GroupProfile {shortDescr = sd, description = wd, publicGroup}} =
  case publicGroup of
    Just PublicGroupProfile {publicGroupId, publicGroupAccess} -> do
      let fName = publicGroupIdFileName publicGroupId <> ".json"
      (items, owners) <- withTransaction (chatStore cc) $ \db -> do
        is <- getGroupWebPreviewItems db user gInfo 50
        os <- getGroupOwners db vr' user gInfo
        pure (is, os)
      ts <- getCurrentTime
      let rendered = mapMaybe toRenderedItem $ rights items
          msgs = map fst rendered
          senders = uniqueSenders $ map memberToProfile owners <> mapMaybe snd rendered
          preview = WebChannelPreview
            { channel = gp,
              shortDescription = toFormattedText =<< sd,
              welcomeMessage = toFormattedText =<< wd,
              members = senders,
              messages = msgs,
              updatedAt = ts
            }
      LB.writeFile (webJsonDir </> fName) (J.encode preview)
      pure $ corsEntry publicGroupId <$> publicGroupAccess
    Nothing -> pure Nothing
  where
    vr' = chatVRange (config cc)

channelChanged :: ChatController -> Int64 -> Bool -> STM ()
channelChanged cc gId updateCors =
  forM_ (webPreviewState cc) $ \WebPreviewState {publishableGroupIds, priorityRender, corsNeeded, routinePending, wakeSignal} -> do
    ids <- readTVar publishableGroupIds
    when (M.member gId ids) $ do
      writeTQueue priorityRender gId
      modifyTVar' routinePending (S.delete gId)
      when updateCors $ writeTVar corsNeeded True
      void $ tryPutTMVar wakeSignal ()

channelRemoved :: ChatController -> Int64 -> STM ()
channelRemoved cc gId =
  forM_ (webPreviewState cc) $ \WebPreviewState {publishableGroupIds, filesToRemove, corsNeeded, routinePending, wakeSignal} -> do
    ids <- readTVar publishableGroupIds
    forM_ (M.lookup gId ids) $ writeTQueue filesToRemove
    modifyTVar' publishableGroupIds (M.delete gId)
    modifyTVar' routinePending (S.delete gId)
    writeTVar corsNeeded True
    void $ tryPutTMVar wakeSignal ()

toRenderedItem :: CChatItem 'CTGroup -> Maybe (WebMessage, Maybe WebMemberProfile)
toRenderedItem (CChatItem _ ChatItem {chatDir, meta = CIMeta {itemTs, itemTimed, itemForwarded, itemEdited}, content, mentions, formattedText, quotedItem, reactions, file})
  | isJust itemTimed = Nothing
  | otherwise = case ciMsgContent content of
      Just mc | not (isReport mc) ->
        let (sender, senderProfile) = case chatDir of
              CIGroupRcv m@GroupMember {memberId} -> (Just memberId, Just $ memberToProfile m)
              _ -> (Nothing, Nothing)
         in Just
              ( WebMessage
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
                  },
                senderProfile
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

uniqueSenders :: [WebMemberProfile] -> [WebMemberProfile]
uniqueSenders = nubBy sameId
  where
    sameId (WebMemberProfile {memberId = a}) (WebMemberProfile {memberId = b}) = a == b

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
  let jsonFiles = S.filter (\f -> takeExtension f == ".json") . S.fromList
  allFiles <- jsonFiles <$> listDirectory dir
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

publicGroupFileName :: GroupInfo -> Maybe FilePath
publicGroupFileName GroupInfo {groupProfile = GroupProfile {publicGroup}} =
  (\PublicGroupProfile {publicGroupId} -> publicGroupIdFileName publicGroupId <> ".json") <$> publicGroup
