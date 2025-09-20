{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Directory.Store
  ( DirectoryStore (..),
    GroupReg (..),
    GroupRegStatus (..),
    UserGroupRegId,
    GroupApprovalId,
    DirectoryGroupData (..),
    DirectoryMemberAcceptance (..),
    DirectoryStatus (..),
    ProfileCondition (..),
    restoreDirectoryStore,
    addGroupReg,
    delGroupReg,
    setGroupStatusStore,
    setGroupPromotedStore,
    grDirectoryStatus,
    setGroupRegOwner,
    getGroupReg,
    getUserGroupReg,
    getUserGroupRegs,
    filterListedGroups,
    groupRegStatusText,
    pendingApproval,
    groupRemoved,
    fromCustomData,
    toCustomData,
    noJoinFilter,
    basicJoinFilter,
    moderateJoinFilter,
    strongJoinFilter,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.List (find, foldl', sortOn)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Simplex.Chat.Types
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON)
import Simplex.Messaging.Util (ifM, whenM)
import System.Directory
import System.IO (BufferMode (..), Handle, IOMode (..), hSetBuffering, openFile)

data DirectoryStore = DirectoryStore
  { groupRegs :: TVar [GroupReg], -- most recent first, reversed when listed
    listedGroups :: TVar (Set GroupId), -- includes promoted
    promotedGroups :: TVar (Set GroupId),
    reservedGroups :: TVar (Set GroupId),
    directoryLogFile :: Maybe Handle
  }

data DirectoryStoreData = DirectoryStoreData
  { groupRegs_ :: [GroupReg],
    listedGroups_ :: Set GroupId,
    promotedGroups_ :: Set GroupId,
    reservedGroups_ :: Set GroupId
  }

data GroupReg = GroupReg
  { dbGroupId :: GroupId,
    userGroupRegId :: UserGroupRegId,
    dbContactId :: ContactId,
    dbOwnerMemberId :: TVar (Maybe GroupMemberId),
    groupRegStatus :: TVar GroupRegStatus,
    promoted :: TVar Bool
  }

data GroupRegData = GroupRegData
  { dbGroupId_ :: GroupId,
    userGroupRegId_ :: UserGroupRegId,
    dbContactId_ :: ContactId,
    dbOwnerMemberId_ :: Maybe GroupMemberId,
    groupRegStatus_ :: GroupRegStatus,
    promoted_ :: Bool
  }

data DirectoryGroupData = DirectoryGroupData
  { memberAcceptance :: DirectoryMemberAcceptance
  }

-- these filters are applied in the order of fields, depending on ProfileCondition:
-- Nothing - do not apply
-- Just
--   PCAll - apply to all profiles
--   PCNoImage - apply to profiles without images
data DirectoryMemberAcceptance = DirectoryMemberAcceptance
  { rejectNames :: Maybe ProfileCondition, -- reject long names and names with profanity
    passCaptcha :: Maybe ProfileCondition, -- run captcha challenge with joining members
    makeObserver :: Maybe ProfileCondition -- the role assigned in the end, after captcha challenge
  }
  deriving (Eq, Show)

data ProfileCondition = PCAll | PCNoImage deriving (Eq, Show)

noJoinFilter :: DirectoryMemberAcceptance
noJoinFilter = DirectoryMemberAcceptance Nothing Nothing Nothing

basicJoinFilter :: DirectoryMemberAcceptance
basicJoinFilter =
  DirectoryMemberAcceptance
    { rejectNames = Just PCNoImage,
      passCaptcha = Nothing,
      makeObserver = Nothing
    }

moderateJoinFilter :: DirectoryMemberAcceptance
moderateJoinFilter =
  DirectoryMemberAcceptance
    { rejectNames = Just PCAll,
      passCaptcha = Just PCNoImage,
      makeObserver = Nothing
    }

strongJoinFilter :: DirectoryMemberAcceptance
strongJoinFilter =
  DirectoryMemberAcceptance
    { rejectNames = Just PCAll,
      passCaptcha = Just PCAll,
      makeObserver = Nothing
    }

type UserGroupRegId = Int64

type GroupApprovalId = Int64

data GroupRegStatus
  = GRSPendingConfirmation
  | GRSProposed
  | GRSPendingUpdate
  | GRSPendingApproval GroupApprovalId
  | GRSActive
  | GRSSuspended
  | GRSSuspendedBadRoles
  | GRSRemoved
  deriving (Eq, Show)

pendingApproval :: GroupRegStatus -> Bool
pendingApproval = \case
  GRSPendingApproval _ -> True
  _ -> False

groupRemoved :: GroupRegStatus -> Bool
groupRemoved = \case
  GRSRemoved -> True
  _ -> False

data DirectoryStatus = DSListed | DSReserved | DSRegistered | DSRemoved
  deriving (Eq)

groupRegStatusText :: GroupRegStatus -> Text
groupRegStatusText = \case
  GRSPendingConfirmation -> "pending confirmation (duplicate names)"
  GRSProposed -> "proposed"
  GRSPendingUpdate -> "pending profile update"
  GRSPendingApproval _ -> "pending admin approval"
  GRSActive -> "active"
  GRSSuspended -> "suspended by admin"
  GRSSuspendedBadRoles -> "suspended because roles changed"
  GRSRemoved -> "removed"

grDirectoryStatus :: GroupRegStatus -> DirectoryStatus
grDirectoryStatus = \case
  GRSActive -> DSListed
  GRSSuspended -> DSReserved
  GRSSuspendedBadRoles -> DSReserved
  GRSRemoved -> DSRemoved
  _ -> DSRegistered

$(JQ.deriveJSON (enumJSON $ dropPrefix "PC") ''ProfileCondition)

$(JQ.deriveJSON defaultJSON ''DirectoryMemberAcceptance)

$(JQ.deriveJSON defaultJSON ''DirectoryGroupData)

fromCustomData :: Maybe CustomData -> DirectoryGroupData
fromCustomData cd_ =
  let memberAcceptance = fromMaybe noJoinFilter $ cd_ >>= \(CustomData o) -> JT.parseMaybe (.: "memberAcceptance") o
   in DirectoryGroupData {memberAcceptance}

toCustomData :: DirectoryGroupData -> CustomData
toCustomData DirectoryGroupData {memberAcceptance} =
  CustomData $ JM.fromList ["memberAcceptance" .= memberAcceptance]

addGroupReg :: DirectoryStore -> Contact -> GroupInfo -> GroupRegStatus -> IO UserGroupRegId
addGroupReg st ct GroupInfo {groupId} grStatus = do
  grData <- addGroupReg_
  logGCreate st grData
  pure $ userGroupRegId_ grData
  where
    addGroupReg_ = do
      let grData = GroupRegData {dbGroupId_ = groupId, userGroupRegId_ = 1, dbContactId_ = ctId, dbOwnerMemberId_ = Nothing, groupRegStatus_ = grStatus, promoted_ = False}
      gr <- dataToGroupReg grData
      atomically $ stateTVar (groupRegs st) $ \grs ->
        let ugrId = 1 + foldl' maxUgrId 0 grs
            grData' = grData {userGroupRegId_ = ugrId}
            gr' = gr {userGroupRegId = ugrId}
         in (grData', gr' : grs)
    ctId = contactId' ct
    maxUgrId mx GroupReg {dbContactId, userGroupRegId}
      | dbContactId == ctId && userGroupRegId > mx = userGroupRegId
      | otherwise = mx

delGroupReg :: DirectoryStore -> GroupReg -> IO ()
delGroupReg st gr@GroupReg {dbGroupId = gId, groupRegStatus} = do
  logGDelete st gId
  atomically $ writeTVar groupRegStatus GRSRemoved
  atomically $ unlistGroup st gr
  atomically $ modifyTVar' (groupRegs st) $ filter ((gId /=) . dbGroupId)

setGroupStatusStore :: DirectoryStore -> GroupReg -> GroupRegStatus -> IO DirectoryStatus
setGroupStatusStore st gr grStatus' = do
  logGUpdateStatus st (dbGroupId gr) grStatus'
  atomically $ do
    grStatus <- swapTVar (groupRegStatus gr) grStatus'
    updateListing st gr
    pure $ grDirectoryStatus grStatus
  where
    status' = grDirectoryStatus grStatus'
    updateListing = case status' of
      DSListed -> listGroup
      DSReserved -> reserveGroup
      DSRegistered -> unlistGroup
      DSRemoved -> unlistGroup

setGroupPromotedStore :: DirectoryStore -> GroupReg -> Bool -> IO (DirectoryStatus, Bool)
setGroupPromotedStore st gr grPromoted' = do
  let gId = dbGroupId gr
  logGUpdatePromotion st gId grPromoted'
  atomically $ do
    grPromoted <- swapTVar (promoted gr) grPromoted'
    status <- grDirectoryStatus <$> readTVar (groupRegStatus gr)
    let update = if status == DSListed && grPromoted' then S.insert else S.delete
    modifyTVar' (promotedGroups st) $ update gId
    pure (status, grPromoted)

setGroupRegOwner :: DirectoryStore -> GroupReg -> GroupMember -> IO ()
setGroupRegOwner st gr owner = do
  let memberId = groupMemberId' owner
  logGUpdateOwner st (dbGroupId gr) memberId
  atomically $ writeTVar (dbOwnerMemberId gr) (Just memberId)

getGroupReg :: DirectoryStore -> GroupId -> IO (Maybe GroupReg)
getGroupReg st gId = find ((gId ==) . dbGroupId) <$> readTVarIO (groupRegs st)

getUserGroupReg :: DirectoryStore -> ContactId -> UserGroupRegId -> IO (Maybe GroupReg)
getUserGroupReg st ctId ugrId = find (\r -> ctId == dbContactId r && ugrId == userGroupRegId r) <$> readTVarIO (groupRegs st)

getUserGroupRegs :: DirectoryStore -> ContactId -> IO [GroupReg]
getUserGroupRegs st ctId = filter ((ctId ==) . dbContactId) <$> readTVarIO (groupRegs st)

filterListedGroups :: DirectoryStore -> [GroupInfoSummary] -> IO [GroupInfoSummary]
filterListedGroups st gs = do
  lgs <- readTVarIO $ listedGroups st
  pure $ filter (\(GIS GroupInfo {groupId} _ _) -> groupId `S.member` lgs) gs

listGroup :: DirectoryStore -> GroupReg -> STM ()
listGroup st gr = do
  let gId = dbGroupId gr
  modifyTVar' (listedGroups st) $ S.insert gId
  whenM (readTVar $ promoted gr) $ modifyTVar' (promotedGroups st) $ S.insert gId
  modifyTVar' (reservedGroups st) $ S.delete gId

reserveGroup :: DirectoryStore -> GroupReg -> STM ()
reserveGroup st gr = do
  let gId = dbGroupId gr
  modifyTVar' (listedGroups st) $ S.delete gId
  modifyTVar' (promotedGroups st) $ S.delete gId
  modifyTVar' (reservedGroups st) $ S.insert gId

unlistGroup :: DirectoryStore -> GroupReg -> STM ()
unlistGroup st gr = do
  let gId = dbGroupId gr
  modifyTVar' (listedGroups st) $ S.delete gId
  modifyTVar' (promotedGroups st) $ S.delete gId
  modifyTVar' (reservedGroups st) $ S.delete gId

data DirectoryLogRecord
  = GRCreate GroupRegData
  | GRDelete GroupId
  | GRUpdateStatus GroupId GroupRegStatus
  | GRUpdatePromotion GroupId Bool
  | GRUpdateOwner GroupId GroupMemberId

data DLRTag
  = GRCreate_
  | GRDelete_
  | GRUpdateStatus_
  | GRUpdatePromotion_
  | GRUpdateOwner_

logDLR :: DirectoryStore -> DirectoryLogRecord -> IO ()
logDLR st r = forM_ (directoryLogFile st) $ \h -> B.hPutStrLn h (strEncode r)

logGCreate :: DirectoryStore -> GroupRegData -> IO ()
logGCreate st = logDLR st . GRCreate

logGDelete :: DirectoryStore -> GroupId -> IO ()
logGDelete st = logDLR st . GRDelete

logGUpdateStatus :: DirectoryStore -> GroupId -> GroupRegStatus -> IO ()
logGUpdateStatus st gId = logDLR st . GRUpdateStatus gId

logGUpdatePromotion :: DirectoryStore -> GroupId -> Bool -> IO ()
logGUpdatePromotion st gId = logDLR st . GRUpdatePromotion gId

logGUpdateOwner :: DirectoryStore -> GroupId -> GroupMemberId -> IO ()
logGUpdateOwner st gId = logDLR st . GRUpdateOwner gId

instance StrEncoding DLRTag where
  strEncode = \case
    GRCreate_ -> "GCREATE"
    GRDelete_ -> "GDELETE"
    GRUpdateStatus_ -> "GSTATUS"
    GRUpdatePromotion_ -> "GPROMOTE"
    GRUpdateOwner_ -> "GOWNER"
  strP =
    A.takeTill (== ' ') >>= \case
      "GCREATE" -> pure GRCreate_
      "GDELETE" -> pure GRDelete_
      "GSTATUS" -> pure GRUpdateStatus_
      "GPROMOTE" -> pure GRUpdatePromotion_
      "GOWNER" -> pure GRUpdateOwner_
      _ -> fail "invalid DLRTag"

instance StrEncoding DirectoryLogRecord where
  strEncode = \case
    GRCreate gr -> strEncode (GRCreate_, gr)
    GRDelete gId -> strEncode (GRDelete_, gId)
    GRUpdateStatus gId grStatus -> strEncode (GRUpdateStatus_, gId, grStatus)
    GRUpdatePromotion gId promoted -> strEncode (GRUpdatePromotion_, gId, promoted)
    GRUpdateOwner gId grOwnerId -> strEncode (GRUpdateOwner_, gId, grOwnerId)
  strP =
    strP_ >>= \case
      GRCreate_ -> GRCreate <$> strP
      GRDelete_ -> GRDelete <$> strP
      GRUpdateStatus_ -> GRUpdateStatus <$> A.decimal <*> _strP
      GRUpdatePromotion_ -> GRUpdatePromotion <$> A.decimal <*> _strP
      GRUpdateOwner_ -> GRUpdateOwner <$> A.decimal <* A.space <*> A.decimal

instance StrEncoding GroupRegData where
  strEncode GroupRegData {dbGroupId_, userGroupRegId_, dbContactId_, dbOwnerMemberId_, groupRegStatus_, promoted_} =
    B.unwords $
      [ "group_id=" <> strEncode dbGroupId_,
        "user_group_id=" <> strEncode userGroupRegId_,
        "contact_id=" <> strEncode dbContactId_,
        "owner_member_id=" <> strEncode dbOwnerMemberId_,
        "status=" <> strEncode groupRegStatus_
      ]
        <> ["promoted=" <> strEncode promoted_ | promoted_]
  strP = do
    dbGroupId_ <- "group_id=" *> strP_
    userGroupRegId_ <- "user_group_id=" *> strP_
    dbContactId_ <- "contact_id=" *> strP_
    dbOwnerMemberId_ <- "owner_member_id=" *> strP_
    groupRegStatus_ <- "status=" *> strP
    promoted_ <- (" promoted=" *> strP) <|> pure False
    pure GroupRegData {dbGroupId_, userGroupRegId_, dbContactId_, dbOwnerMemberId_, groupRegStatus_, promoted_}

instance StrEncoding GroupRegStatus where
  strEncode = \case
    GRSPendingConfirmation -> "pending_confirmation"
    GRSProposed -> "proposed"
    GRSPendingUpdate -> "pending_update"
    GRSPendingApproval gaId -> "pending_approval:" <> strEncode gaId
    GRSActive -> "active"
    GRSSuspended -> "suspended"
    GRSSuspendedBadRoles -> "suspended_bad_roles"
    GRSRemoved -> "removed"
  strP =
    A.takeTill (\c -> c == ' ' || c == ':') >>= \case
      "pending_confirmation" -> pure GRSPendingConfirmation
      "proposed" -> pure GRSProposed
      "pending_update" -> pure GRSPendingUpdate
      "pending_approval" -> GRSPendingApproval <$> (A.char ':' *> A.decimal)
      "active" -> pure GRSActive
      "suspended" -> pure GRSSuspended
      "suspended_bad_roles" -> pure GRSSuspendedBadRoles
      "removed" -> pure GRSRemoved
      _ -> fail "invalid GroupRegStatus"

dataToGroupReg :: GroupRegData -> IO GroupReg
dataToGroupReg GroupRegData {dbGroupId_, userGroupRegId_, dbContactId_, dbOwnerMemberId_, groupRegStatus_, promoted_} = do
  dbOwnerMemberId <- newTVarIO dbOwnerMemberId_
  groupRegStatus <- newTVarIO groupRegStatus_
  promoted <- newTVarIO promoted_
  pure
    GroupReg
      { dbGroupId = dbGroupId_,
        userGroupRegId = userGroupRegId_,
        dbContactId = dbContactId_,
        dbOwnerMemberId,
        groupRegStatus,
        promoted
      }

restoreDirectoryStore :: Maybe FilePath -> IO DirectoryStore
restoreDirectoryStore = \case
  Just f -> ifM (doesFileExist f) (restore f) (newFile f >>= newDirectoryStore . Just)
  Nothing -> newDirectoryStore Nothing
  where
    newFile f = do
      h <- openFile f WriteMode
      hSetBuffering h LineBuffering
      pure h
    restore f = do
      grs <- readDirectoryData f
      renameFile f (f <> ".bak")
      h <- writeDirectoryData f grs -- compact
      mkDirectoryStore h grs

emptyStoreData :: DirectoryStoreData
emptyStoreData = DirectoryStoreData [] S.empty S.empty S.empty

newDirectoryStore :: Maybe Handle -> IO DirectoryStore
newDirectoryStore = (`mkDirectoryStore_` emptyStoreData)

mkDirectoryStore :: Handle -> [GroupRegData] -> IO DirectoryStore
mkDirectoryStore h groups =
  foldM addGroupRegData emptyStoreData groups >>= mkDirectoryStore_ (Just h)
  where
    addGroupRegData d gr@GroupRegData {dbGroupId_ = gId} = do
      gr' <- dataToGroupReg gr
      let !grs' = gr' : groupRegs_ d
      pure $ case grDirectoryStatus $ groupRegStatus_ gr of
        DSListed ->
          let !listed = S.insert gId $ listedGroups_ d
              !promoted = (if promoted_ gr then S.insert gId else id) $ promotedGroups_ d
           in d {groupRegs_ = grs', listedGroups_ = listed, promotedGroups_ = promoted}
        DSReserved ->
          let !reserved = S.insert gId $ reservedGroups_ d
           in d {groupRegs_ = grs', reservedGroups_ = reserved}
        DSRegistered -> d {groupRegs_ = grs'}
        DSRemoved -> d

mkDirectoryStore_ :: Maybe Handle -> DirectoryStoreData -> IO DirectoryStore
mkDirectoryStore_ h d = do
  groupRegs <- newTVarIO $ groupRegs_ d
  listedGroups <- newTVarIO $ listedGroups_ d
  promotedGroups <- newTVarIO $ promotedGroups_ d
  reservedGroups <- newTVarIO $ reservedGroups_ d
  pure DirectoryStore {groupRegs, listedGroups, promotedGroups, reservedGroups, directoryLogFile = h}

readDirectoryData :: FilePath -> IO [GroupRegData]
readDirectoryData f =
  sortOn dbGroupId_ . M.elems
    <$> (foldM processDLR M.empty . B.lines =<< B.readFile f)
  where
    processDLR :: Map GroupId GroupRegData -> ByteString -> IO (Map GroupId GroupRegData)
    processDLR m l = case strDecode l of
      Left e -> m <$ putStrLn ("Error parsing log record: " <> e <> ", " <> B.unpack (B.take 80 l))
      Right r -> case r of
        GRCreate gr@GroupRegData {dbGroupId_ = gId} -> do
          when (isJust $ M.lookup gId m) $
            putStrLn $
              "Warning: duplicate group with ID " <> show gId <> ", group replaced."
          pure $ M.insert gId gr m
        GRDelete gId -> case M.lookup gId m of
          Just _ -> pure $ M.delete gId m
          Nothing -> m <$ putStrLn ("Warning: no group with ID " <> show gId <> ", deletion ignored.")
        GRUpdateStatus gId groupRegStatus_ -> case M.lookup gId m of
          Just gr -> pure $ M.insert gId gr {groupRegStatus_} m
          Nothing -> m <$ putStrLn ("Warning: no group with ID " <> show gId <> ", status update ignored.")
        GRUpdatePromotion gId promoted_ -> case M.lookup gId m of
          Just gr -> pure $ M.insert gId gr {promoted_} m
          Nothing -> m <$ putStrLn ("Warning: no group with ID " <> show gId <> ", promotion update ignored.")
        GRUpdateOwner gId grOwnerId -> case M.lookup gId m of
          Just gr -> pure $ M.insert gId gr {dbOwnerMemberId_ = Just grOwnerId} m
          Nothing -> m <$ putStrLn ("Warning: no group with ID " <> show gId <> ", owner update ignored.")

writeDirectoryData :: FilePath -> [GroupRegData] -> IO Handle
writeDirectoryData f grs = do
  h <- openFile f WriteMode
  hSetBuffering h LineBuffering
  forM_ grs $ B.hPutStrLn h . strEncode . GRCreate
  pure h
