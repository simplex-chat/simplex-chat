{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Stats where

import Control.Applicative ((<|>))
import qualified Data.Aeson.TH as J
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Client
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Stats
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.Protocol

data PresentedServersSummary = PresentedServersSummary
  { statsStartedAt :: UTCTime,
    userServersSummary :: ServersSummary,
    allServersSummary :: ServersSummary
  }
  deriving (Show)

-- Presentation of servers will be split into separate categories,
-- so users can differentiate currently used (connected) servers,
-- previously connected servers that were in use in previous sessions,
-- and servers that are only proxied (not connected directly).
data ServersSummary = ServersSummary
  { -- currently used SMP servers are those with Just in sessions and/or subs in SMPServerSummary;
    -- all other servers would fall either into previously used or only proxied servers category
    currentlyUsedSMPServers :: [SMPServerSummary],
    -- previously used SMP servers are those with Nothing in sessions and subs,
    -- and have any of sentDirect, sentProxied, recvMsgs, etc. > 0 in server stats (see toPresentedServersSummary);
    -- remaining servers would fall into only proxied servers category
    previouslyUsedSMPServers :: [SMPServerSummary],
    -- only proxied SMP servers are those that aren't (according to current state - sessions and subs)
    -- and weren't (according to stats) connected directly; they would have Nothing in sessions and subs,
    -- and have all of sentDirect, sentProxied, recvMsgs, etc. = 0 in server stats
    onlyProxiedSMPServers :: [SMPServerSummary],
    -- currently used XFTP servers are those with Just in sessions in XFTPServerSummary,
    -- and/or have upload/download/deletion in progress;
    -- all other servers would fall into previously used servers category
    currentlyUsedXFTPServers :: [XFTPServerSummary],
    -- previously used XFTP servers are those with Nothing in sessions and don't have any process in progress
    previouslyUsedXFTPServers :: [XFTPServerSummary]
  }
  deriving (Show)

data SMPServerSummary = SMPServerSummary
  { smpServer :: SMPServer,
    -- known:
    -- for simplicity always Nothing in totalServersSummary - allows us to load configured servers only for current user,
    -- and also unnecessary unless we want to add navigation to other users servers settings;
    -- always Just in userServersSummary - True if server is in list of user servers, otherwise False;
    -- True - allows to navigate to server settings, False - allows to add server to configured as known (SEKnown)
    known :: Maybe Bool,
    sessions :: Maybe ServerSessions,
    subs :: Maybe SMPServerSubs,
    -- stats:
    -- even if sessions and subs are Nothing, stats can be Just - server could be used earlier in session,
    -- or in previous sessions and stats for it were restored; server would fall into a category of
    -- previously used or only proxied servers - see ServersSummary above
    stats :: Maybe AgentSMPServerStatsData
  }
  deriving (Show)

data XFTPServerSummary = XFTPServerSummary
  { xftpServer :: XFTPServer,
    known :: Maybe Bool, -- same as for SMPServerSummary
    sessions :: Maybe ServerSessions,
    stats :: Maybe AgentXFTPServerStatsData,
    rcvInProgress :: Bool,
    sndInProgress :: Bool,
    delInProgress :: Bool
  }
  deriving (Show)

-- Maps AgentServersSummary to PresentedServersSummary:
-- - userServersSummary is for currentUser;
-- - users are passed to exclude hidden users from totalServersSummary;
-- - if currentUser is hidden, it should be accounted in totalServersSummary;
-- - known is set only in user level summaries based on passed userSMPSrvs and userXFTPSrvs
toPresentedServersSummary :: AgentServersSummary -> [User] -> User -> [SMPServer] -> [XFTPServer] -> PresentedServersSummary
toPresentedServersSummary agentSummary users currentUser userSMPSrvs userXFTPSrvs = do
  let (userSMPSrvsSumms, allSMPSrvsSumms) = accSMPSrvsSummaries
      (userSMPCurr, userSMPPrev, userSMPProx) = smpSumsIntoCategories userSMPSrvsSumms
      (allSMPCurr, allSMPPrev, allSMPProx) = smpSumsIntoCategories allSMPSrvsSumms
      (userXFTPSrvsSumms, allXFTPSrvsSumms) = accXFTPSrvsSummaries
      (userXFTPCurr, userXFTPPrev) = xftpSumsIntoCategories userXFTPSrvsSumms
      (allXFTPCurr, allXFTPPrev) = xftpSumsIntoCategories allXFTPSrvsSumms
  PresentedServersSummary
    { statsStartedAt,
      userServersSummary =
        ServersSummary
          { currentlyUsedSMPServers = sortSMPSums userSMPCurr,
            previouslyUsedSMPServers = sortSMPSums userSMPPrev,
            onlyProxiedSMPServers = sortSMPSums userSMPProx,
            currentlyUsedXFTPServers = sortXFTPSums userXFTPCurr,
            previouslyUsedXFTPServers = sortXFTPSums userXFTPPrev
          },
      allServersSummary =
        ServersSummary
          { currentlyUsedSMPServers = sortSMPSums allSMPCurr,
            previouslyUsedSMPServers = sortSMPSums allSMPPrev,
            onlyProxiedSMPServers = sortSMPSums allSMPProx,
            currentlyUsedXFTPServers = sortXFTPSums allXFTPCurr,
            previouslyUsedXFTPServers = sortXFTPSums allXFTPPrev
          }
    }
  where
    AgentServersSummary {statsStartedAt, smpServersSessions, smpServersSubs, smpServersStats, xftpServersSessions, xftpServersStats, xftpRcvInProgress, xftpSndInProgress, xftpDelInProgress} = agentSummary
    countUserInAll auId = auId == aUserId currentUser || auId `notElem` hiddenUserIds
    hiddenUserIds = map aUserId $ filter (isJust . viewPwdHash) users
    sortSMPSums :: [SMPServerSummary] -> [SMPServerSummary]
    sortSMPSums = sortOn (\SMPServerSummary {smpServer} -> smpServer)
    sortXFTPSums :: [XFTPServerSummary] -> [XFTPServerSummary]
    sortXFTPSums = sortOn (\XFTPServerSummary {xftpServer} -> xftpServer)
    smpSumsIntoCategories :: [SMPServerSummary] -> ([SMPServerSummary], [SMPServerSummary], [SMPServerSummary])
    smpSumsIntoCategories = foldr partitionSummary ([], [], [])
      where
        partitionSummary srvSumm (curr, prev, prox)
          | isCurrentlyUsed srvSumm = (srvSumm : curr, prev, prox)
          | isPreviouslyUsed srvSumm = (curr, srvSumm : prev, prox)
          | otherwise = (curr, prev, srvSumm : prox)
        isCurrentlyUsed SMPServerSummary {sessions, subs} = isJust sessions || isJust subs
        isPreviouslyUsed SMPServerSummary {stats} = case stats of
          Nothing -> False
          -- add connCompleted, connDeleted?
          -- check: should connCompleted be counted for proxy? is it?
          Just AgentSMPServerStatsData {_sentDirect, _sentProxied, _sentDirectAttempts, _sentProxiedAttempts, _recvMsgs, _connCreated, _connSecured, _connSubscribed, _connSubAttempts} ->
            _sentDirect > 0 || _sentProxied > 0 || _sentDirectAttempts > 0 || _sentProxiedAttempts > 0 || _recvMsgs > 0 || _connCreated > 0 || _connSecured > 0 || _connSubscribed > 0 || _connSubAttempts > 0
    xftpSumsIntoCategories :: [XFTPServerSummary] -> ([XFTPServerSummary], [XFTPServerSummary])
    xftpSumsIntoCategories = foldr partitionSummary ([], [])
      where
        partitionSummary srvSumm (curr, prev)
          | isCurrentlyUsed srvSumm = (srvSumm : curr, prev)
          | otherwise = (curr, srvSumm : prev)
        isCurrentlyUsed XFTPServerSummary {sessions, rcvInProgress, sndInProgress, delInProgress} =
          isJust sessions || rcvInProgress || sndInProgress || delInProgress
    mergeSMPSrvsMaps ::
      Map (UserId, SMPServer) ServerSessions ->
      Map (UserId, SMPServer) SMPServerSubs ->
      Map (UserId, SMPServer) AgentSMPServerStatsData ->
      Map (UserId, SMPServer) (Maybe ServerSessions, Maybe SMPServerSubs, Maybe AgentSMPServerStatsData)
    mergeSMPSrvsMaps sessions subs stats = mergedMap3
      where
        combinedMap1 = M.foldrWithKey (\k v acc -> M.insertWith combine k (Just v, Nothing, Nothing) acc) M.empty sessions
        combinedMap2 = M.foldrWithKey (\k v acc -> M.insertWith combine k (Nothing, Just v, Nothing) acc) combinedMap1 subs
        mergedMap3 = M.foldrWithKey (\k v acc -> M.insertWith combine k (Nothing, Nothing, Just v) acc) combinedMap2 stats
        combine (m1, m2, m3) (n1, n2, n3) = (m1 <|> n1, m2 <|> n2, m3 <|> n3)
    accSMPSrvsSummaries :: ([SMPServerSummary], [SMPServerSummary])
    accSMPSrvsSummaries = (userSrvSummaries, allSrvSummaries)
      where
        allSrvSummaries = M.elems allSrvSummariesMap
        (userSrvSummaries, allSrvSummariesMap) = M.foldrWithKey accumulate initialAcc mergedData
        mergedData = mergeSMPSrvsMaps smpServersSessions smpServersSubs smpServersStats
        initialAcc = ([], M.empty)
        accumulate (auId, server) (sessions, subs, stats) (uAcc, allAcc) =
          let knownForUser = Just $ server `elem` userSMPSrvs
              summaryForUser = SMPServerSummary server knownForUser sessions subs stats
              newUAcc = if auId == aUserId currentUser then summaryForUser : uAcc else uAcc
              summaryForAll = SMPServerSummary server Nothing sessions subs stats
              newAllAcc =
                if countUserInAll auId
                  then M.insertWith combineSummaries server summaryForAll allAcc
                  else allAcc
           in (newUAcc, newAllAcc)
        combineSummaries
          (SMPServerSummary _ _ sessions1 subs1 stats1)
          (SMPServerSummary smpServer known sessions2 subs2 stats2) =
            SMPServerSummary
              { smpServer,
                known,
                sessions = addMaybes addServerSessions sessions1 sessions2,
                subs = addMaybes addSMPSubs subs1 subs2,
                stats = addMaybes addSMPStats stats1 stats2
              }
    mergeXFTPSrvsMaps ::
      Map (UserId, XFTPServer) ServerSessions ->
      Map (UserId, XFTPServer) AgentXFTPServerStatsData ->
      Map (UserId, XFTPServer) (Maybe ServerSessions, Maybe AgentXFTPServerStatsData)
    mergeXFTPSrvsMaps sessions stats = mergedMap2
      where
        combinedMap1 = M.foldrWithKey (\k v acc -> M.insertWith combine k (Just v, Nothing) acc) M.empty sessions
        mergedMap2 = M.foldrWithKey (\k v acc -> M.insertWith combine k (Nothing, Just v) acc) combinedMap1 stats
        combine (m1, m2) (n1, n2) = (m1 <|> n1, m2 <|> n2)
    accXFTPSrvsSummaries :: ([XFTPServerSummary], [XFTPServerSummary])
    accXFTPSrvsSummaries = (userSrvSummaries, allSrvSummaries)
      where
        allSrvSummaries = M.elems allSrvSummariesMap
        (userSrvSummaries, allSrvSummariesMap) = M.foldrWithKey accumulate initialAcc mergedData
        mergedData = mergeXFTPSrvsMaps xftpServersSessions xftpServersStats
        initialAcc = ([], M.empty)
        accumulate (auId, server) (sessions, stats) (uAcc, allAcc) =
          let rcvInProgress = server `elem` xftpRcvInProgress
              sndInProgress = server `elem` xftpSndInProgress
              delInProgress = server `elem` xftpDelInProgress
              knownForUser = Just $ server `elem` userXFTPSrvs
              summaryForUser = XFTPServerSummary server knownForUser sessions stats rcvInProgress sndInProgress delInProgress
              newUAcc = if auId == aUserId currentUser then summaryForUser : uAcc else uAcc
              summaryForAll = XFTPServerSummary server Nothing sessions stats rcvInProgress sndInProgress delInProgress
              newAllAcc =
                if countUserInAll auId
                  then M.insertWith combineSummaries server summaryForAll allAcc
                  else allAcc
           in (newUAcc, newAllAcc)
        combineSummaries
          (XFTPServerSummary _ _ sessions1 stats1 _ _ _)
          (XFTPServerSummary xftpServer known sessions2 stats2 rcvInProgress sndInProgress delInProgress) =
            XFTPServerSummary
              { xftpServer,
                known,
                sessions = addMaybes addServerSessions sessions1 sessions2,
                stats = addMaybes addXFTPStats stats1 stats2,
                rcvInProgress,
                sndInProgress,
                delInProgress
              }
    addMaybes :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
    addMaybes add (Just x) (Just y) = Just $ add x y
    addMaybes _ Nothing (Just y) = Just y
    addMaybes _ (Just x) Nothing = Just x
    addMaybes _ Nothing Nothing = Nothing
    addServerSessions :: ServerSessions -> ServerSessions -> ServerSessions
    addServerSessions ss1 ss2 =
      ServerSessions
        { ssConnected = ssConnected ss1 + ssConnected ss2,
          ssErrors = ssErrors ss1 + ssErrors ss2,
          ssConnecting = ssConnecting ss1 + ssConnecting ss2
        }
    addSMPSubs :: SMPServerSubs -> SMPServerSubs -> SMPServerSubs
    addSMPSubs ss1 ss2 =
      SMPServerSubs
        { ssActive = ssActive ss1 + ssActive ss2,
          ssPending = ssPending ss1 + ssPending ss2
        }
    addSMPStats :: AgentSMPServerStatsData -> AgentSMPServerStatsData -> AgentSMPServerStatsData
    addSMPStats sd1 sd2 =
      AgentSMPServerStatsData
        { _sentDirect = _sentDirect sd1 + _sentDirect sd2,
          _sentViaProxy = _sentViaProxy sd1 + _sentViaProxy sd2,
          _sentProxied = _sentProxied sd1 + _sentProxied sd2,
          _sentDirectAttempts = _sentDirectAttempts sd1 + _sentDirectAttempts sd2,
          _sentViaProxyAttempts = _sentViaProxyAttempts sd1 + _sentViaProxyAttempts sd2,
          _sentProxiedAttempts = _sentProxiedAttempts sd1 + _sentProxiedAttempts sd2,
          _sentAuthErrs = _sentAuthErrs sd1 + _sentAuthErrs sd2,
          _sentQuotaErrs = _sentQuotaErrs sd1 + _sentQuotaErrs sd2,
          _sentExpiredErrs = _sentExpiredErrs sd1 + _sentExpiredErrs sd2,
          _sentOtherErrs = _sentOtherErrs sd1 + _sentOtherErrs sd2,
          _recvMsgs = _recvMsgs sd1 + _recvMsgs sd2,
          _recvDuplicates = _recvDuplicates sd1 + _recvDuplicates sd2,
          _recvCryptoErrs = _recvCryptoErrs sd1 + _recvCryptoErrs sd2,
          _recvErrs = _recvErrs sd1 + _recvErrs sd2,
          _connCreated = _connCreated sd1 + _connCreated sd2,
          _connSecured = _connSecured sd1 + _connSecured sd2,
          _connCompleted = _connCompleted sd1 + _connCompleted sd2,
          _connDeleted = _connDeleted sd1 + _connDeleted sd2,
          _connSubscribed = _connSubscribed sd1 + _connSubscribed sd2,
          _connSubAttempts = _connSubAttempts sd1 + _connSubAttempts sd2,
          _connSubErrs = _connSubErrs sd1 + _connSubErrs sd2
        }
    addXFTPStats :: AgentXFTPServerStatsData -> AgentXFTPServerStatsData -> AgentXFTPServerStatsData
    addXFTPStats sd1 sd2 =
      AgentXFTPServerStatsData
        { _uploads = _uploads sd1 + _uploads sd2,
          _uploadAttempts = _uploadAttempts sd1 + _uploadAttempts sd2,
          _uploadErrs = _uploadErrs sd1 + _uploadErrs sd2,
          _downloads = _downloads sd1 + _downloads sd2,
          _downloadAttempts = _downloadAttempts sd1 + _downloadAttempts sd2,
          _downloadAuthErrs = _downloadAuthErrs sd1 + _downloadAuthErrs sd2,
          _downloadErrs = _downloadErrs sd1 + _downloadErrs sd2,
          _deletions = _deletions sd1 + _deletions sd2,
          _deleteAttempts = _deleteAttempts sd1 + _deleteAttempts sd2,
          _deleteErrs = _deleteErrs sd1 + _deleteErrs sd2
        }

$(J.deriveJSON defaultJSON ''SMPServerSummary)

$(J.deriveJSON defaultJSON ''XFTPServerSummary)

$(J.deriveJSON defaultJSON ''ServersSummary)

$(J.deriveJSON defaultJSON ''PresentedServersSummary)
