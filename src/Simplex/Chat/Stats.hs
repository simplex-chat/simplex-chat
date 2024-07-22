{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Stats where

import qualified Data.Aeson.TH as J
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Client
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Stats
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.Protocol

data PresentedServersSummary = PresentedServersSummary
  { statsStartedAt :: UTCTime,
    allUsersSMP :: SMPServersSummary,
    allUsersXFTP :: XFTPServersSummary,
    allUsersNtf :: NtfServersSummary,
    currentUserSMP :: SMPServersSummary,
    currentUserXFTP :: XFTPServersSummary,
    currentUserNtf :: NtfServersSummary
  }
  deriving (Show)

-- Presentation of servers will be split into separate categories,
-- so users can differentiate currently used (connected) servers,
-- previously connected servers that were in use in previous sessions,
-- and servers that are only proxied (not connected directly).
data SMPServersSummary = SMPServersSummary
  { -- SMP totals are calculated from all accounted SMP server summaries
    smpTotals :: SMPTotals,
    -- currently used SMP servers are those with Just in sessions and/or subs in SMPServerSummary;
    -- all other servers would fall either into previously used or only proxied servers category
    currentlyUsedSMPServers :: [SMPServerSummary],
    -- previously used SMP servers are those with Nothing in sessions and subs,
    -- and have any of sentDirect, sentProxied, recvMsgs, etc. > 0 in server stats (see toPresentedServersSummary);
    -- remaining servers would fall into only proxied servers category
    previouslyUsedSMPServers :: [SMPServerSummary],
    -- only proxied SMP servers are those that aren't (according to current state - sessions and subs)
    -- and weren't (according to stats) connected directly; they would have Nothing in sessions and subs,
    -- and have all of sentDirect, sentProxied, recvMsgs, etc. = 0 in server stats
    onlyProxiedSMPServers :: [SMPServerSummary]
  }
  deriving (Show)

data SMPTotals = SMPTotals
  { sessions :: ServerSessions,
    subs :: SMPServerSubs,
    stats :: AgentSMPServerStatsData
  }
  deriving (Show)

data SMPServerSummary = SMPServerSummary
  { smpServer :: SMPServer,
    -- known:
    -- for simplicity always Nothing in totalServersSummary - allows us to load configured servers only for current user,
    -- and also unnecessary unless we want to add navigation to other users servers settings;
    -- always Just in currentUserServers - True if server is in list of user servers, otherwise False;
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

data XFTPServersSummary = XFTPServersSummary
  { -- XFTP totals are calculated from all accounted XFTP server summaries
    xftpTotals :: XFTPTotals,
    -- currently used XFTP servers are those with Just in sessions in XFTPServerSummary,
    -- and/or have upload/download/deletion in progress;
    -- all other servers would fall into previously used servers category
    currentlyUsedXFTPServers :: [XFTPServerSummary],
    -- previously used XFTP servers are those with Nothing in sessions and don't have any process in progress
    previouslyUsedXFTPServers :: [XFTPServerSummary]
  }
  deriving (Show)

data XFTPTotals = XFTPTotals
  { sessions :: ServerSessions,
    stats :: AgentXFTPServerStatsData
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

data NtfServersSummary = NtfServersSummary
  { ntfTotals :: NtfTotals,
    currentlyUsedNtfServers :: [NtfServerSummary],
    previouslyUsedNtfServers :: [NtfServerSummary]
  }
  deriving (Show)

data NtfTotals = NtfTotals
  { sessions :: ServerSessions,
    stats :: AgentNtfServerStatsData
  }
  deriving (Show)

data NtfServerSummary = NtfServerSummary
  { ntfServer :: NtfServer,
    known :: Maybe Bool,
    sessions :: Maybe ServerSessions,
    stats :: Maybe AgentNtfServerStatsData
  }
  deriving (Show)

-- Maps AgentServersSummary to PresentedServersSummary:
-- - currentUserServers is for currentUser;
-- - users are passed to exclude hidden users from totalServersSummary;
-- - if currentUser is hidden, it should be accounted in totalServersSummary;
-- - known is set only in user level summaries based on passed userSMPSrvs and userXFTPSrvs
toPresentedServersSummary :: AgentServersSummary -> [User] -> User -> NonEmpty SMPServer -> NonEmpty XFTPServer -> [NtfServer] -> PresentedServersSummary
toPresentedServersSummary agentSummary users currentUser userSMPSrvs userXFTPSrvs userNtfSrvs = do
  let (userSMPSrvsSumms, allSMPSrvsSumms) = accSMPSrvsSummaries
      (userSMPCurr, userSMPPrev, userSMPProx) = smpSummsIntoCategories userSMPSrvsSumms
      (allSMPCurr, allSMPPrev, allSMPProx) = smpSummsIntoCategories allSMPSrvsSumms
  let (userXFTPSrvsSumms, allXFTPSrvsSumms) = accXFTPSrvsSummaries
      (userXFTPCurr, userXFTPPrev) = xftpSummsIntoCategories userXFTPSrvsSumms
      (allXFTPCurr, allXFTPPrev) = xftpSummsIntoCategories allXFTPSrvsSumms
  let (userNtfSrvsSumms, allNtfSrvsSumms) = accNtfSrvsSummaries
      (userNtfCurr, userNtfPrev) = ntfSummsIntoCategories userNtfSrvsSumms
      (allNtfCurr, allNtfPrev) = ntfSummsIntoCategories allNtfSrvsSumms
  PresentedServersSummary
    { statsStartedAt,
      allUsersSMP =
        SMPServersSummary
          { smpTotals = accSMPTotals allSMPSrvsSumms,
            currentlyUsedSMPServers = allSMPCurr,
            previouslyUsedSMPServers = allSMPPrev,
            onlyProxiedSMPServers = allSMPProx
          },
      allUsersXFTP =
        XFTPServersSummary
          { xftpTotals = accXFTPTotals allXFTPSrvsSumms,
            currentlyUsedXFTPServers = allXFTPCurr,
            previouslyUsedXFTPServers = allXFTPPrev
          },
      allUsersNtf =
        NtfServersSummary
          { ntfTotals = accNtfTotals allNtfSrvsSumms,
            currentlyUsedNtfServers = allNtfCurr,
            previouslyUsedNtfServers = allNtfPrev
          },
      currentUserSMP =
        SMPServersSummary
          { smpTotals = accSMPTotals userSMPSrvsSumms,
            currentlyUsedSMPServers = userSMPCurr,
            previouslyUsedSMPServers = userSMPPrev,
            onlyProxiedSMPServers = userSMPProx
          },
      currentUserXFTP =
        XFTPServersSummary
          { xftpTotals = accXFTPTotals userXFTPSrvsSumms,
            currentlyUsedXFTPServers = userXFTPCurr,
            previouslyUsedXFTPServers = userXFTPPrev
          },
      currentUserNtf =
        NtfServersSummary
          { ntfTotals = accNtfTotals userNtfSrvsSumms,
            currentlyUsedNtfServers = userNtfCurr,
            previouslyUsedNtfServers = userNtfPrev
          }
    }
  where
    AgentServersSummary {statsStartedAt, smpServersSessions, smpServersSubs, smpServersStats, xftpServersSessions, xftpServersStats, xftpRcvInProgress, xftpSndInProgress, xftpDelInProgress, ntfServersSessions, ntfServersStats} = agentSummary
    countUserInAll auId = countUserInAllStats (AgentUserId auId) currentUser users
    accSMPTotals :: Map SMPServer SMPServerSummary -> SMPTotals
    accSMPTotals = M.foldr' addTotals initialTotals
      where
        initialTotals = SMPTotals {sessions = ServerSessions 0 0 0, subs = SMPServerSubs 0 0, stats = newAgentSMPServerStatsData}
        addTotals SMPServerSummary {sessions, subs, stats} SMPTotals {sessions = accSess, subs = accSubs, stats = accStats} =
          SMPTotals
            { sessions = maybe accSess (accSess `addServerSessions`) sessions,
              subs = maybe accSubs (accSubs `addSMPSubs`) subs,
              stats = maybe accStats (accStats `addSMPStatsData`) stats
            }
    accXFTPTotals :: Map XFTPServer XFTPServerSummary -> XFTPTotals
    accXFTPTotals = M.foldr' addTotals initialTotals
      where
        initialTotals = XFTPTotals {sessions = ServerSessions 0 0 0, stats = newAgentXFTPServerStatsData}
        addTotals XFTPServerSummary {sessions, stats} XFTPTotals {sessions = accSess, stats = accStats} =
          XFTPTotals
            { sessions = maybe accSess (accSess `addServerSessions`) sessions,
              stats = maybe accStats (accStats `addXFTPStatsData`) stats
            }
    accNtfTotals :: Map NtfServer NtfServerSummary -> NtfTotals
    accNtfTotals = M.foldr' addTotals initialTotals
      where
        initialTotals = NtfTotals {sessions = ServerSessions 0 0 0, stats = newAgentNtfServerStatsData}
        addTotals NtfServerSummary {sessions, stats} NtfTotals {sessions = accSess, stats = accStats} =
          NtfTotals
            { sessions = maybe accSess (accSess `addServerSessions`) sessions,
              stats = maybe accStats (accStats `addNtfStatsData`) stats
            }
    smpSummsIntoCategories :: Map SMPServer SMPServerSummary -> ([SMPServerSummary], [SMPServerSummary], [SMPServerSummary])
    smpSummsIntoCategories = M.foldr' addSummary ([], [], [])
      where
        addSummary srvSumm (curr, prev, prox)
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
    xftpSummsIntoCategories :: Map XFTPServer XFTPServerSummary -> ([XFTPServerSummary], [XFTPServerSummary])
    xftpSummsIntoCategories = partition isCurrentlyUsed . M.elems
      where
        isCurrentlyUsed XFTPServerSummary {sessions, rcvInProgress, sndInProgress, delInProgress} =
          isJust sessions || rcvInProgress || sndInProgress || delInProgress
    ntfSummsIntoCategories :: Map NtfServer NtfServerSummary -> ([NtfServerSummary], [NtfServerSummary])
    ntfSummsIntoCategories = partition isCurrentlyUsed . M.elems
      where
        isCurrentlyUsed NtfServerSummary {sessions} = isJust sessions
    accSMPSrvsSummaries :: (Map SMPServer SMPServerSummary, Map SMPServer SMPServerSummary)
    accSMPSrvsSummaries = M.foldrWithKey' (addServerData addStats) summs2 smpServersStats
      where
        summs1 = M.foldrWithKey' (addServerData addSessions) (M.empty, M.empty) smpServersSessions
        summs2 = M.foldrWithKey' (addServerData addSubs) summs1 smpServersSubs
        addServerData = addServerData_ newSummary newUserSummary
        newUserSummary srv = (newSummary srv :: SMPServerSummary) {known = Just $ srv `elem` userSMPSrvs}
        newSummary srv =
          SMPServerSummary
            { smpServer = srv,
              known = Nothing,
              sessions = Nothing,
              subs = Nothing,
              stats = Nothing
            }
        addSessions :: ServerSessions -> SMPServerSummary -> SMPServerSummary
        addSessions s summ@SMPServerSummary {sessions} = summ {sessions = Just $ maybe s (s `addServerSessions`) sessions}
        addSubs :: SMPServerSubs -> SMPServerSummary -> SMPServerSummary
        addSubs s summ@SMPServerSummary {subs} = summ {subs = Just $ maybe s (s `addSMPSubs`) subs}
        addStats :: AgentSMPServerStatsData -> SMPServerSummary -> SMPServerSummary
        addStats s summ@SMPServerSummary {stats} = summ {stats = Just $ maybe s (s `addSMPStatsData`) stats}
    accXFTPSrvsSummaries :: (Map XFTPServer XFTPServerSummary, Map XFTPServer XFTPServerSummary)
    accXFTPSrvsSummaries = M.foldrWithKey' (addServerData addStats) summs1 xftpServersStats
      where
        summs1 = M.foldrWithKey' (addServerData addSessions) (M.empty, M.empty) xftpServersSessions
        addServerData = addServerData_ newSummary newUserSummary
        addSessions :: ServerSessions -> XFTPServerSummary -> XFTPServerSummary
        addSessions s summ@XFTPServerSummary {sessions} = summ {sessions = Just $ maybe s (s `addServerSessions`) sessions}
        addStats :: AgentXFTPServerStatsData -> XFTPServerSummary -> XFTPServerSummary
        addStats s summ@XFTPServerSummary {stats} = summ {stats = Just $ maybe s (s `addXFTPStatsData`) stats}
        newUserSummary srv = (newSummary srv :: XFTPServerSummary) {known = Just $ srv `elem` userXFTPSrvs}
        newSummary srv =
          XFTPServerSummary
            { xftpServer = srv,
              known = Nothing,
              sessions = Nothing,
              stats = Nothing,
              rcvInProgress = srv `elem` xftpRcvInProgress,
              sndInProgress = srv `elem` xftpSndInProgress,
              delInProgress = srv `elem` xftpDelInProgress
            }
    accNtfSrvsSummaries :: (Map NtfServer NtfServerSummary, Map NtfServer NtfServerSummary)
    accNtfSrvsSummaries = M.foldrWithKey' (addServerData addStats) summs1 ntfServersStats
      where
        summs1 = M.foldrWithKey' (addServerData addSessions) (M.empty, M.empty) ntfServersSessions
        addServerData = addServerData_ newSummary newUserSummary
        addSessions :: ServerSessions -> NtfServerSummary -> NtfServerSummary
        addSessions s summ@NtfServerSummary {sessions} = summ {sessions = Just $ maybe s (s `addServerSessions`) sessions}
        addStats :: AgentNtfServerStatsData -> NtfServerSummary -> NtfServerSummary
        addStats s summ@NtfServerSummary {stats} = summ {stats = Just $ maybe s (s `addNtfStatsData`) stats}
        newUserSummary srv = (newSummary srv :: NtfServerSummary) {known = Just $ srv `elem` userNtfSrvs}
        newSummary srv =
          NtfServerSummary
            { ntfServer = srv,
              known = Nothing,
              sessions = Nothing,
              stats = Nothing
            }
    addServerData_ ::
      (ProtocolServer p -> s) ->
      (ProtocolServer p -> s) ->
      (a -> s -> s) ->
      (UserId, ProtocolServer p) ->
      a ->
      (Map (ProtocolServer p) s, Map (ProtocolServer p) s) ->
      (Map (ProtocolServer p) s, Map (ProtocolServer p) s)
    addServerData_ newSummary newUserSummary addData (userId, srv) d (userSumms, allUsersSumms) = (userSumms', allUsersSumms')
      where
        userSumms'
          | userId == aUserId currentUser = alterSumms (newUserSummary srv) userSumms
          | otherwise = userSumms
        allUsersSumms'
          | countUserInAll userId = alterSumms (newSummary srv) allUsersSumms
          | otherwise = allUsersSumms
        alterSumms n = M.alter (Just . addData d . fromMaybe n) srv
    addServerSessions :: ServerSessions -> ServerSessions -> ServerSessions
    addServerSessions ss1 ss2 =
      ServerSessions
        { ssConnected = ssConnected ss1 + ssConnected ss2,
          ssErrors = ssErrors ss1 + ssErrors ss2,
          ssConnecting = ssConnecting ss1 + ssConnecting ss2
        }

countUserInAllStats :: AgentUserId -> User -> [User] -> Bool
countUserInAllStats (AgentUserId auId) currentUser users =
  auId == aUserId currentUser || auId `notElem` hiddenUserIds
  where
    hiddenUserIds = map aUserId $ filter (isJust . viewPwdHash) users

addSMPSubs :: SMPServerSubs -> SMPServerSubs -> SMPServerSubs
addSMPSubs ss1 ss2 =
  SMPServerSubs
    { ssActive = ssActive ss1 + ssActive ss2,
      ssPending = ssPending ss1 + ssPending ss2
    }

$(J.deriveJSON defaultJSON ''SMPTotals)

$(J.deriveJSON defaultJSON ''SMPServerSummary)

$(J.deriveJSON defaultJSON ''SMPServersSummary)

$(J.deriveJSON defaultJSON ''XFTPTotals)

$(J.deriveJSON defaultJSON ''XFTPServerSummary)

$(J.deriveJSON defaultJSON ''XFTPServersSummary)

$(J.deriveJSON defaultJSON ''NtfTotals)

$(J.deriveJSON defaultJSON ''NtfServerSummary)

$(J.deriveJSON defaultJSON ''NtfServersSummary)

$(J.deriveJSON defaultJSON ''PresentedServersSummary)
