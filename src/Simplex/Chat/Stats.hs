{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Stats where

import qualified Data.Aeson.TH as J
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Client
import Simplex.Messaging.Agent.Stats
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.Protocol

data PresentedServersSummary = PresentedServersSummary
  { userServersSummary :: ServersSummary,
    allServersSummary :: ServersSummary
  }
  deriving (Show)

-- Presentation of servers will be split into separate categories,
-- so users can differentiate currently used (connected) servers,
-- previously connected servers that were in use in previous sessions,
-- and servers that are only proxied (not connected directly).
data ServersSummary = ServersSummary
  { -- currently used SMP servers are those with Just in sessions and/or subs in SMPServerSummary;
    -- all other servers would fall either into previously used or only proxied servers
    currentlyUsedSMPServers :: [SMPServerSummary],
    -- previously used SMP servers are those with Nothing in sessions and subs,
    -- and have sentDirect, sentProxied and/or recvMsgs > 0 in server stats (see AgentSMPServerStats)
    previouslyUsedSMPServers :: [SMPServerSummary],
    -- only proxied SMP servers are those that aren't (according to current state - sessions and subs)
    -- and weren't (according to stats) connected directly; they would have Nothing in sessions and subs,
    -- and have sentDirect, sentProxied and recvMsgs = 0 in server stats
    onlyProxiedSMPServers :: [SMPServerSummary],
    -- currently used XFTP servers are those with Just in sessions in XFTPServerSummary,
    -- and/or have upload/download/deletion in progress
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
    known :: Maybe Bool, -- same as in SMPServerSummary
    sessions :: Maybe ServerSessions,
    rcvInProgress :: Bool,
    sndInProgress :: Bool,
    delInProgress :: Bool
  }
  deriving (Show)

toPresentedServersSummary :: AgentServersSummary -> [User] -> User -> [ServerCfg p1] -> [ServerCfg p2] -> PresentedServersSummary
toPresentedServersSummary _agentServersSummary _users _currentUser _smpSrvs _xftpSrvs = do
  -- map from agentServersSummary to PresentedServersSummary
  -- - userServersSummary is for currentUser
  -- - users are passed to exclude hidden users from totalServersSummary
  -- - if currentUser is hidden, it should be accounted in totalServersSummary
  -- - set known based on passed smpSrvs and xftpSrvs
  PresentedServersSummary
    { userServersSummary =
        ServersSummary
          { currentlyUsedSMPServers = [],
            previouslyUsedSMPServers = [],
            onlyProxiedSMPServers = [],
            currentlyUsedXFTPServers = [],
            previouslyUsedXFTPServers = []
          },
      allServersSummary =
        ServersSummary
          { currentlyUsedSMPServers = [],
            previouslyUsedSMPServers = [],
            onlyProxiedSMPServers = [],
            currentlyUsedXFTPServers = [],
            previouslyUsedXFTPServers = []
          }
    }

$(J.deriveJSON defaultJSON ''SMPServerSummary)

$(J.deriveJSON defaultJSON ''XFTPServerSummary)

$(J.deriveJSON defaultJSON ''ServersSummary)

$(J.deriveJSON defaultJSON ''PresentedServersSummary)
