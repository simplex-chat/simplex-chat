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
    totalServersSummary :: ServersSummary
  }
  deriving (Show)

data ServersSummary = ServersSummary
  { currentlyUsedSMPServers :: [SMPServerSummary],
    previouslyUsedSMPServers :: [SMPServerSummary],
    onlyProxiedSMPServers :: [SMPServerSummary],
    currentlyUsedXFTPServers :: [XFTPServerSummary],
    previouslyUsedXFTPServers :: [XFTPServerSummary]
  }
  deriving (Show)

data SMPServerSummary = SMPServerSummary
  { smpServer :: SMPServer,
    -- known:
    -- always Nothing in totalServersSummary - to avoid hassle of navigating to other users server settings;
    -- always Just in userServersSummary - True if server is in list of user servers, otherwise False;
    -- True - allows to navigate to server settings, False - allows to add server to configured as known (SEKnown)
    known :: Maybe Bool,
    -- sessions:
    -- Just if currently used in session, otherwise Nothing;
    -- onlyProxiedSMPServers would always have sessions and stats as Nothing
    sessions :: Maybe SMPServerSessions,
    -- stats:
    -- even if sessions is Nothing, stats can be Just - server could be used earlier in session
    -- or in previous sessions and stats for it were restored
    stats :: Maybe AgentSMPServerStatsData
  }
  deriving (Show)

data XFTPServerSummary = CurrentlyUsedXFTPServerSummary
  { xftpServer :: XFTPServer,
    known :: Maybe Bool,
    sessions :: Maybe XFTPServerSessions,
    stats :: Maybe AgentXFTPServerStatsData
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
      totalServersSummary =
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
