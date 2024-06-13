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
  { currentlyUsedSMPServers :: [CurrentlyUsedSMPServerSummary],
    previouslyUsedSMPServers :: [PreviouslyUsedSMPServerSummary],
    onlyProxiedSMPServers :: [SMPServer],
    currentlyUsedXFTPServers :: [CurrentlyUsedXFTPServerSummary],
    previouslyUsedXFTPServers :: [PreviouslyUsedXFTPServerSummary]
  }
  deriving (Show)

-- currently used in session - have state;
-- stats are optional in case none were collected yet (?)
data CurrentlyUsedSMPServerSummary = CurrentlyUsedSMPServerSummary
  { smpServer :: SMPServer,
    state :: SMPServerState,
    statsData :: Maybe AgentSMPServerStatsData
  }
  deriving (Show)

-- could be used earlier in session, or in previous sessions and stats for it were restored
data PreviouslyUsedSMPServerSummary = PreviouslyUsedSMPServerSummary
  { smpServer :: SMPServer,
    statsData :: AgentSMPServerStatsData
  }
  deriving (Show)

data CurrentlyUsedXFTPServerSummary = CurrentlyUsedXFTPServerSummary
  { xftpServer :: XFTPServer,
    state :: XFTPServerState,
    statsData :: Maybe AgentXFTPServerStatsData
  }
  deriving (Show)

data PreviouslyUsedXFTPServerSummary = PreviouslyUsedXFTPServerSummary
  { xftpServer :: XFTPServer,
    statsData :: AgentXFTPServerStatsData
  }
  deriving (Show)

toPresentedServersSummary :: AgentServersSummary -> [User] -> User -> PresentedServersSummary
toPresentedServersSummary _agentServersSummary _users _currentUser = do
  -- map from agentServersSummary to PresentedServersSummary
  -- - userServersSummary is for currentUser
  -- - users are passed to exclude hidden users from totalServersSummary
  -- - if currentUser is hidden, it should be accounted in totalServersSummary
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

$(J.deriveJSON defaultJSON ''CurrentlyUsedSMPServerSummary)

$(J.deriveJSON defaultJSON ''PreviouslyUsedSMPServerSummary)

$(J.deriveJSON defaultJSON ''CurrentlyUsedXFTPServerSummary)

$(J.deriveJSON defaultJSON ''PreviouslyUsedXFTPServerSummary)

$(J.deriveJSON defaultJSON ''ServersSummary)

$(J.deriveJSON defaultJSON ''PresentedServersSummary)
