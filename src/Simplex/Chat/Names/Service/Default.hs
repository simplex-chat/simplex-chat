{-# LANGUAGE OverloadedStrings #-}

-- | The single place the names service is wired in.
--
-- Right now it resolves to the in-memory mock, so the whole names UX is
-- exercisable from the terminal CLI with no store payment, no relayer and no
-- chain — while still producing and verifying real signatures.
--
-- When the SMP-backed client lands this binding is what changes; nothing in
-- "Simplex.Chat.Library.Commands" needs to know which implementation it has.
-- The process-wide mock state is a deliberate development shortcut and is the
-- reason this module is separate: it is the seam to delete.
module Simplex.Chat.Names.Service.Default
  ( namesService,
    nameDeployment,
    namesDevMock,
    devMockChain,
  )
where

import Simplex.Chat.Names.Service (NamesService)
import Simplex.Chat.Names.Service.Mock (MockChain, mockDeployment, mockNamesService, newMockChain)
import Simplex.Chat.Names.Snrc (SnrcDeployment)
import System.IO.Unsafe (unsafePerformIO)

-- | Process-wide mock chain. Development only.
devMockChain :: MockChain
devMockChain = unsafePerformIO newMockChain
{-# NOINLINE devMockChain #-}

namesService :: NamesService
namesService = mockNamesService devMockChain

-- | True while the binding above is the mock. Name resolution reads it to fall
-- back to the mock's own records: the mock chain lives in this process, so no
-- SMP names role can see a name bought through it. Goes away with this module.
namesDevMock :: Bool
namesDevMock = True

-- | The deployment the client signs against. Must match the service.
nameDeployment :: SnrcDeployment
nameDeployment = mockDeployment
