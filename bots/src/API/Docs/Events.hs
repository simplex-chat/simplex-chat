{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Docs.Events where

import API.TypeInfo
import GHC.Generics
import Simplex.Chat.Controller

deriving instance Generic ChatEvent

chatEventConsInfo :: [RecordTypeInfo]
chatEventConsInfo = gTypeInfo @(Rep ChatEvent)
