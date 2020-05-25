{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simplex.Messaging.Broker where

import Simplex.Messaging.Protocol

-- instance PartyApi m Broker where
--   process cmd = \case
--     Subscribe connId -> m ()
