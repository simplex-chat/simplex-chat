{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simplex.Messaging.Client where

import ClassyPrelude
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types

  
instance ProtocolAction Recipient CreateConnCmd
         CreateConnRequest CreateConnResponse  -- request responce
         None New Idle Idle                    -- connection states
         where 
  protoAction _ = rCreateConn


rCreateConn :: Connection Recipient None Idle
            -> CreateConnRequest
            -> Either String CreateConnResponse
            -> Connection Recipient New Idle
rCreateConn (Connection str) _ _ = Connection str --  TODO stub
