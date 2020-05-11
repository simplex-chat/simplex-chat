{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Simplex.Messaging.Client where

import ClassyPrelude
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types

  
instance ProtocolAction Recipient CreateConnCmd
         CreateConnRequest CreateConnResponse  -- request responce
         None New Idle Idle                    -- connection states
         where 
  protoAction _ = \(Connection str) _ _ -> Connection str --  TODO stub


rCreateConn :: Connection Recipient None Idle
            -> CreateConnRequest
            -> Either String CreateConnResponse
            -> Connection Recipient New Idle
rCreateConn = protoAction $ CreateConn @None
