{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simplex.Messaging.Broker where

import ClassyPrelude
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types

instance ProtocolFunc Broker CreateConnCmd
         CreateConnRequest CreateConnResponse -- request response
         None New Idle Idle                   -- connection states
         where
  protoFunc _ = bCreateConn

bCreateConn :: Connection Broker None Idle
            -> CreateConnRequest
            -> Either String
                      (CreateConnResponse, Connection Broker New Idle)
bCreateConn (Connection str) _ = Right (CreateConnResponse "1" "2", Connection str)
-- TODO stub
