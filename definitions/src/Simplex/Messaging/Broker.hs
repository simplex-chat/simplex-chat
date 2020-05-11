{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simplex.Messaging.Broker where

import ClassyPrelude
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types

-- For some reason PrfCmd type-class requirement is not enforced here:
-- if I change one of the connection states to one for which
-- instance of PrfCmd does not exist (i.e. Command cannot be constructed),
-- it compiles anyway.
-- Creating Command instance here would prevent compilation
-- in case the types are incorrect, as it is done in Client.hs
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


-- below code should NOT compile, but it does
instance ProtocolFunc Broker CreateConnCmd
         CreateConnRequest CreateConnResponse -- request response
         None Secured Idle Idle -- "Secured" should not be allowed here,
         where                  -- such command does not exist, so no instance of
  protoFunc _ = bCreateConn'    -- PrfCmd exist...? But it compiles.

bCreateConn' :: Connection Broker None Idle
            -> CreateConnRequest
            -> Either String
                      (CreateConnResponse, Connection Broker Secured Idle)
bCreateConn' (Connection str) _ = Right (CreateConnResponse "1" "2", Connection str)
