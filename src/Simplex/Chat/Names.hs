{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Names
  ( SimplexDomainClaim (..),
    SimplexDomainProof (..),
    mkDomainClaim,
    claimDomain,
  )
where

import qualified Data.Aeson.TH as JQ
import Simplex.Chat.Badges (ProofPresHeader)
import Simplex.Messaging.Agent.Protocol (OwnerId, SimplexDomain)
import Simplex.Messaging.Agent.Store.DB (fromTextField_)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.Util (decodeJSON, encodeJSON)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
#else
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
#endif

-- A name claim proof: signed by the address owner's key over proof payload - see verifyDomainProof.
data SimplexDomainProof = SimplexDomainProof
  { linkOwnerId :: Maybe (StrJSON "OwnerId" OwnerId),
    presHeader :: ProofPresHeader,
    signature :: C.Signature 'C.Ed25519
  }
  deriving (Eq, Show)

$(JQ.deriveJSON defaultJSON ''SimplexDomainProof)

instance ToField SimplexDomainProof where toField = toField . encodeJSON

instance FromField SimplexDomainProof where fromField = fromTextField_ decodeJSON

data SimplexDomainClaim = SimplexDomainClaim
  { domain :: StrJSON "SimplexDomain" SimplexDomain,
    proof :: Maybe SimplexDomainProof
  }
  deriving (Eq, Show)

mkDomainClaim :: SimplexDomain -> SimplexDomainClaim
mkDomainClaim = (`SimplexDomainClaim` Nothing) . StrJSON

claimDomain :: SimplexDomainClaim -> SimplexDomain
claimDomain (SimplexDomainClaim n _) = unStrJSON n

$(JQ.deriveJSON defaultJSON ''SimplexDomainClaim)
