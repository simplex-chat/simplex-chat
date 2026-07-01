{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Names
  ( SimplexNameClaim (..),
    mkSimplexNameClaim,
    claimName,
    claimProof,
    setClaimProof,
    NameClaimProof (..),
    verifyNameProofSig,
  )
where

import qualified Data.Aeson.TH as JQ
import Data.ByteString.Char8 (ByteString)
import Simplex.Chat.Badges (ProofPresHeader)
import Simplex.Messaging.Agent.Protocol (ConnShortLink (..), ConnectionMode (..), OwnerId)
import Simplex.Messaging.Agent.Store.DB (fromTextField_)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.SimplexName (SimplexNameInfo)
import Simplex.Messaging.Util (decodeJSON, encodeJSON)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
#else
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
#endif

-- A name claim proof: signed by the address owner's key (linkOwnerId = Just oid when a channel
-- owner other than the address signs, Nothing when the address's own root key signs) over
-- the name-proof payload, binding the name to the address (see nameProofPayload).
data NameClaimProof = NameClaimProof
  { linkOwnerId :: Maybe (StrJSON "OwnerId" OwnerId),
    presHeader :: ProofPresHeader,
    signature :: C.Signature 'C.Ed25519
  }
  deriving (Eq, Show)

nameProofPayload :: SimplexNameInfo -> ProofPresHeader -> ConnShortLink 'CMContact -> ByteString
nameProofPayload name presHeader (CSLContact _ ct srv key) =
  strEncode (Str "simplex_names_v1", presHeader, name, ct, srv, key)

verifyNameProofSig :: C.PublicKeyEd25519 -> SimplexNameInfo -> ConnShortLink 'CMContact -> NameClaimProof -> Bool
verifyNameProofSig ownerKey name sLnk NameClaimProof {presHeader, signature} =
  C.verify' ownerKey signature (nameProofPayload name presHeader sLnk)

$(JQ.deriveJSON defaultJSON ''NameClaimProof)

-- stored as JSON in contact_profiles.simplex_name_proof
instance ToField NameClaimProof where toField = toField . encodeJSON

instance FromField NameClaimProof where fromField = fromTextField_ decodeJSON

data SimplexNameClaim = SimplexNameClaim
  { name :: StrJSON "SimplexNameInfo" SimplexNameInfo,
    proof :: Maybe NameClaimProof
  }
  deriving (Eq, Show)

mkSimplexNameClaim :: Maybe SimplexNameInfo -> Maybe NameClaimProof -> Maybe SimplexNameClaim
mkSimplexNameClaim name_ proof_ = (\n -> SimplexNameClaim (StrJSON n) proof_) <$> name_

claimName :: SimplexNameClaim -> SimplexNameInfo
claimName (SimplexNameClaim n _) = unStrJSON n

claimProof :: SimplexNameClaim -> Maybe NameClaimProof
claimProof (SimplexNameClaim _ p) = p

setClaimProof :: Maybe NameClaimProof -> SimplexNameClaim -> SimplexNameClaim
setClaimProof p (SimplexNameClaim n _) = SimplexNameClaim n p

$(JQ.deriveJSON defaultJSON ''SimplexNameClaim)
