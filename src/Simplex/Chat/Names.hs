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
    signNameProof,
    verifyNameProofSig,
  )
where

import qualified Data.Aeson.TH as JQ
import Data.ByteString.Char8 (ByteString)
import Simplex.Chat.Badges (ProofPresHeader)
import Simplex.Messaging.Agent.Protocol (OwnerId)
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
-- strEncode name <> strEncode presHeader, tied to the link it is shown through.
data NameClaimProof = NameClaimProof
  { linkOwnerId :: Maybe (StrJSON "OwnerId" OwnerId),
    presHeader :: ProofPresHeader,
    signature :: C.Signature 'C.Ed25519
  }
  deriving (Eq, Show)

nameProofPayload :: SimplexNameInfo -> ProofPresHeader -> ByteString
nameProofPayload name presHeader = strEncode name <> strEncode presHeader

-- linkOwnerId names the signing owner in the link's owner chain (Nothing = root key for a contact address).
signNameProof :: C.PrivateKeyEd25519 -> Maybe OwnerId -> SimplexNameInfo -> ProofPresHeader -> NameClaimProof
signNameProof key linkOwnerId name presHeader =
  NameClaimProof
    { linkOwnerId = StrJSON <$> linkOwnerId,
      presHeader,
      signature = C.sign' key (nameProofPayload name presHeader)
    }

-- verify a name proof's signature against the resolved address owner key. The caller must
-- SEPARATELY check the proof's presHeader link is the one it was shown through, so a proof made
-- for one link can't be reused on another.
verifyNameProofSig :: C.PublicKeyEd25519 -> SimplexNameInfo -> NameClaimProof -> Bool
verifyNameProofSig ownerKey name NameClaimProof {presHeader, signature} =
  C.verify' ownerKey signature (nameProofPayload name presHeader)

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
