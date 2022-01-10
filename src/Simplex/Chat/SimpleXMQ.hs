{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat.SimpleXMQ where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Type.Equality
import Data.Typeable (Typeable)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import qualified "simplexmq" Simplex.Messaging.Agent.Protocol as P
-- import qualified "simplexmq-legacy" Simplex.Messaging.Agent.Protocol as PL
import Simplex.Messaging.Encoding.String
import "simplexmq" Simplex.Messaging.Parsers (blobFieldDecoder)
import "simplexmq" Simplex.Messaging.Util ((<$?>))

data AgentVersion = AgentV1 | AgentV0

data SAgentVersion (v :: AgentVersion) where
  SAgentV1 :: SAgentVersion 'AgentV1
  SAgentV0 :: SAgentVersion 'AgentV0

deriving instance Eq (SAgentVersion v)

deriving instance Show (SAgentVersion v)

instance TestEquality SAgentVersion where
  testEquality SAgentV1 SAgentV1 = Just Refl
  testEquality SAgentV0 SAgentV0 = Just Refl
  testEquality _ _ = Nothing

class AgentVersionI (a :: AgentVersion) where sAgentVersion :: SAgentVersion a

instance AgentVersionI 'AgentV1 where sAgentVersion = SAgentV1

instance AgentVersionI 'AgentV0 where sAgentVersion = SAgentV0

data ConnReqInv (v :: AgentVersion) where
  ConnReqInv :: P.ConnectionRequestUri 'P.CMInvitation -> ConnReqInv 'AgentV1

-- ConnReqInvV0 :: PL.ConnectionRequest 'PL.CMInvitation -> ConnReqInv 'AgentV0

deriving instance Eq (ConnReqInv v)

deriving instance Show (ConnReqInv v)

data AChatConnReqInv = forall v. AgentVersionI v => ACReqInv (SAgentVersion v) (ConnReqInv v)

instance Eq AChatConnReqInv where
  ACReqInv v cr == ACReqInv v' cr' = case testEquality v v' of
    Just Refl -> cr == cr'
    _ -> False

deriving instance Show AChatConnReqInv

data AChatConnReq (v :: AgentVersion) where
  AChatConnReq :: P.AConnectionRequestUri -> AChatConnReq 'AgentV1

-- AChatConnReqV0 :: PL.AConnectionRequest -> AChatConnReq 'AgentV0

data AAChatConnReq = forall v. AgentVersionI v => ACReq (SAgentVersion v) (AChatConnReq v)

instance AgentVersionI v => StrEncoding (ConnReqInv v) where
  strEncode = \case
    ConnReqInv cReq -> strEncode cReq

  -- ConnReqInvV0 cReq -> PL.serializeConnReq' cReq
  strP = (\(ACReqInv _ cReq) -> checkAgentVersion cReq) <$?> strP

instance StrEncoding AChatConnReqInv where
  strEncode (ACReqInv _ cReq) = strEncode cReq
  strP =
    ACReqInv SAgentV1 <$> (ConnReqInv <$> strP)

-- <|> ACReqInv SAgentV0 <$> (ConnReqInvV0 <$> PL.connReqP')

instance AgentVersionI v => FromJSON (ConnReqInv v) where
  parseJSON = strParseJSON "ConnReqInv"

instance AgentVersionI v => ToJSON (ConnReqInv v) where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromJSON AChatConnReqInv where
  parseJSON = strParseJSON "AChatConnReqInv"

instance ToJSON AChatConnReqInv where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance AgentVersionI v => ToField (ConnReqInv v) where toField = toField . strEncode

instance (AgentVersionI v, Typeable v) => FromField (ConnReqInv v) where fromField = blobFieldDecoder strDecode

instance ToField AChatConnReqInv where toField = toField . strEncode

instance FromField AChatConnReqInv where fromField = blobFieldDecoder strDecode

checkAgentVersion :: forall t v v'. (AgentVersionI v, AgentVersionI v') => t v' -> Either String (t v)
checkAgentVersion x = case testEquality (sAgentVersion @v) (sAgentVersion @v') of
  Just Refl -> Right x
  Nothing -> Left "bad agent version"
