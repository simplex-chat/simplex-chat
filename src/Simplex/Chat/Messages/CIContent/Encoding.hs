{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Simplex.Chat.Messages.CIContent.Encoding where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as J
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Messages.CIContent.Base
import Simplex.Chat.Messages.CIContent.Types

data ACIContent = forall d. MsgDirectionI d => ACIContent (SMsgDirection d) (CIContent d)

deriving instance Show ACIContent

-- platform independent
dbParseACIContent :: Text -> Either String ACIContent
dbParseACIContent = fmap aciContentDBJSON . J.eitherDecodeStrict' . encodeUtf8

-- platform specific
instance FromJSON ACIContent where
  parseJSON = fmap aciContentJSON . J.parseJSON

aciContentJSON :: JSONCIContent -> ACIContent
aciContentJSON = \case
  JCISndMsgContent mc -> ACIContent SMDSnd $ CISndMsgContent mc
  JCIRcvMsgContent mc -> ACIContent SMDRcv $ CIRcvMsgContent mc
  JCISndDeleted cidm -> ACIContent SMDSnd $ CISndDeleted cidm
  JCIRcvDeleted cidm -> ACIContent SMDRcv $ CIRcvDeleted cidm
  JCISndCall {status, duration} -> ACIContent SMDSnd $ CISndCall status duration
  JCIRcvCall {status, duration} -> ACIContent SMDRcv $ CIRcvCall status duration
  JCIRcvIntegrityError err -> ACIContent SMDRcv $ CIRcvIntegrityError err
  JCIRcvDecryptionError err n -> ACIContent SMDRcv $ CIRcvDecryptionError err n
  JCIRcvGroupInvitation {groupInvitation, memberRole} -> ACIContent SMDRcv $ CIRcvGroupInvitation groupInvitation memberRole
  JCISndGroupInvitation {groupInvitation, memberRole} -> ACIContent SMDSnd $ CISndGroupInvitation groupInvitation memberRole
  JCIRcvGroupEvent {rcvGroupEvent} -> ACIContent SMDRcv $ CIRcvGroupEvent rcvGroupEvent
  JCISndGroupEvent {sndGroupEvent} -> ACIContent SMDSnd $ CISndGroupEvent sndGroupEvent
  JCIRcvConnEvent {rcvConnEvent} -> ACIContent SMDRcv $ CIRcvConnEvent rcvConnEvent
  JCISndConnEvent {sndConnEvent} -> ACIContent SMDSnd $ CISndConnEvent sndConnEvent
  JCIRcvChatFeature {feature, enabled, param} -> ACIContent SMDRcv $ CIRcvChatFeature feature enabled param
  JCISndChatFeature {feature, enabled, param} -> ACIContent SMDSnd $ CISndChatFeature feature enabled param
  JCIRcvChatPreference {feature, allowed, param} -> ACIContent SMDRcv $ CIRcvChatPreference feature allowed param
  JCISndChatPreference {feature, allowed, param} -> ACIContent SMDSnd $ CISndChatPreference feature allowed param
  JCIRcvGroupFeature {groupFeature, preference, param} -> ACIContent SMDRcv $ CIRcvGroupFeature groupFeature preference param
  JCISndGroupFeature {groupFeature, preference, param} -> ACIContent SMDSnd $ CISndGroupFeature groupFeature preference param
  JCIRcvChatFeatureRejected {feature} -> ACIContent SMDRcv $ CIRcvChatFeatureRejected feature
  JCIRcvGroupFeatureRejected {groupFeature} -> ACIContent SMDRcv $ CIRcvGroupFeatureRejected groupFeature
  JCISndModerated -> ACIContent SMDSnd CISndModerated
  JCIRcvModerated -> ACIContent SMDRcv CIRcvModerated
  JCIInvalidJSON dir json -> case fromMsgDirection dir of
    AMsgDirection d -> ACIContent d $ CIInvalidJSON json

aciContentDBJSON :: DBJSONCIContent -> ACIContent
aciContentDBJSON = \case
  DBJCISndMsgContent mc -> ACIContent SMDSnd $ CISndMsgContent mc
  DBJCIRcvMsgContent mc -> ACIContent SMDRcv $ CIRcvMsgContent mc
  DBJCISndDeleted cidm -> ACIContent SMDSnd $ CISndDeleted cidm
  DBJCIRcvDeleted cidm -> ACIContent SMDRcv $ CIRcvDeleted cidm
  DBJCISndCall {status, duration} -> ACIContent SMDSnd $ CISndCall status duration
  DBJCIRcvCall {status, duration} -> ACIContent SMDRcv $ CIRcvCall status duration
  DBJCIRcvIntegrityError (DBME err) -> ACIContent SMDRcv $ CIRcvIntegrityError err
  DBJCIRcvDecryptionError err n -> ACIContent SMDRcv $ CIRcvDecryptionError err n
  DBJCIRcvGroupInvitation {groupInvitation, memberRole} -> ACIContent SMDRcv $ CIRcvGroupInvitation groupInvitation memberRole
  DBJCISndGroupInvitation {groupInvitation, memberRole} -> ACIContent SMDSnd $ CISndGroupInvitation groupInvitation memberRole
  DBJCIRcvGroupEvent (RGE rge) -> ACIContent SMDRcv $ CIRcvGroupEvent rge
  DBJCISndGroupEvent (SGE sge) -> ACIContent SMDSnd $ CISndGroupEvent sge
  DBJCIRcvConnEvent (RCE rce) -> ACIContent SMDRcv $ CIRcvConnEvent rce
  DBJCISndConnEvent (SCE sce) -> ACIContent SMDSnd $ CISndConnEvent sce
  DBJCIRcvChatFeature {feature, enabled, param} -> ACIContent SMDRcv $ CIRcvChatFeature feature enabled param
  DBJCISndChatFeature {feature, enabled, param} -> ACIContent SMDSnd $ CISndChatFeature feature enabled param
  DBJCIRcvChatPreference {feature, allowed, param} -> ACIContent SMDRcv $ CIRcvChatPreference feature allowed param
  DBJCISndChatPreference {feature, allowed, param} -> ACIContent SMDSnd $ CISndChatPreference feature allowed param
  DBJCIRcvGroupFeature {groupFeature, preference, param} -> ACIContent SMDRcv $ CIRcvGroupFeature groupFeature preference param
  DBJCISndGroupFeature {groupFeature, preference, param} -> ACIContent SMDSnd $ CISndGroupFeature groupFeature preference param
  DBJCIRcvChatFeatureRejected {feature} -> ACIContent SMDRcv $ CIRcvChatFeatureRejected feature
  DBJCIRcvGroupFeatureRejected {groupFeature} -> ACIContent SMDRcv $ CIRcvGroupFeatureRejected groupFeature
  DBJCISndModerated -> ACIContent SMDSnd CISndModerated
  DBJCIRcvModerated -> ACIContent SMDRcv CIRcvModerated
  DBJCIInvalidJSON dir json -> case fromMsgDirection dir of
    AMsgDirection d -> ACIContent d $ CIInvalidJSON json
