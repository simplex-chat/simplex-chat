{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module OperatorTests (operatorTests) where

import Data.Bifunctor (second)
import qualified Data.List.NonEmpty as L
import Simplex.Chat
import Simplex.Chat.Controller (ChatConfig (..), PresetServers (..))
import Simplex.Chat.Operators
import Simplex.Chat.Operators.Presets
import Simplex.Chat.Protocol (RelayProfile (..), mkRelayProfile)
import Simplex.Chat.Types
import Simplex.FileTransfer.Client.Presets (defaultXFTPServers)
import Simplex.Messaging.Agent.Env.SQLite (ServerCfg (..), ServerRoles (..), allRoles)
import Simplex.Messaging.Agent.Store.Entity
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Protocol
import Test.Hspec

operatorTests :: Spec
operatorTests = describe "managing server operators" $ do
  validateServersTest
  updatedServersTest
  perServerRolesTest

validateServersTest :: Spec
validateServersTest = describe "validate user servers" $ do
  it "should pass valid user servers" $ validateUserServers [valid] [] `shouldBe` ([], [])
  it "should fail without servers" $ do
    validateUserServers [invalidNoServers] [] `shouldBe` ([USENoServers aSMP Nothing], [USWNoNamesServers Nothing])
    validateUserServers [invalidDisabled] [] `shouldBe` ([USENoServers aSMP Nothing], [USWNoNamesServers Nothing])
    validateUserServers [invalidDisabledOp] [] `shouldBe` ([USENoServers aSMP Nothing, USENoServers aXFTP Nothing], [USWNoChatRelays Nothing, USWNoNamesServers Nothing])
  it "should fail without servers with storage role" $ do
    validateUserServers [invalidNoStorage] [] `shouldBe` ([USEStorageMissing aSMP Nothing], [])
  it "should fail with duplicate host" $ do
    validateUserServers [invalidDuplicateSrv] []
      `shouldBe` ( [ USEDuplicateServer aSMP "smp://0YuTwO05YJWS8rkjn9eLJDjQhFKvIYd8d4xG8X1blIU=@smp8.simplex.im,beccx4yfxxbvyhqypaavemqurytl6hozr47wfc7uuecacjqdvwpw2xid.onion" "smp8.simplex.im",
                     USEDuplicateServer aSMP "smp://abcd@smp8.simplex.im" "smp8.simplex.im"
                   ],
                   []
                 )
  it "should warn without chat relays" $
    validateUserServers [invalidNoChatRelays] [] `shouldBe` ([], [USWNoChatRelays Nothing])
  it "should allow duplicate chat relay name" $
    validateUserServers [duplicateChatRelayName] [] `shouldBe` ([], [])
  it "should fail with duplicate chat relay address" $ do
    validateUserServers [invalidDuplicateChatRelayAddress] []
      `shouldBe` ( [ USEDuplicateChatRelayAddress "SimpleX Chat Relay 2" duplicateAddr,
                     USEDuplicateChatRelayAddress "chat_relay_4" duplicateAddr
                   ],
                   []
                 )
  where
    aSMP = AProtocolType SPSMP
    aXFTP = AProtocolType SPXFTP

updatedServersTest :: Spec
updatedServersTest = describe "validate user servers" $ do
  it "adding preset operators on first start" $ do
    let ops' :: [(Maybe PresetOperator, Maybe AServerOperator)] =
          updatedServerOperators operators []
    length ops' `shouldBe` 2
    all addedPreset ops' `shouldBe` True
    let ops'' :: [(Maybe PresetOperator, Maybe ServerOperator)] =
          saveOps ops' -- mock getUpdateServerOperators
    uss <- groupByOperator' (ops'', [], [], []) -- no stored servers or relays
    length uss `shouldBe` 3
    [op1, op2, op3] <- pure $ map updatedUserServers uss
    [p1, p2] <- pure operators -- presets
    sameServers p1 op1
    sameRelays p1 op1
    sameServers p2 op2
    sameRelays p2 op2
    null (servers' SPSMP op3) `shouldBe` True
    null (servers' SPXFTP op3) `shouldBe` True
    null (chatRelays' op3) `shouldBe` True
  it "adding preset operators and assigning servers to operator for existing users" $ do
    let ops' = updatedServerOperators operators []
        ops'' = saveOps ops'
    uss <-
      groupByOperator'
        ( ops'',
          saveSrvs $ take 3 simplexChatSMPServers <> [newUserServer "smp://abcd@smp.example.im"],
          saveSrvs $ map (presetServer True) $ L.take 3 defaultXFTPServers,
          saveRelays $ take 2 simplexChatRelays <> [newChatRelay (mkRelayProfile "custom_relay" Nothing) ["example.im"] customRelayAddr]
        )
    [op1, op2, op3] <- pure $ map updatedUserServers uss
    [p1, p2] <- pure operators -- presets
    sameServers p1 op1
    sameRelays p1 op1
    sameServers p2 op2
    sameRelays p2 op2
    map srvHost' (servers' SPSMP op3) `shouldBe` [["smp.example.im"]]
    null (servers' SPXFTP op3) `shouldBe` True
    map relayName' (chatRelays' op3) `shouldBe` ["custom_relay"]
  where
    addedPreset = \case
      (Just PresetOperator {operator = Just op}, Just (ASO SDBNew op')) -> operatorTag op == operatorTag op'
      _ -> False
    saveOps = zipWith (\i -> second ((\(ASO _ op) -> op {operatorId = DBEntityId i}) <$>)) [1 ..]
    saveSrvs = zipWith (\i srv -> srv {serverId = DBEntityId i}) [1 ..]
    saveRelays = zipWith (\i relay -> relay {chatRelayId = DBEntityId i}) [1 ..]
    sameServers preset op = do
      map srvHost (pServers SPSMP preset) `shouldBe` map srvHost' (servers' SPSMP op)
      map srvHost (pServers SPXFTP preset) `shouldBe` map srvHost' (servers' SPXFTP op)
    sameRelays PresetOperator {chatRelays = presetRelays} op =
      map chatRelayAddress presetRelays `shouldBe` map relayAddr' (chatRelays' op)
    srvHost' (AUS _ s) = srvHost s
    relayAddr' (AUCR _ r) = chatRelayAddress r
    relayName' (AUCR _ UserChatRelay {relayProfile = RelayProfile {displayName}}) = displayName
    PresetServers {operators} = presetServers defaultChatConfig
    customRelayAddr = either error id $ strDecode "https://relay.example.im/r#Pz9qz7ZVljMofoRxiDDpL_w2DZSazK8IgafxqnWKv6Y"

perServerRolesTest :: Spec
perServerRolesTest = describe "per-server roles" $ do
  describe "agentServerCfgs resolution" $ do
    it "self-hosted server keeps its per-server roles" $
      case agentServerCfgs SPSMP opDomains [selfHostedSMP (ServerRolesOverride (Just True) (Just False) (Just True))] of
        [ServerCfg {operator, roles}] -> do
          operator `shouldBe` Nothing
          rolesTuple roles `shouldBe` (True, False, True)
        cfgs -> expectationFailure $ "expected one self-hosted ServerCfg, got: " <> show cfgs
    it "self-hosted server without roles falls back to default roles" $
      case agentServerCfgs SPSMP opDomains [selfHostedSMP emptyServerRolesOverride] of
        [ServerCfg {operator, roles}] -> do
          operator `shouldBe` Nothing
          rolesTuple roles `shouldBe` (True, True, False)
        cfgs -> expectationFailure $ "expected one self-hosted ServerCfg, got: " <> show cfgs
    it "self-hosted partial override keeps defaults for unset roles" $
      case agentServerCfgs SPSMP opDomains [selfHostedSMP (ServerRolesOverride Nothing Nothing (Just True))] of
        [ServerCfg {operator, roles}] -> do
          operator `shouldBe` Nothing
          rolesTuple roles `shouldBe` (True, True, True)
        cfgs -> expectationFailure $ "expected one self-hosted ServerCfg, got: " <> show cfgs
    it "self-hosted explicit No overrides a default-yes role" $
      case agentServerCfgs SPSMP opDomains [selfHostedSMP (ServerRolesOverride (Just False) Nothing Nothing)] of
        [ServerCfg {operator, roles}] -> do
          operator `shouldBe` Nothing
          rolesTuple roles `shouldBe` (False, True, False)
        cfgs -> expectationFailure $ "expected one self-hosted ServerCfg, got: " <> show cfgs
    it "operator-matched server applies its override over operator roles" $
      -- operator roles are all on; override turns storage/names off, proxy inherits (on)
      case agentServerCfgs SPSMP opDomains [opMatchedSMP (ServerRolesOverride (Just False) Nothing (Just False))] of
        [ServerCfg {operator, roles}] -> do
          operator `shouldBe` Just 1
          rolesTuple roles `shouldBe` (False, True, False)
        cfgs -> expectationFailure $ "expected one operator-matched ServerCfg, got: " <> show cfgs
    it "two self-hosted servers resolve their three roles independently" $
      -- agentServerCfgs preserves input order
      let a = (newUserServer "smp://abcd@self.example.com" :: NewUserServer 'PSMP) {roles = ServerRolesOverride (Just False) (Just True) (Just True)}
          b = (newUserServer "smp://abcd@self2.example.com" :: NewUserServer 'PSMP) {roles = ServerRolesOverride (Just True) (Just False) Nothing}
       in case agentServerCfgs SPSMP opDomains [a, b] of
            [ServerCfg {operator = opA, roles = rolesA}, ServerCfg {operator = opB, roles = rolesB}] -> do
              (opA, opB) `shouldBe` (Nothing, Nothing)
              rolesTuple rolesA `shouldBe` (False, True, True)
              rolesTuple rolesB `shouldBe` (True, False, False)
            cfgs -> expectationFailure $ "expected two self-hosted ServerCfgs, got: " <> show cfgs
  describe "validateUserServers per-server names coverage" $ do
    it "self-hosted-only user without names servers warns USWNoNamesServers" $ do
      let (_errs, warns) = validateUserServers [selfHostedUser emptyServerRolesOverride] []
      warns `shouldSatisfy` elem (USWNoNamesServers Nothing)
    it "self-hosted user with a names server does not warn USWNoNamesServers" $ do
      let (_errs, warns) = validateUserServers [selfHostedUser (ServerRolesOverride Nothing Nothing (Just True))] []
      warns `shouldSatisfy` notElem (USWNoNamesServers Nothing)
  where
    testOp = operatorSimpleXChat {operatorId = DBEntityId 1}
    opDomains = operatorDomains [testOp]
    -- host matches no operator domain -> self-hosted
    selfHostedSMP :: ServerRolesOverride -> NewUserServer 'PSMP
    selfHostedSMP r = (newUserServer "smp://abcd@self.example.com" :: NewUserServer 'PSMP) {roles = r}
    -- host matches operator domain simplex.im
    opMatchedSMP :: ServerRolesOverride -> NewUserServer 'PSMP
    opMatchedSMP r = (newUserServer "smp://abcd@smp8.simplex.im" :: NewUserServer 'PSMP) {roles = r}
    selfHostedUser :: ServerRolesOverride -> UpdatedUserOperatorServers
    selfHostedUser r =
      UpdatedUserOperatorServers
        { operator = Nothing,
          smpServers = [AUS SDBNew $ selfHostedSMP r],
          xftpServers = [],
          chatRelays = []
        }
    rolesTuple :: ServerRoles -> (Bool, Bool, Bool)
    rolesTuple ServerRoles {storage, proxy, names} = (storage, proxy, names)

deriving instance Eq User

deriving instance Eq UserServersError

deriving instance Eq UserServersWarning

valid :: UpdatedUserOperatorServers
valid =
  UpdatedUserOperatorServers
    { operator = Just operatorSimpleXChat {operatorId = DBEntityId 1},
      smpServers = map (AUS SDBNew) simplexChatSMPServers,
      xftpServers = map (AUS SDBNew . presetServer True) $ L.toList defaultXFTPServers,
      chatRelays = map (AUCR SDBNew) simplexChatRelays
    }

invalidNoServers :: UpdatedUserOperatorServers
invalidNoServers = (valid :: UpdatedUserOperatorServers) {smpServers = []}

invalidDisabled :: UpdatedUserOperatorServers
invalidDisabled =
  (valid :: UpdatedUserOperatorServers)
    { smpServers = map (AUS SDBNew . (\srv -> (srv :: NewUserServer 'PSMP) {enabled = False})) simplexChatSMPServers
    }

invalidDisabledOp :: UpdatedUserOperatorServers
invalidDisabledOp =
  (valid :: UpdatedUserOperatorServers)
    { operator = Just operatorSimpleXChat {operatorId = DBEntityId 1, enabled = False}
    }

invalidNoStorage :: UpdatedUserOperatorServers
invalidNoStorage =
  (valid :: UpdatedUserOperatorServers)
    { operator = Just operatorSimpleXChat {operatorId = DBEntityId 1, smpRoles = allRoles {storage = False}}
    }

invalidDuplicateSrv :: UpdatedUserOperatorServers
invalidDuplicateSrv =
  (valid :: UpdatedUserOperatorServers)
    { smpServers = map (AUS SDBNew) $ simplexChatSMPServers <> [presetServer True "smp://abcd@smp8.simplex.im"]
    }

invalidNoChatRelays :: UpdatedUserOperatorServers
invalidNoChatRelays = (valid :: UpdatedUserOperatorServers) {chatRelays = []}

duplicateChatRelayName :: UpdatedUserOperatorServers
duplicateChatRelayName =
  (valid :: UpdatedUserOperatorServers)
    { chatRelays = map (AUCR SDBNew) $ simplexChatRelays <> [presetChatRelay True (mkRelayProfile "chat_relay_1" Nothing) ["simplex.im"] (either error id $ strDecode "https://smp444.simplex.im/r#Pz9qz7ZVljMofoRxiDDpL_w2DZSazK8IgafxqnWKv6Y")]
    }

invalidDuplicateChatRelayAddress :: UpdatedUserOperatorServers
invalidDuplicateChatRelayAddress =
  (valid :: UpdatedUserOperatorServers)
    { chatRelays = map (AUCR SDBNew) $ simplexChatRelays <> [presetChatRelay True (mkRelayProfile "chat_relay_4" Nothing) ["simplex.im"] duplicateAddr]
    }

duplicateAddr :: ShortLinkContact
duplicateAddr = either error id $ strDecode "https://smp6.simplex.im/r#_qlQfogHGDJ8MAF2wKmkglRBM-xHR142gDJstKiGRQQ"
