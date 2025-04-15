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
import Simplex.Chat.Types
import Simplex.FileTransfer.Client.Presets (defaultXFTPServers)
import Simplex.Messaging.Agent.Env.SQLite (ServerRoles (..), allRoles)
import Simplex.Messaging.Protocol
import Test.Hspec

operatorTests :: Spec
operatorTests = describe "managing server operators" $ do
  validateServersTest
  updatedServersTest

validateServersTest :: Spec
validateServersTest = describe "validate user servers" $ do
  it "should pass valid user servers" $ validateUserServers [valid] [] `shouldBe` []
  it "should fail without servers" $ do
    validateUserServers [invalidNoServers] [] `shouldBe` [USENoServers aSMP Nothing]
    validateUserServers [invalidDisabled] [] `shouldBe` [USENoServers aSMP Nothing]
    validateUserServers [invalidDisabledOp] [] `shouldBe` [USENoServers aSMP Nothing, USENoServers aXFTP Nothing]
  it "should fail without servers with storage role" $ do
    validateUserServers [invalidNoStorage] [] `shouldBe` [USEStorageMissing aSMP Nothing]
  it "should fail with duplicate host" $ do
    validateUserServers [invalidDuplicate] [] `shouldBe`
      [ USEDuplicateServer aSMP "smp://0YuTwO05YJWS8rkjn9eLJDjQhFKvIYd8d4xG8X1blIU=@smp8.simplex.im,beccx4yfxxbvyhqypaavemqurytl6hozr47wfc7uuecacjqdvwpw2xid.onion" "smp8.simplex.im",
        USEDuplicateServer aSMP "smp://abcd@smp8.simplex.im" "smp8.simplex.im"
      ]
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
    uss <- groupByOperator' (ops'', [], []) -- no stored servers
    length uss `shouldBe` 3
    [op1, op2, op3] <- pure $ map updatedUserServers uss
    [p1, p2] <- pure operators -- presets
    sameServers p1 op1
    sameServers p2 op2
    null (servers' SPSMP op3) `shouldBe` True
    null (servers' SPXFTP op3) `shouldBe` True
  it "adding preset operators and assiging servers to operator for existing users" $ do
    let ops' = updatedServerOperators operators []
        ops'' = saveOps ops'
    uss <-
      groupByOperator'
        ( ops'',
          saveSrvs $ take 3 simplexChatSMPServers <> [newUserServer "smp://abcd@smp.example.im"],
          saveSrvs $ map (presetServer True) $ L.take 3 defaultXFTPServers
        )
    [op1, op2, op3] <- pure $ map updatedUserServers uss
    [p1, p2] <- pure operators -- presets
    sameServers p1 op1
    sameServers p2 op2
    map srvHost' (servers' SPSMP op3) `shouldBe` [["smp.example.im"]]
    null (servers' SPXFTP op3) `shouldBe` True
  where
    addedPreset = \case
      (Just PresetOperator {operator = Just op}, Just (ASO SDBNew op')) -> operatorTag op == operatorTag op'
      _ -> False
    saveOps = zipWith (\i -> second ((\(ASO _ op) -> op {operatorId = DBEntityId i}) <$>)) [1..]
    saveSrvs = zipWith (\i srv -> srv {serverId = DBEntityId i}) [1..]
    sameServers preset op = do
      map srvHost (pServers SPSMP preset) `shouldBe` map srvHost' (servers' SPSMP op)
      map srvHost (pServers SPXFTP preset) `shouldBe` map srvHost' (servers' SPXFTP op)
    srvHost' (AUS _ s) = srvHost s
    PresetServers {operators} = presetServers defaultChatConfig

deriving instance Eq User

deriving instance Eq UserServersError

valid :: UpdatedUserOperatorServers
valid =
  UpdatedUserOperatorServers
    { operator = Just operatorSimpleXChat {operatorId = DBEntityId 1},
      smpServers = map (AUS SDBNew) simplexChatSMPServers,
      xftpServers = map (AUS SDBNew . presetServer True) $ L.toList defaultXFTPServers
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

invalidDuplicate :: UpdatedUserOperatorServers
invalidDuplicate =
  (valid :: UpdatedUserOperatorServers)
    { smpServers = map (AUS SDBNew) $ simplexChatSMPServers <> [presetServer True "smp://abcd@smp8.simplex.im"]
    }
