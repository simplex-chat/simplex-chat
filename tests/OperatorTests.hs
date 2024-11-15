{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module OperatorTests (operatorTests) where

import qualified Data.List.NonEmpty as L
import Simplex.Chat
import Simplex.Chat.Operators
import Simplex.Chat.Types
import Simplex.FileTransfer.Client.Presets (defaultXFTPServers)
import Simplex.Messaging.Agent.Env.SQLite (ServerRoles (..), allRoles)
import Simplex.Messaging.Protocol
import Test.Hspec

operatorTests :: Spec
operatorTests = describe "managing server operators" $ do
  validateServers

validateServers :: Spec
validateServers = describe "validate user servers" $ do
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
  it "should fail with invalid host" $ do
    validateUserServers [invalidHost] [] `shouldBe` [USENoServers aXFTP Nothing, USEInvalidServer aSMP "smp:abcd@smp8.simplex.im"]
  where
    aSMP = AProtocolType SPSMP
    aXFTP = AProtocolType SPXFTP

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

invalidHost :: ValidatedUserOperatorServers
invalidHost =
  ValidatedUserOperatorServers
    { operator = Just operatorSimpleXChat {operatorId = DBEntityId 1},
      smpServers = [validatedServer (Left "smp:abcd@smp8.simplex.im"), validatedServer (Right "smp://abcd@smp8.simplex.im")],
      xftpServers = []
    }
  where
    validatedServer srv =
      AVS SDBNew (presetServer @'PSMP True "smp://abcd@smp8.simplex.im") {server = ValidatedProtoServer srv}
