{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module RandomServers where

import Control.Monad (replicateM)
import Data.Foldable (foldMap')
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Monoid (Sum (..))
import Simplex.Chat (defaultChatConfig, randomPresetServers)
import Simplex.Chat.Controller (ChatConfig (..), PresetServers (..))
import Simplex.Chat.Operators
import Simplex.Messaging.Agent.Env.SQLite (ServerRoles (..))
import Simplex.Messaging.Protocol (ProtoServerWithAuth (..), SProtocolType (..), UserProtocol)
import Test.Hspec

randomServersTests :: Spec
randomServersTests = describe "choosig random servers" $ do
  it "should choose 4 + 3 random SMP servers and keep the rest disabled" testRandomSMPServers
  it "should choose 3 + 3 random XFTP servers and keep the rest disabled" testRandomXFTPServers

deriving instance Eq ServerRoles

deriving instance Eq (DBEntityId' s)

deriving instance Eq (UserServer' s p)

testRandomSMPServers :: IO ()
testRandomSMPServers = do
  [srvs1, srvs2, srvs3] <-
    replicateM 3 $
      checkEnabled SPSMP 7 False =<< randomPresetServers SPSMP (presetServers defaultChatConfig)
  (srvs1 == srvs2 && srvs2 == srvs3) `shouldBe` False -- && to avoid rare failures

testRandomXFTPServers :: IO ()
testRandomXFTPServers = do
  [srvs1, srvs2, srvs3] <-
    replicateM 3 $
      checkEnabled SPXFTP 6 False =<< randomPresetServers SPXFTP (presetServers defaultChatConfig)
  (srvs1 == srvs2 && srvs2 == srvs3) `shouldBe` False -- && to avoid rare failures

checkEnabled :: UserProtocol p => SProtocolType p -> Int -> Bool -> NonEmpty (NewUserServer p) -> IO [NewUserServer p]
checkEnabled p n allUsed srvs = do
  let srvs' = sortOn server' $ L.toList srvs
      PresetServers {operators = presetOps} = presetServers defaultChatConfig
      presetSrvs = sortOn server' $ concatMap (operatorServers p) presetOps
      Sum toUse = foldMap' (Sum . operatorServersToUse p) presetOps
  srvs' == presetSrvs `shouldBe` allUsed
  map enable srvs' `shouldBe` map enable presetSrvs
  let enbldSrvs = filter (\UserServer {enabled} -> enabled) srvs'
  toUse `shouldBe` n
  length enbldSrvs `shouldBe` n
  pure enbldSrvs
  where
    server' UserServer {server = ProtoServerWithAuth srv _} = srv
    enable :: forall p. NewUserServer p -> NewUserServer p
    enable srv = (srv :: NewUserServer p) {enabled = False}
