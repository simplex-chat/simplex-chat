{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RandomServers where

import Control.Monad (replicateM)
import qualified Data.List.NonEmpty as L
import Simplex.Chat (cfgServers, cfgServersToUse, defaultChatConfig, randomServers)
import Simplex.Chat.Controller (ChatConfig (..))
import Simplex.Messaging.Agent.Env.SQLite (ServerCfg (..))
import Simplex.Messaging.Protocol (ProtoServerWithAuth (..), SProtocolType (..), UserProtocol)
import Test.Hspec

randomServersTests :: Spec
randomServersTests = describe "choosig random servers" $ do
  it "should choose 4 random SMP servers and keep the rest disabled" testRandomSMPServers
  it "should keep all 6 XFTP servers" testRandomXFTPServers

deriving instance Eq (ServerCfg p)

testRandomSMPServers :: IO ()
testRandomSMPServers = do
  [srvs1, srvs2, srvs3] <-
    replicateM 3 $
      checkEnabled SPSMP 4 False =<< randomServers SPSMP defaultChatConfig
  (srvs1 == srvs2 && srvs2 == srvs3) `shouldBe` False -- && to avoid rare failures

testRandomXFTPServers :: IO ()
testRandomXFTPServers = do
  [srvs1, srvs2, srvs3] <-
    replicateM 3 $
      checkEnabled SPXFTP 6 True =<< randomServers SPXFTP defaultChatConfig
  (srvs1 == srvs2 && srvs2 == srvs3) `shouldBe` True

checkEnabled :: UserProtocol p => SProtocolType p -> Int -> Bool -> (L.NonEmpty (ServerCfg p), [ServerCfg p]) -> IO [ServerCfg p]
checkEnabled p n allUsed (srvs, _) = do
  let def = defaultServers defaultChatConfig
      cfgSrvs = L.sortWith server' $ cfgServers p def
      toUse = cfgServersToUse p def
  srvs == cfgSrvs `shouldBe` allUsed
  L.map enable srvs `shouldBe` L.map enable cfgSrvs
  let enbldSrvs = L.filter (\ServerCfg {enabled} -> enabled) srvs
  toUse `shouldBe` n
  length enbldSrvs `shouldBe` n
  pure enbldSrvs
  where
    server' ServerCfg {server = ProtoServerWithAuth srv _} = srv
    enable :: forall p. ServerCfg p -> ServerCfg p
    enable srv = (srv :: ServerCfg p) {enabled = False}
