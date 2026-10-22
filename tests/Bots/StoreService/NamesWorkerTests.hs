{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesWorkerTests (namesWorkerTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (registrarOf)
import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.NamesOwnerTests (answerExpiry, answerUnowned, failedPhase, labelBytes, ownerOfSelector, paidIn, stored)
import Control.Monad (replicateM_)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import StoreService.Names.Abi (addressWord)
import StoreService.Names.Orders (Landing (..), confirmLanded)
import StoreService.Names.Store (NamePhase (..), NameRegistration (..))
import Test.Hspec

namesWorkerTests :: Spec
namesWorkerTests = describe "StoreService names: worker" $ do
  describe "confirmation depth" $
    it "judges a register landed by ownerOf only once the owner holds the name at the confirmation depth" testLandedAtDepth

-- fixtures

orderSeconds :: Word64
orderSeconds = floor $ utcTimeToPOSIXSeconds orderTime

expiry :: Word64
expiry = orderSeconds + 2 * 31536000

-- tests

testLandedAtDepth :: IO ()
testLandedAtDepth = withOrders $ \ctx@Ctx {cNode, cOrders} -> do
  r <- paidIn ctx aliceOrder failedPhase
  answerUnowned ctx "alicebakery"
  answerExpiry ctx "alicebakery" expiry
  -- the owner's own register, mined in the latest block
  latest <- mine cNode []
  answerCallFrom cNode latest (registrarOf cNode) (ownerOfSelector <> labelBytes "alicebakery") $ Returns $ addressWord $ nrOwner r
  confirmLanded cOrders r `shouldReturn` LNotLanded
  stored ctx r `shouldReturn` r
  -- confirmations 3: two more blocks put it at the depth
  replicateM_ 2 $ mine cNode []
  confirmLanded cOrders r `shouldReturn` LRegistered (nrVersion r + 1)
  NameRegistration {nrPhase, nrExpiresAt, nrRegisterTx} <- stored ctx r
  (nrPhase, nrExpiresAt, nrRegisterTx) `shouldBe` (PRegistered, Just expiry, Nothing)
