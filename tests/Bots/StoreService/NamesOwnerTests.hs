{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesOwnerTests
  ( answerExpiry,
    answerFree,
    answerOwner,
    answerUnowned,
    byB,
    codeIn,
    committedPhase,
    dynamisOrder,
    failedPhase,
    labelBytes,
    ownerOfSelector,
    paidIn,
    rewrite,
    sentRegister,
    stored,
  )
where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (registrarOf, unhex)
import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.WebTests (digestFixture)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.IORef (readIORef)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64, Word8)
import Simplex.Chat.PaymentService.Types (InvoiceStatus (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash (..), labelHash)
import StoreService.Names.Abi (addressWord, uint256)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Store hiding (findLimit)
import Test.Hspec

seconds :: UTCTime -> Word64
seconds = floor . utcTimeToPOSIXSeconds

byB :: Order -> Order
byB o = o {oOwner = ownerBText, oKey = keyB}

dynamisOrder :: Order
dynamisOrder = aliceOrder {oLabel = "dynamis", oCommitment = commitmentOf 3, oSealed = "sealed-dynamis-3", oPrice = 20000}

-- viem's selectors of nameExpires(uint256), reservedNames(bytes32) and ownerOf(uint256), as in the calls of the contracts tests
nameExpiresSelector, reservedNamesSelector, ownerOfSelector :: ByteString
nameExpiresSelector = unhex "d6e4fa86"
reservedNamesSelector = unhex "389c0748"
ownerOfSelector = unhex "6352211e"

labelBytes :: Text -> ByteString
labelBytes label = lh
  where
    LabelHash lh = labelHash label

-- | ownerOf of a name nobody holds: the registrar's bare require, a revert without data.
answerUnowned :: Ctx -> Text -> IO ()
answerUnowned Ctx {cNode} label = answerCall cNode (registrarOf cNode) (ownerOfSelector <> labelBytes label) $ Reverts ""

answerOwner :: Ctx -> Text -> Address -> IO ()
answerOwner Ctx {cNode} label owner = answerCall cNode (registrarOf cNode) (ownerOfSelector <> labelBytes label) $ Returns $ addressWord owner

answerExpiry :: Ctx -> Text -> Word64 -> IO ()
answerExpiry Ctx {cNode} label expiry = answerCall cNode (registrarOf cNode) (nameExpiresSelector <> labelBytes label) $ Returns $ uint256 expiry

-- | A register transaction of the order as the sender saves it, then moved to this outcome.
sentRegister :: Ctx -> NameRegistration -> Word8 -> TxStatus -> IO TxHash
sentRegister Ctx {cStore} NameRegistration {nrId} n status = withTransaction cStore $ \db -> do
  let h = TxHash $ digestFixture n
  void $ insertTransaction db (TxRegister nrId) NewTransaction {ntxRaw = "raw", ntxHash = h, ntxNonce = fromIntegral n} orderTime
  recordTransactionOutcome db h TSPending status (Just 120) `shouldReturn` True
  pure h

-- | The chain answers the name as available and held by nobody.
answerFree :: Ctx -> Text -> IO ()
answerFree ctx label = answerReserved ctx label 0 >> answerUnowned ctx label

answerReserved :: Ctx -> Text -> Int -> IO ()
answerReserved Ctx {cNode} label reason = do
  answerCall cNode (registrarOf cNode) (nameExpiresSelector <> lh) $ Returns $ uint256 (0 :: Int)
  answerCall cNode fakeController (reservedNamesSelector <> lh) $ Returns $ uint256 reason
  where
    LabelHash lh = labelHash label

-- | A paid order, settled, then written as the worker would write it.
paidIn :: Ctx -> Order -> (NameRegistration -> NameRegistration) -> IO NameRegistration
paidIn ctx o change = do
  (invId, _) <- placed ctx o
  settleNow ctx invId settled `shouldReturn` Right ISPaid
  r <- invoiceRegistration ctx invId
  rewrite ctx r change

-- | A code order of a code of this length, then written as the worker would write it.
codeIn :: Ctx -> Int -> Order -> (NameRegistration -> NameRegistration) -> IO (NameRegistration, PrepaidCode)
codeIn ctx len o change = do
  (text, code) <- issueCode ctx len 2 id
  r <- registrationById ctx =<< registered ctx text o
  (,code) <$> rewrite ctx r change

-- | A write outside the turns, as the worker's, at the stored version.
rewrite :: Ctx -> NameRegistration -> (NameRegistration -> NameRegistration) -> IO NameRegistration
rewrite Ctx {cStore, cClock} NameRegistration {nrId} change = do
  t <- readIORef cClock
  withTransaction cStore $ \db -> do
    current <- fromJust <$> getRegistration db nrId
    updateRegistration db (change current) t >>= \case
      Written _ -> fromJust <$> getRegistration db nrId
      refused -> error $ "not written: " <> show refused

stored :: Ctx -> NameRegistration -> IO NameRegistration
stored ctx = registrationById ctx . nrId

failedPhase, committedPhase :: NameRegistration -> NameRegistration
failedPhase r = r {nrPhase = PFailed, nrFailedAt = Just orderTime, nrError = Just "The name could not be registered"}
committedPhase r =
  r
    { nrPhase = PCommitted,
      nrCommitTx = Just (TxHash $ digestFixture 8),
      nrCommitBlock = Just 100,
      nrRevealAfter = Just (seconds orderTime + 60),
      nrRevealBefore = Just (seconds orderTime + 86400)
    }