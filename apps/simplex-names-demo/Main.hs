{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | End-to-end walkthrough of the SimpleX names UX against a mocked service.
--
-- The periphery is mocked — no store payment, no relayer, no chain — but the
-- crypto is real: every intent is signed with the profile's derived key, and the
-- mock recovers the signer from the signature and applies the same rules the
-- contracts do. A step that prints OK here is a step the contracts would accept.
--
-- Run: cabal run simplex-names-demo
module Main (main) where

import Control.Concurrent.STM
import Control.Monad (forM_, unless)
import Crypto.Random (drgNew)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Word (Word32)
import Simplex.Chat.Names.Service
import Simplex.Chat.Names.Service.Mock
import Simplex.Chat.Names.Snrc
import Simplex.Chat.Wallet
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import System.Exit (exitFailure)

main :: IO ()
main = do
  g <- newTVarIO =<< drgNew
  chain <- newMockChain
  let svc = mockNamesService chain
  setPendingRounds chain 2 -- stand in for the 60s commit-reveal wait

  section "1. First purchase creates the seed, lazily"
  entropy <- atomically $ newSeed B39.MS128 g
  let wallet = WalletSeed {wsId = SeedId 1, wsEntropy = entropy, wsBackedUp = False}
  phrase <- expect "recovery key" $ recoveryKeyPhrase wallet
  say $ "  seed created, id " <> bshow (wsId wallet)
  say $ "  recovery key: " <> phrase
  say "  (shown once after purchase; a reminder persists until acknowledged)"

  section "2. Each chat profile derives its own address from that one seed"
  alice <- expect "alice key" $ deriveAccount wallet 0
  bob <- expect "bob key" $ deriveAccount wallet 1
  say $ "  profile 0 (personal)  -> " <> bshow (accountAddress alice)
  say $ "  profile 1 (anonymous) -> " <> bshow (accountAddress bob)
  unless (accountAddress alice /= accountAddress bob) $ die "profiles must not share an address"
  say "  different addresses, so names under the two are unlinked on-chain"

  section "3. Quote a name"
  checkQuote svc "ab" "too short is rejected"
  checkQuote svc "simplex" "reserved name is rejected"
  q <- expectIO "quote" $ quoteName svc "alicechat"
  say $ "  alicechat.simplex available=" <> bshow (nqAvailable q) <> " price=" <> money (nqPriceCents q) <> "/yr"

  section "4. Pay and register (payment proof mocked, registration sponsored)"
  let contactLink = "https://smp16.simplex.im/a#alice-contact"
  pid <-
    expectIO "purchase" $
      buyName
        svc
        BuyRequest
          { brLabel = "alicechat",
            brOwner = accountAddress alice,
            brYears = 1,
            brPayment = PPAppleReceipt "mock-storekit-jws",
            brContactLink = Just contactLink,
            brChannelLink = Nothing
          }
  say $ "  purchase accepted: " <> bshow pid
  reg <- pollUntilConfirmed svc pid
  say $ "  registered, tx " <> BC.take 18 (rsTxHash reg) <> "..."

  section "5. The app confirms independently by resolving the name"
  rec1 <- expectIO "resolve" $ resolveName svc "alicechat.simplex"
  say $ "  owner   " <> bshow (nrvOwner rec1)
  say $ "  contact " <> bshow (nrvContact rec1)
  unless (nrvOwner rec1 == accountAddress alice) $ die "owner is not our derived address"
  say "  owner matches our key -> the name is ours; profile claim can now be set"

  section "6. Repoint the name after rotating the contact address"
  let newLink = "https://smp11.simplex.im/a#alice-rotated"
  n0 <- expectIO "nonce" $ currentNonce svc (accountAddress alice)
  txSet <-
    relaySigned svc alice $
      SetTextRecord
        { sxName = "alicechat.simplex",
          sxKey = contactRecordKey,
          sxValue = newLink,
          sxNonce = n0,
          sxDeadline = farFuture
        }
  say $ "  relayed setTextWithSig, tx " <> BC.take 18 txSet <> "..."
  rec2 <- expectIO "resolve" $ resolveName svc "alicechat.simplex"
  unless (nrvContact rec2 == [newLink]) $ die "record did not change"
  say $ "  contact now " <> bshow (nrvContact rec2)

  section "7. Forgery and replay are rejected"
  -- Bob signs an intent over Alice's name. Use *Bob's* nonce so the request is
  -- otherwise well-formed and the ownership check is what actually rejects it —
  -- signing with Alice's nonce would trip the nonce check first and prove nothing.
  nBob <- expectIO "nonce" $ currentNonce svc (accountAddress bob)
  let forged =
        SetTextRecord
          { sxName = "alicechat.simplex",
            sxKey = contactRecordKey,
            sxValue = "https://evil.example/a#hijack",
            sxNonce = nBob,
            sxDeadline = farFuture
          }
  relaySignedRaw svc bob forged >>= \case
    Left SENotOwner -> say "  forged by another key: rejected as not the owner"
    Left e -> die $ "expected SENotOwner, got: " <> serviceErrorText e
    Right _ -> die "a forged intent was accepted"
  -- Alice replaying her own already-used nonce.
  nAlice <- expectIO "nonce" $ currentNonce svc (accountAddress alice)
  relaySignedRaw svc alice forged {sxNonce = nAlice - 1} >>= \case
    Left SEBadNonce -> say "  replayed nonce: rejected"
    Left e -> die $ "expected SEBadNonce, got: " <> serviceErrorText e
    Right _ -> die "a replayed nonce was accepted"
  -- An expired deadline.
  relaySignedRaw svc alice forged {sxNonce = nAlice, sxDeadline = 0} >>= \case
    Left SEExpiredIntent -> say "  expired deadline: rejected"
    Left e -> die $ "expected SEExpiredIntent, got: " <> serviceErrorText e
    Right _ -> die "an expired intent was accepted"

  section "8. Gift the name to a contact"
  say $ "  recipient address " <> bshow (accountAddress bob) <> " (how this is learned is the open decision in the plan)"
  n2 <- expectIO "nonce" $ currentNonce svc (accountAddress alice)
  txXfer <-
    relaySigned svc alice $
      TransferName
        { tiFrom = accountAddress alice,
          tiTo = accountAddress bob,
          tiLabel = "alicechat",
          tiNonce = n2,
          tiDeadline = farFuture
        }
  say $ "  relayed transferWithSig, tx " <> BC.take 18 txXfer <> "..."
  rec3 <- expectIO "resolve" $ resolveName svc "alicechat.simplex"
  unless (nrvOwner rec3 == accountAddress bob) $ die "transfer did not move ownership"
  say $ "  owner now " <> bshow (nrvOwner rec3)
  say $ "  records carried over: " <> bshow (nrvContact rec3)

  section "9. The previous owner can no longer touch it"
  n3 <- expectIO "nonce" $ currentNonce svc (accountAddress alice)
  afterGift <-
    relaySignedRaw svc alice $
      SetTextRecord
        { sxName = "alicechat.simplex",
          sxKey = contactRecordKey,
          sxValue = "https://smp16.simplex.im/a#taken-back",
          sxNonce = n3,
          sxDeadline = farFuture
        }
  case afterGift of
    Left SENotOwner -> say "  rejected: no longer the owner"
    Left e -> say $ "  rejected: " <> serviceErrorText e
    Right _ -> die "the old owner could still write records"

  section "10. Recovery: wipe the device, re-import the recovery key"
  imported <- expect "import" $ importRecoveryKey phrase
  let restored = WalletSeed {wsId = SeedId 1, wsEntropy = imported, wsBackedUp = True}
  forM_ [(0 :: Word32, "personal"), (1, "anonymous")] $ \(i, label) -> do
    pk <- expect "derive" $ deriveAccount restored i
    owned <- expectIO "ownedBy" $ namesOwnedBy svc (accountAddress pk)
    say $ "  profile " <> bshow i <> " (" <> label <> ") " <> bshow (accountAddress pk) <> " owns " <> bshow owned
  restoredAlice <- expect "derive" $ deriveAccount restored 0
  unless (accountAddress restoredAlice == accountAddress alice) $ die "recovery derived a different address"
  say "  same addresses recovered from the phrase alone"

  putStrLn ""
  putStrLn "ALL STEPS OK"

-- helpers

farFuture :: Integer
farFuture = 1786000000 + 3600

section :: ByteString -> IO ()
section t = putStrLn "" >> BC.putStrLn t

say :: ByteString -> IO ()
say = BC.putStrLn

bshow :: Show a => a -> ByteString
bshow = BC.pack . show

money :: Int -> ByteString
money cents = BC.pack $ "$" <> show (cents `div` 100) <> "." <> pad (cents `mod` 100)
  where
    pad n = let s = show n in if length s < 2 then '0' : s else s

die :: ByteString -> IO a
die msg = BC.putStrLn ("FAILED: " <> msg) >> exitFailure

expect :: ByteString -> Either String a -> IO a
expect what = either (\e -> die (what <> ": " <> BC.pack e)) pure

expectIO :: ByteString -> IO (Either ServiceError a) -> IO a
expectIO what act = act >>= either (\e -> die (what <> ": " <> serviceErrorText e)) pure

checkQuote :: NamesService -> ByteString -> ByteString -> IO ()
checkQuote svc label why =
  quoteName svc label >>= \case
    Left e -> say $ "  " <> why <> " (" <> serviceErrorText e <> ")"
    Right _ -> die $ "expected rejection: " <> label

pollUntilConfirmed :: NamesService -> PurchaseId -> IO RegistrationStatus
pollUntilConfirmed svc pid = go (10 :: Int)
  where
    go 0 = die "registration never confirmed"
    go n =
      expectIO "status" (registrationStatus svc pid) >>= \case
        RegPending -> say "  waiting for commit-reveal..." >> go (n - 1)
        RegFailed e -> die $ "registration failed: " <> e
        r@RegConfirmed {} -> pure r

-- | Sign an intent with a profile key and hand it to the relayer.
relaySigned :: NamesService -> WalletAccount -> Intent -> IO ByteString
relaySigned svc pk intent =
  relaySignedRaw svc pk intent >>= either (\e -> die ("relay: " <> serviceErrorText e)) pure

relaySignedRaw :: NamesService -> WalletAccount -> Intent -> IO (Either ServiceError ByteString)
relaySignedRaw svc pk intent = do
  digest <- expect "digest" $ intentDigest mockDeployment intent
  sig <- expect "sign" $ signDigest pk digest
  relayIntent svc SignedIntent {siIntent = intent, siSignature = sig}
