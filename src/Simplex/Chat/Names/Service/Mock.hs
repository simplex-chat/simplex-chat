{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An in-memory stand-in for the names service, the relayer and the chain.
--
-- It is a mock in that nothing is persisted and no transaction is broadcast,
-- but it is deliberately *not* a stub: every relayed intent has its EIP-712
-- digest recomputed and its signer recovered from the signature, and is
-- rejected unless the recovered address is the current owner and the nonce and
-- deadline check out. That is the same rule @transferWithSig@ and
-- @setTextWithSig@ enforce on-chain, so a client that satisfies this mock is
-- producing signatures the contracts would accept.
--
-- It also models the rc2 funding design: registration consumes a __registrar
-- credit__ granted by the beneficiary rather than paying a fee, and relayed
-- record edits consume a per-name __edit credit__ granted at registration. Both
-- are enforced here the way the contracts would enforce them, so running out of
-- either surfaces in development rather than in production.
--
-- Not modelled: gas, reorgs, expiry sweeps, the Dutch-auction premium, and
-- commit-reveal timing beyond a settable pending-poll count.
module Simplex.Chat.Names.Service.Mock
  ( MockChain,
    newMockChain,
    mockNamesService,
    mockDeployment,
    chainOwnerOf,
    chainRecords,
    setPendingRounds,
    setPaymentValidator,
    setRegistrarCredits,
    registrarCredits,
    editCreditsPerYear,
    linkSeparator,
  )
where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Simplex.Chat.Names.Service
import Simplex.Chat.Names.Snrc
import Simplex.Chat.Wallet (recoverSigner)
import Simplex.Messaging.Eth.Address (Address, mkAddress)
import Simplex.Messaging.Eth.Keccak (keccak256)

data NameEntry = NameEntry
  { neOwner :: Address,
    neRecords :: Map ByteString ByteString,
    neExpires :: Integer,
    neEditCredits :: Integer
  }

data Pending = Pending
  { pReq :: BuyRequest,
    pRoundsLeft :: Int
  }

data MockChain = MockChain
  { mcNames :: TVar (Map ByteString NameEntry),
    mcNonces :: TVar (Map Address Integer),
    mcPending :: TVar (Map ByteString Pending),
    mcSeq :: TVar Int,
    mcNow :: TVar Integer,
    mcPendingRounds :: TVar Int,
    mcValidatePayment :: TVar (PaymentProof -> Either ByteString ()),
    -- | Registration credits held by the relayer, granted by the beneficiary.
    mcRegistrarCredits :: TVar Integer
  }

-- | Relayed record edits granted per year of registration. AB's figure: ten a
-- year is plenty, and it is what bounds the relayer's gas exposure per name.
editCreditsPerYear :: Integer
editCreditsPerYear = 10

-- | Multiple links in one text record are separated by @;@ — this follows
-- @scripts/resolver/snrc-resolve.py@ (@LINK_SEPARATOR@), which is what the
-- deployed resolver actually parses. Note the namespace repo's CLAUDE.md
-- describes them as comma-separated; the code is authoritative and the doc
-- looks stale.
linkSeparator :: Char
linkSeparator = ';'

-- | A deployment description matching the mock. The contract addresses are
-- arbitrary but fixed — what matters is that client and mock agree, since the
-- EIP-712 domain binds them.
mockDeployment :: SnrcDeployment
mockDeployment =
  SnrcDeployment
    { sdTld = "simplex",
      sdChainId = 1,
      sdRegistrar = fixedAddr 0xB1,
      sdResolver = fixedAddr 0xB2
    }
  where
    fixedAddr b = either error id . mkAddress $ B.replicate 19 0 <> B.singleton b

newMockChain :: IO MockChain
newMockChain = do
  mcNames <- newTVarIO M.empty
  mcNonces <- newTVarIO M.empty
  mcPending <- newTVarIO M.empty
  mcSeq <- newTVarIO 0
  mcNow <- newTVarIO 1786000000
  mcPendingRounds <- newTVarIO 1
  mcValidatePayment <- newTVarIO (const $ Right ())
  mcRegistrarCredits <- newTVarIO 100
  pure MockChain {mcNames, mcNonces, mcPending, mcSeq, mcNow, mcPendingRounds, mcValidatePayment, mcRegistrarCredits}

-- | How many status polls a registration stays pending, standing in for the
-- 60-second commit-reveal wait. 0 makes registration immediate.
setPendingRounds :: MockChain -> Int -> IO ()
setPendingRounds c n = atomically $ writeTVar (mcPendingRounds c) n

-- | Swap in a validator that rejects some payments, to exercise the failure UI.
setPaymentValidator :: MockChain -> (PaymentProof -> Either ByteString ()) -> IO ()
setPaymentValidator c f = atomically $ writeTVar (mcValidatePayment c) f

-- | Stand-in for the beneficiary multisig calling @setRegistrarCredits@. Set it
-- to 0 to exercise the exhausted-credits path.
setRegistrarCredits :: MockChain -> Integer -> IO ()
setRegistrarCredits c n = atomically $ writeTVar (mcRegistrarCredits c) n

registrarCredits :: MockChain -> IO Integer
registrarCredits c = readTVarIO (mcRegistrarCredits c)

chainOwnerOf :: MockChain -> ByteString -> IO (Maybe Address)
chainOwnerOf c name = atomically $ fmap neOwner . M.lookup name <$> readTVar (mcNames c)

chainRecords :: MockChain -> ByteString -> IO (Map ByteString ByteString)
chainRecords c name = atomically $ maybe M.empty neRecords . M.lookup name <$> readTVar (mcNames c)

fqdn :: ByteString -> ByteString
fqdn label = label <> "." <> sdTld mockDeployment

-- | Matches SimplexController: 6+ characters, plus the [a-z0-9-] label grammar
-- the app already enforces client-side, plus the two reserved names.
validLabel :: ByteString -> Either ServiceError ()
validLabel l
  | B.length l < 6 = Left $ SENameInvalid "names must be at least 6 characters"
  | not (BC.all lowerAlnumHyphen l) = Left $ SENameInvalid "only lowercase letters, digits and hyphens"
  | "-" `B.isPrefixOf` l || "-" `B.isSuffixOf` l = Left $ SENameInvalid "cannot start or end with a hyphen"
  | l `elem` reserved = Left $ SENameInvalid "that name is reserved"
  | otherwise = Right ()
  where
    lowerAlnumHyphen ch = isDigit ch || (ch >= 'a' && ch <= 'z') || ch == '-'
    reserved = ["simplex", "simplex-chat"]

-- | The .simplex curve: $128 / $32 / $8 / $1 per year for 3 / 4 / 5 / 6+ chars.
priceCents :: ByteString -> Int -> Int
priceCents l years = years * perYear
  where
    perYear = case B.length l of
      3 -> 12800
      4 -> 3200
      5 -> 800
      _ -> 100

mockNamesService :: MockChain -> NamesService
mockNamesService c =
  NamesService
    { quoteName = quote,
      buyName = buy,
      registrationStatus = status,
      relayIntent = relay,
      resolveName = resolve,
      namesOwnedBy = ownedBy,
      currentNonce = nonceOf,
      editCreditsFor = editCredits
    }
  where
    quote label = case validLabel label of
      Left e -> pure $ Left e
      Right () -> do
        taken <- atomically $ M.member (fqdn label) <$> readTVar (mcNames c)
        pure . Right $
          NameQuote {nqLabel = label, nqAvailable = not taken, nqPriceCents = priceCents label 1, nqYears = 1}

    buy req@BuyRequest {brLabel, brPayment} = case validLabel brLabel of
      Left e -> pure $ Left e
      Right () -> do
        validate <- readTVarIO (mcValidatePayment c)
        case validate brPayment of
          Left e -> pure $ Left (SEPaymentRejected e)
          Right () -> atomically $ do
            names <- readTVar (mcNames c)
            credits <- readTVar (mcRegistrarCredits c)
            if M.member (fqdn brLabel) names
              then pure $ Left SENameTaken
              else if credits <= 0
              then pure $ Left SENoRegistrarCredits
              else do
                -- One credit per register call. No fee is transferred: the
                -- beneficiary granted the allowance up front instead.
                writeTVar (mcRegistrarCredits c) (credits - 1)
                n <- stateTVar (mcSeq c) $ \i -> (i + 1, i + 1)
                rounds <- readTVar (mcPendingRounds c)
                let pid = "purchase-" <> BC.pack (show n)
                modifyTVar' (mcPending c) $ M.insert pid Pending {pReq = req, pRoundsLeft = rounds}
                pure . Right $ PurchaseId pid

    status (PurchaseId pid) = atomically $ do
      pend <- readTVar (mcPending c)
      case M.lookup pid pend of
        Nothing -> pure $ Left SENotFound
        Just p
          | pRoundsLeft p > 0 -> do
              modifyTVar' (mcPending c) $ M.insert pid p {pRoundsLeft = pRoundsLeft p - 1}
              pure $ Right RegPending
          | otherwise -> do
              let BuyRequest {brLabel, brOwner, brYears, brContactLink, brChannelLink} = pReq p
                  name = fqdn brLabel
                  recs =
                    M.fromList $
                      [(contactRecordKey, l) | Just l <- [brContactLink]]
                        <> [(channelRecordKey, l) | Just l <- [brChannelLink]]
              now <- readTVar (mcNow c)
              let expires = now + fromIntegral brYears * 31536000
              -- Owner and records are written together, as
              -- SimplexController.register does via the resolver data[] array.
              modifyTVar' (mcNames c) $
                M.insert
                  name
                  NameEntry
                    { neOwner = brOwner,
                      neRecords = recs,
                      neExpires = expires,
                      neEditCredits = editCreditsPerYear * fromIntegral brYears
                    }
              modifyTVar' (mcPending c) $ M.delete pid
              pure . Right $ RegConfirmed {rsTxHash = txHash ("register:" <> name), rsExpires = expires}

    resolve name = atomically $ do
      names <- readTVar (mcNames c)
      pure $ case M.lookup name names of
        Nothing -> Left SENotFound
        Just NameEntry {neOwner, neRecords, neExpires, neEditCredits} ->
          Right
            NameRecordView
              { nrvName = name,
                nrvOwner = neOwner,
                nrvContact = maybe [] splitLinks $ M.lookup contactRecordKey neRecords,
                nrvChannel = maybe [] splitLinks $ M.lookup channelRecordKey neRecords,
                nrvExpires = neExpires,
                nrvEditCredits = neEditCredits
              }

    ownedBy owner = atomically $ do
      names <- readTVar (mcNames c)
      pure . Right . M.keys $ M.filter ((== owner) . neOwner) names

    nonceOf owner = Right . M.findWithDefault 0 owner <$> readTVarIO (mcNonces c)

    editCredits name = atomically $ do
      names <- readTVar (mcNames c)
      pure $ maybe (Left SENotFound) (Right . neEditCredits) (M.lookup name names)

    -- The heart of the mock: recompute the digest, recover the signer, and
    -- apply the same authorisation rules the contracts do.
    relay SignedIntent {siIntent, siSignature} = do
      now <- readTVarIO (mcNow c)
      case intentDigest mockDeployment siIntent of
        Left e -> pure . Left $ SEUnavailable (BC.pack e)
        Right digest -> case recoverSigner siSignature digest of
          Left _ -> pure $ Left SEBadSignature
          Right signer -> atomically $ do
            names <- readTVar (mcNames c)
            nonces <- readTVar (mcNonces c)
            let expected = M.findWithDefault 0 signer nonces
                bump = modifyTVar' (mcNonces c) $ M.insert signer (expected + 1)
            case siIntent of
              TransferName {tiFrom, tiTo, tiLabel, tiNonce, tiDeadline}
                | tiDeadline < now -> pure $ Left SEExpiredIntent
                | tiNonce /= expected -> pure $ Left SEBadNonce
                | otherwise ->
                    let name = fqdn tiLabel
                     in case M.lookup name names of
                          Nothing -> pure $ Left SENotFound
                          Just e
                            -- ownerOf reverts once expired, so transfer is refused
                            | neOwner e /= signer || tiFrom /= signer || neExpires e <= now -> pure $ Left SENotOwner
                            | otherwise -> do
                                modifyTVar' (mcNames c) $ M.insert name e {neOwner = tiTo}
                                bump
                                pure . Right $ txHash ("transfer:" <> name)
              SetTextRecord {sxName, sxKey, sxValue, sxNonce, sxDeadline}
                | sxDeadline < now -> pure $ Left SEExpiredIntent
                | sxNonce /= expected -> pure $ Left SEBadNonce
                | otherwise -> case M.lookup sxName names of
                    Nothing -> pure $ Left SENotFound
                    Just e
                      | neOwner e /= signer -> pure $ Left SENotOwner
                      -- Only the sponsored path is metered. An owner paying
                      -- their own gas is never charged a credit.
                      | neEditCredits e <= 0 -> pure $ Left SENoEditCredits
                      | otherwise -> do
                          modifyTVar' (mcNames c) $
                            M.insert
                              sxName
                              e
                                { neRecords = M.insert sxKey sxValue (neRecords e),
                                  neEditCredits = neEditCredits e - 1
                                }
                          bump
                          pure . Right $ txHash ("setText:" <> sxName <> ":" <> sxKey)

splitLinks :: ByteString -> [ByteString]
splitLinks = filter (not . B.null) . map trim . BC.split linkSeparator
  where
    trim = BC.dropWhile (== ' ') . BC.reverse . BC.dropWhile (== ' ') . BC.reverse

txHash :: ByteString -> ByteString
txHash = ("0x" <>) . BC.pack . concatMap byteHex . B.unpack . keccak256
  where
    byteHex w = [hexDigit (w `div` 16), hexDigit (w `mod` 16)]
    hexDigit n = "0123456789abcdef" !! fromIntegral n
