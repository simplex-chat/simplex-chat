{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module ResolveNameTests (resolveNameTests) where

import Data.Functor.Identity (Identity (..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Simplex.Chat.Controller (ChatError (..), ChatErrorType (..))
import Simplex.Chat.Library.Commands (ResolveError (..), firstNameLink, iterateResolvers, resolveErrorToChatError)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Agent.Protocol (AgentErrorType (..), SimplexNameDomain (..), SimplexNameInfo (..), SimplexNameType (..), SimplexTLD (..))
import Simplex.Messaging.Client (ProxyClientError (..))
import Simplex.Messaging.Protocol (NameRecord (..), SMPServer, mkNameOwner, pattern SMPServer)
import qualified Simplex.Messaging.Protocol as SMP
import Test.Hspec

resolveNameTests :: Spec
resolveNameTests = do
  -- iterateResolvers is the testable core of resolveOnUserServers: it walks
  -- a list of candidate SMP servers, querying a resolver per server, applying
  -- the AUTH-is-NotFound and PROHIBITED-is-skip rules from the plan.
  describe "iterateResolvers" $ do
    it "returns the first server's NameRecord on hit" $ do
      let r = runIdentity $ iterateResolvers [srv1, srv2] $ \_ -> pure $ Right sampleRecord
      r `shouldBe` Right sampleRecord
    it "skips CMD PROHIBITED servers and uses the next one's success" $ do
      callsRef <- newIORef []
      r <- iterateResolvers [srv1, srv2] (recording callsRef stubProhibitedThenHit)
      r `shouldBe` Right sampleRecord
      -- both servers must be consulted, in order
      readIORef callsRef `shouldReturn` [srv1, srv2]
    it "treats AUTH as definitive NameNotRegistered and stops iteration" $ do
      callsRef <- newIORef []
      r <- iterateResolvers [srv1, srv2] (recording callsRef stubAuthThenHit)
      r `shouldBe` Left NameNotRegistered
      -- second server must NOT be consulted: AUTH is authoritative
      readIORef callsRef `shouldReturn` [srv1]
    it "returns ResolverUnavailable when every server is non-name-capable" $ do
      let r = runIdentity $ iterateResolvers [srv1, srv2] (\_ -> pure $ Left prohibitedErr)
      r `shouldBe` Left ResolverUnavailable
    it "returns ResolverTransport when only transport-style errors are seen" $ do
      let r = runIdentity $ iterateResolvers [srv1, srv2] (\_ -> pure $ Left timeoutErr)
      case r of
        Left (ResolverTransport _) -> pure ()
        other -> expectationFailure $ "expected ResolverTransport, got " <> show other
    it "returns ResolverUnavailable on an empty server list" $ do
      let r = runIdentity $ iterateResolvers [] (\_ -> pure $ Right sampleRecord)
      r `shouldBe` Left ResolverUnavailable
  -- resolveErrorToChatError is the pure mapping used by connectPlanName's
  -- resolveAndDispatch: it converts the resolver outcome (when not a hit) to
  -- the user-visible ChatError. The success path (Right NameRecord) is dispatched
  -- via dispatchResolvedRecord (Task 6); we only exercise the failure mapping here.
  describe "resolveErrorToChatError" $ do
    it "maps NameNotRegistered to CESimplexNameNotFound (local-miss UX)" $
      case resolveErrorToChatError aliceNi NameNotRegistered of
        ChatError (CESimplexNameNotFound ni) -> ni `shouldBe` aliceNi
        other -> expectationFailure $ "expected CESimplexNameNotFound, got " <> show other
    it "maps ResolverUnavailable to CESimplexNameResolverUnavailable" $
      case resolveErrorToChatError aliceNi ResolverUnavailable of
        ChatError (CESimplexNameResolverUnavailable ni) -> ni `shouldBe` aliceNi
        other -> expectationFailure $ "expected CESimplexNameResolverUnavailable, got " <> show other
    it "wraps ResolverTransport via chatErrorAgent so the UI reuses agent-error rendering" $
      case resolveErrorToChatError aliceNi (ResolverTransport timeoutErr) of
        ChatErrorAgent e _ _ -> e `shouldBe` timeoutErr
        other -> expectationFailure $ "expected ChatErrorAgent, got " <> show other
  -- firstNameLink is the pure link-picker used by dispatchResolvedRecord:
  -- it selects nrSimplexContact for NTContact, nrSimplexChannel for NTPublicGroup.
  -- A Nothing for the queried type collapses to CESimplexNameNotFound so the UX
  -- is identical to a local-store miss.
  describe "firstNameLink" $ do
    it "picks nrSimplexContact for NTContact" $
      case firstNameLink NTContact (Just channelLink) (Just contactLink) aliceNi of
        Right lnk -> lnk `shouldBe` contactLink
        Left e -> expectationFailure $ "expected Right, got " <> show e
    it "picks nrSimplexChannel for NTPublicGroup" $
      case firstNameLink NTPublicGroup (Just channelLink) (Just contactLink) groupNi of
        Right lnk -> lnk `shouldBe` channelLink
        Left e -> expectationFailure $ "expected Right, got " <> show e
    it "returns CESimplexNameNotFound when nrSimplexContact is Nothing for NTContact" $
      case firstNameLink NTContact (Just channelLink) Nothing aliceNi of
        Left (ChatError (CESimplexNameNotFound ni)) -> ni `shouldBe` aliceNi
        other -> expectationFailure $ "expected CESimplexNameNotFound, got " <> show other
    it "returns CESimplexNameNotFound when nrSimplexChannel is Nothing for NTPublicGroup" $
      case firstNameLink NTPublicGroup Nothing (Just contactLink) groupNi of
        Left (ChatError (CESimplexNameNotFound ni)) -> ni `shouldBe` groupNi
        other -> expectationFailure $ "expected CESimplexNameNotFound, got " <> show other
    -- NTContact ignores nrSimplexChannel even when nrSimplexContact is Nothing.
    -- The resolver-side semantics say each name advertises a per-type link;
    -- cross-type fallback would silently connect to the wrong target.
    it "does not fall back to nrSimplexChannel for NTContact" $
      case firstNameLink NTContact (Just channelLink) Nothing aliceNi of
        Left (ChatError (CESimplexNameNotFound _)) -> pure ()
        other -> expectationFailure $ "expected CESimplexNameNotFound, got " <> show other

-- | Wrap a resolver to record which servers it was called for.
recording :: IORef [SMPServer] -> (SMPServer -> IO (Either AgentErrorType NameRecord)) -> SMPServer -> IO (Either AgentErrorType NameRecord)
recording ref f srv = modifyIORef' ref (<> [srv]) >> f srv

stubProhibitedThenHit :: SMPServer -> IO (Either AgentErrorType NameRecord)
stubProhibitedThenHit srv = pure $ M.findWithDefault (Right sampleRecord) srv $ M.fromList [(srv1, Left prohibitedErr)]

stubAuthThenHit :: SMPServer -> IO (Either AgentErrorType NameRecord)
stubAuthThenHit srv = pure $ M.findWithDefault (Right sampleRecord) srv $ M.fromList [(srv1, Left authErr)]

aliceNi :: SimplexNameInfo
aliceNi = SimplexNameInfo NTContact (SimplexNameDomain TLDSimplex "alice" [])

groupNi :: SimplexNameInfo
groupNi = SimplexNameInfo NTPublicGroup (SimplexNameDomain TLDSimplex "team" [])

-- Synthetic links to exercise firstNameLink's selection logic. The strings are
-- deliberately not real AConnShortLink encodings — the link parser is exercised
-- separately by the A_LINK error path inside dispatchResolvedRecord and by
-- AConnShortLink's own round-trip tests in simplexmq.
channelLink, contactLink :: Text
channelLink = "simplex:/channel-team"
contactLink = "simplex:/contact-alice"

srv1 :: SMPServer
srv1 = SMPServer "smp1.example" "5223" (C.KeyHash "\1\2\3\4")

srv2 :: SMPServer
srv2 = SMPServer "smp2.example" "5223" (C.KeyHash "\5\6\7\8")

sampleRecord :: NameRecord
sampleRecord =
  NameRecord
    { nrName = "alice",
      nrNickname = Nothing,
      nrWebsite = Nothing,
      nrLocation = Nothing,
      nrSimplexContact = Nothing,
      nrSimplexChannel = Nothing,
      nrEth = Nothing,
      nrBtc = Nothing,
      nrXmr = Nothing,
      nrDot = Nothing,
      -- mkNameOwner enforces the 20-byte invariant; these strings are intentionally 20 ASCII bytes.
      nrOwner = either error id $ mkNameOwner "owner-bytes-1234567x",
      nrResolver = either error id $ mkNameOwner "resolver-bytes12345x"
    }

-- AUTH from a name-capable destination relay: surfaces as SMP host AUTH
-- (see Simplex.Messaging.Agent.Client.protocolClientError).
authErr :: AgentErrorType
authErr = SMP "smp1.example" SMP.AUTH

-- CMD PROHIBITED on the PFWD path: surfaces as PROXY ... (ProxyProtocolError ...).
prohibitedErr :: AgentErrorType
prohibitedErr = PROXY "proxy.example" "smp1.example" (ProxyProtocolError (SMP.CMD SMP.PROHIBITED))

-- A generic transport-style failure that should bubble up as ResolverTransport
-- when no name-capable server was reached.
timeoutErr :: AgentErrorType
timeoutErr = INTERNAL "simulated network timeout"
