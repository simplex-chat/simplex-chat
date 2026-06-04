{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module ResolveNameTests (resolveNameTests) where

import Data.Functor.Identity (Identity (..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as M
import Simplex.Chat.Library.Commands (ResolveError (..), iterateResolvers)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Agent.Protocol (AgentErrorType (..))
import Simplex.Messaging.Client (ProxyClientError (..))
import Simplex.Messaging.Protocol (NameRecord (..), SMPServer, mkNameOwner, pattern SMPServer)
import qualified Simplex.Messaging.Protocol as SMP
import Test.Hspec

-- | iterateResolvers is the testable core of resolveOnUserServers: it walks
-- a list of candidate SMP servers, querying a resolver per server, applying
-- the AUTH-is-NotFound and PROHIBITED-is-skip rules from the plan.
resolveNameTests :: Spec
resolveNameTests = describe "iterateResolvers" $ do
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

-- | Wrap a resolver to record which servers it was called for.
recording :: IORef [SMPServer] -> (SMPServer -> IO (Either AgentErrorType NameRecord)) -> SMPServer -> IO (Either AgentErrorType NameRecord)
recording ref f srv = modifyIORef' ref (<> [srv]) >> f srv

stubProhibitedThenHit :: SMPServer -> IO (Either AgentErrorType NameRecord)
stubProhibitedThenHit srv = pure $ M.findWithDefault (Right sampleRecord) srv $ M.fromList [(srv1, Left prohibitedErr)]

stubAuthThenHit :: SMPServer -> IO (Either AgentErrorType NameRecord)
stubAuthThenHit srv = pure $ M.findWithDefault (Right sampleRecord) srv $ M.fromList [(srv1, Left authErr)]

srv1 :: SMPServer
srv1 = SMPServer "smp1.example" "5223" (C.KeyHash "\1\2\3\4")

srv2 :: SMPServer
srv2 = SMPServer "smp2.example" "5223" (C.KeyHash "\5\6\7\8")

sampleRecord :: NameRecord
sampleRecord =
  NameRecord
    { nrDisplayName = "alice",
      -- mkNameOwner enforces the 20-byte invariant; this string is intentionally 20 ASCII bytes.
      nrOwner = either error id $ mkNameOwner "owner-bytes-1234567x",
      nrChannelLinks = [],
      nrContactLinks = [],
      nrAdminAddress = Nothing,
      nrAdminEmail = Nothing,
      nrExpiry = 0,
      nrIsTest = True
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
