{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module ResolveNameTests (resolveNameTests) where

import Data.Functor.Identity (Identity (..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Simplex.Chat.Controller (ChatError (..), ChatErrorType (..))
import Simplex.Chat.Library.Commands (ResolveError (..), firstNameLink, iterateResolvers, linksMatch, resolveErrorToChatError)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Agent.Protocol (AConnectionLink (..), AgentErrorType (..), ConnShortLink, ConnectionLink (..), ConnectionMode (..), SConnectionMode (..), SimplexNameDomain (..), SimplexNameInfo (..), SimplexNameType (..), SimplexTLD (..))
import Simplex.Messaging.Client (ProxyClientError (..))
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Protocol (BrokerErrorType (..), NameRecord (..), NetworkError (..), SMPServer, mkNameOwner, pattern SMPServer)
import qualified Simplex.Messaging.Protocol as SMP
import Test.Hspec

resolveNameTests :: Spec
resolveNameTests = do
  -- iterateResolvers is the testable core of resolveOnUserServers: it walks
  -- a list of candidate SMP servers, querying a resolver per server. Only
  -- transport-level failures (NETWORK / TIMEOUT / host-unreachable) fall through
  -- to the next server. Any "the server answered" outcome — hit, AUTH, CMD
  -- PROHIBITED, or any other definite error — stops iteration so the candidate
  -- name is not broadcast to every operator the user has configured.
  describe "iterateResolvers" $ do
    it "returns the first server's NameRecord on hit" $ do
      let r = runIdentity $ iterateResolvers [srv1, srv2] $ \_ -> pure $ Right sampleRecord
      r `shouldBe` Right sampleRecord
    it "stops on CMD PROHIBITED and returns ResolverUnavailable" $ do
      callsRef <- newIORef []
      r <- iterateResolvers [srv1, srv2] (recording callsRef (\_ -> pure $ Left prohibitedErr))
      r `shouldBe` Left ResolverUnavailable
      -- second server must NOT be consulted: PROHIBITED is a server response,
      -- iterating would broadcast the queried name to every operator.
      readIORef callsRef `shouldReturn` [srv1]
    it "treats AUTH as definitive NameNotRegistered and stops iteration" $ do
      callsRef <- newIORef []
      r <- iterateResolvers [srv1, srv2] (recording callsRef stubAuthThenHit)
      r `shouldBe` Left NameNotRegistered
      -- second server must NOT be consulted: AUTH is authoritative
      readIORef callsRef `shouldReturn` [srv1]
    it "stops on a definite non-transport error and returns ResolverTransport" $ do
      callsRef <- newIORef []
      r <- iterateResolvers [srv1, srv2] (recording callsRef (\_ -> pure $ Left otherDefiniteErr))
      case r of
        Left (ResolverTransport e) -> e `shouldBe` otherDefiniteErr
        other -> expectationFailure $ "expected ResolverTransport, got " <> show other
      -- second server must NOT be consulted: definite error means the server
      -- answered, so iterating would leak the queried name.
      readIORef callsRef `shouldReturn` [srv1]
    it "iterates on transport-level errors and uses the next server's success" $ do
      callsRef <- newIORef []
      r <- iterateResolvers [srv1, srv2] (recording callsRef stubTransportThenHit)
      r `shouldBe` Right sampleRecord
      -- both servers must be consulted, in order: first server was unreachable.
      readIORef callsRef `shouldReturn` [srv1, srv2]
    it "returns ResolverUnavailable when every server is unreachable (all transport)" $ do
      let r = runIdentity $ iterateResolvers [srv1, srv2] (\_ -> pure $ Left networkErr)
      r `shouldBe` Left ResolverUnavailable
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
      case resolveErrorToChatError aliceNi (ResolverTransport otherDefiniteErr) of
        ChatErrorAgent e _ _ -> e `shouldBe` otherDefiniteErr
        other -> expectationFailure $ "expected ChatErrorAgent, got " <> show other
  -- firstNameLink is the pure link-picker used by dispatchResolvedRecord:
  -- it selects nrSimplexContact for NTContact, nrSimplexChannel for NTPublicGroup.
  -- The text fields use the empty string as the "absent" sentinel; an empty
  -- link for the queried type collapses to CESimplexNameNotFound so the UX is
  -- identical to a local-store miss.
  describe "firstNameLink" $ do
    it "NTContact path picks simplexContact" $
      case firstNameLink NTContact channelLink contactLink aliceNi of
        Right lnk -> lnk `shouldBe` contactLink
        Left e -> expectationFailure $ "expected Right, got " <> show e
    it "NTPublicGroup path picks simplexChannel" $
      case firstNameLink NTPublicGroup channelLink contactLink groupNi of
        Right lnk -> lnk `shouldBe` channelLink
        Left e -> expectationFailure $ "expected Right, got " <> show e
    it "empty Text returns NotFound" $
      case firstNameLink NTContact "" "" aliceNi of
        Left (ChatError (CESimplexNameNotFound ni)) -> ni `shouldBe` aliceNi
        other -> expectationFailure $ "expected CESimplexNameNotFound, got " <> show other
    -- Each name advertises a per-type link; cross-type fallback would silently
    -- connect to the wrong target, so a populated off-type slot must not satisfy
    -- the queried type.
    it "cross-type contact link with NTPublicGroup returns NotFound" $
      case firstNameLink NTPublicGroup "" contactLink groupNi of
        Left (ChatError (CESimplexNameNotFound ni)) -> ni `shouldBe` groupNi
        other -> expectationFailure $ "expected CESimplexNameNotFound, got " <> show other
    it "cross-type channel link with NTContact returns NotFound" $
      case firstNameLink NTContact channelLink "" aliceNi of
        Left (ChatError (CESimplexNameNotFound ni)) -> ni `shouldBe` aliceNi
        other -> expectationFailure $ "expected CESimplexNameNotFound, got " <> show other
  -- linksMatch is the byte-equal-after-normalize comparator that gates
  -- APIVerifySimplexName. The agent's simplex:/ scheme and the server-hostname
  -- scheme encode the same link, so a successful verification must accept
  -- either side using either scheme. A malformed RSLV link (anything that
  -- doesn't parse as a contact link) is rejected.
  describe "linksMatch" $ do
    let storedShort = CLShort sampleShortLinkServer
    it "matches an RSLV link in server scheme against a stored short-link" $
      linksMatch sampleShortLinkServerText storedShort `shouldBe` True
    it "matches across scheme normalization (simplex:/ vs https://)" $
      linksMatch sampleShortLinkSimplexText storedShort `shouldBe` True
    it "rejects a non-contact-link RSLV payload" $
      linksMatch "not-a-link" storedShort `shouldBe` False
    it "rejects a structurally different short-link" $
      linksMatch differentShortLinkText storedShort `shouldBe` False
    it "matches an invitation-shaped link only if both sides parse as contact" $
      -- invitation-typed RSLV link is not CMContact and must be rejected even
      -- if the bytes look superficially similar.
      linksMatch invitationLikeText storedShort `shouldBe` False

-- | Known-good short contact link in server-hostname scheme. Mirrors the
-- canonical encoding from simplexmq's ConnectionRequestTests.hs:
-- @CSLContact SLSServer CCTContact srv (LinkKey ...)@.
sampleShortLinkServerText :: Text
sampleShortLinkServerText = "https://smp.simplex.im/a#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=jjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&p=5223&c=1234-w"

-- | The same link as 'sampleShortLinkServerText' but in the agent's
-- simplex:/ scheme. normalize must collapse these to byte-equal forms.
sampleShortLinkSimplexText :: Text
sampleShortLinkSimplexText = "simplex:/a#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=smp.simplex.im%2Cjjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&p=5223&c=1234-w"

-- | Structurally different short link (different LinkKey). Must NOT match.
differentShortLinkText :: Text
differentShortLinkText = "https://smp.simplex.im/a#YWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWE?h=jjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&p=5223&c=1234-w"

-- | An invitation-shaped link (path /i not /a). Even if the bytes happen to
-- parse as some AConnectionLink, the SCMContact projection must fail.
invitationLikeText :: Text
invitationLikeText = "https://smp.simplex.im/i#MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY?h=jjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&p=5223&c=1234-w"

-- | Parsed form of 'sampleShortLinkServerText' for use as the stored side
-- of the linksMatch comparison.
sampleShortLinkServer :: ConnShortLink 'CMContact
sampleShortLinkServer = case strDecode (T.encodeUtf8 sampleShortLinkServerText) of
  Right (ACL SCMContact (CLShort l)) -> l
  other -> error $ "ResolveNameTests fixture failed to parse: " <> show other

-- | Wrap a resolver to record which servers it was called for.
recording :: IORef [SMPServer] -> (SMPServer -> IO (Either AgentErrorType NameRecord)) -> SMPServer -> IO (Either AgentErrorType NameRecord)
recording ref f srv = modifyIORef' ref (<> [srv]) >> f srv

stubAuthThenHit :: SMPServer -> IO (Either AgentErrorType NameRecord)
stubAuthThenHit srv = pure $ M.findWithDefault (Right sampleRecord) srv $ M.fromList [(srv1, Left authErr)]

stubTransportThenHit :: SMPServer -> IO (Either AgentErrorType NameRecord)
stubTransportThenHit srv = pure $ M.findWithDefault (Right sampleRecord) srv $ M.fromList [(srv1, Left networkErr)]

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
      nrNickname = "",
      nrWebsite = "",
      nrLocation = "",
      nrSimplexContact = "",
      nrSimplexChannel = "",
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

-- BROKER NETWORK: the server is unreachable. This is the kind of failure
-- that should cause iteration to fall through to the next configured server,
-- because no information about the queried name has been disclosed.
networkErr :: AgentErrorType
networkErr = BROKER "smp1.example" (NETWORK (NEConnectError "simulated network failure"))

-- A definite, non-transport agent error: the server responded but in a way
-- that doesn't match NAME / AUTH / CMD PROHIBITED. Should surface to the user
-- as ResolverTransport without iterating, to avoid broadcasting the name.
otherDefiniteErr :: AgentErrorType
otherDefiniteErr = INTERNAL "simulated definite error"
