{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module ResolveNameTests (resolveNameTests) where

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Simplex.Chat.Controller (ChatError (..), ChatErrorType (..))
import Simplex.Chat.Library.Commands (firstNameLink, linksMatch)
import Simplex.Messaging.Agent.Protocol (AConnectionLink (..), ConnShortLink, ConnectionLink (..), ConnectionMode (..), SConnectionMode (..), SimplexNameDomain (..), SimplexNameInfo (..), SimplexNameType (..), SimplexTLD (..))
import Simplex.Messaging.Encoding.String (strDecode)
import Test.Hspec

-- Name resolution and verification are owned by the agent (resolveSimplexName),
-- and failures flow through ChatErrorAgent — there is no chat-side iteration or
-- error-translation layer to test. These specs cover the two pure helpers that
-- remain in the chat layer: firstNameLink (link selection) and linksMatch
-- (verification comparison).
resolveNameTests :: Spec
resolveNameTests = do
  -- firstNameLink is the pure link-picker used by dispatchResolvedRecord:
  -- it selects nrSimplexContact for NTContact, nrSimplexChannel for NTPublicGroup.
  -- An empty link for the queried type collapses to CESimplexNameNotFound so the
  -- UX is identical to a local-store miss.
  describe "firstNameLink" $ do
    it "NTContact path picks simplexContact" $
      case firstNameLink NTContact [channelLink] [contactLink] aliceNi of
        Right lnk -> lnk `shouldBe` contactLink
        Left e -> expectationFailure $ "expected Right, got " <> show e
    it "NTPublicGroup path picks simplexChannel" $
      case firstNameLink NTPublicGroup [channelLink] [contactLink] groupNi of
        Right lnk -> lnk `shouldBe` channelLink
        Left e -> expectationFailure $ "expected Right, got " <> show e
    -- Each per-type field is a list of links (primary first); the first non-empty
    -- entry is used. firstNameLink filters out empty strings, so a list of only
    -- empty links collapses to NotFound — same UX as a local-store miss.
    it "picks the first non-empty link, skipping empty entries" $
      case firstNameLink NTContact [] ["", contactLink] aliceNi of
        Right lnk -> lnk `shouldBe` contactLink
        Left e -> expectationFailure $ "expected Right, got " <> show e
    it "empty link list returns NotFound" $
      case firstNameLink NTContact [] [] aliceNi of
        Left (ChatError (CESimplexNameNotFound ni)) -> ni `shouldBe` aliceNi
        other -> expectationFailure $ "expected CESimplexNameNotFound, got " <> show other
    -- Each name advertises a per-type link; cross-type fallback would silently
    -- connect to the wrong target, so a populated off-type slot must not satisfy
    -- the queried type.
    it "cross-type contact link with NTPublicGroup returns NotFound" $
      case firstNameLink NTPublicGroup [] [contactLink] groupNi of
        Left (ChatError (CESimplexNameNotFound ni)) -> ni `shouldBe` groupNi
        other -> expectationFailure $ "expected CESimplexNameNotFound, got " <> show other
    it "cross-type channel link with NTContact returns NotFound" $
      case firstNameLink NTContact [channelLink] [] aliceNi of
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
