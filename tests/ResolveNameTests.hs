{-# LANGUAGE OverloadedStrings #-}

module ResolveNameTests (resolveNameTests) where

import Data.Text (Text)
import Simplex.Chat.Controller (ChatError (..), ChatErrorType (..))
import Simplex.Chat.Library.Commands (firstNameLink)
import Simplex.Messaging.Agent.Protocol (SimplexNameDomain (..), SimplexNameInfo (..), SimplexNameType (..), SimplexTLD (..))
import Test.Hspec

-- Name resolution/verification is owned by the agent (resolveSimplexName), and link comparison
-- uses the agent's sameConnShortLink / sameShortLinkContact. The only pure helper remaining in the
-- chat layer to cover here is firstNameLink (per-type link selection).
resolveNameTests :: Spec
resolveNameTests =
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
