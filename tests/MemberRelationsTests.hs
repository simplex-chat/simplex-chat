{-# LANGUAGE OverloadedStrings #-}

module MemberRelationsTests where

import Control.Monad
import qualified Data.ByteString as B
import Simplex.Chat.Types.MemberRelations
import Test.Hspec

memberRelationsTests :: Spec
memberRelationsTests = do
  describe "MemberRelation vector operations" $ do
    describe "getRelation" $ do
      it "returns MRNew for empty vector" $ do
        getRelation 0 B.empty `shouldBe` MRNew
        getRelation 5 B.empty `shouldBe` MRNew
        getRelation 100 B.empty `shouldBe` MRNew

      it "returns MRNew for negative index" $ do
        getRelation (-1) B.empty `shouldBe` MRNew
        getRelation (-5) (B.pack [0xFF]) `shouldBe` MRNew

      it "returns MRNew for index beyond vector length" $ do
        let vec = B.pack [0x00]
        getRelation 10 vec `shouldBe` MRNew

      it "reads single relation from byte" $ do
        let vec = B.pack [0x01]
        getRelation 0 vec `shouldBe` MRIntroduced

      it "reads multiple relations" $ do
        let vec = B.pack [0, 0, 1, 2, 3, 4]
        getRelation 0 vec `shouldBe` MRNew
        getRelation 1 vec `shouldBe` MRNew
        getRelation 2 vec `shouldBe` MRIntroduced
        getRelation 3 vec `shouldBe` MRSubjectConnected
        getRelation 4 vec `shouldBe` MRReferencedConnected
        getRelation 5 vec `shouldBe` MRConnected

      it "reads multiple relations 2" $ do
        let vec = B.pack [1, 1, 0, 0, 2, 2, 0, 0]
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 1 vec `shouldBe` MRIntroduced
        getRelation 4 vec `shouldBe` MRSubjectConnected
        getRelation 5 vec `shouldBe` MRSubjectConnected

      it "ignore reserved bits" $ do
        let vec = B.pack [0xF1] -- reserved=1111, direction=0, status=001
        getRelation 0 vec `shouldBe` MRIntroduced

    describe "setRelation" $ do
      it "sets relation in empty vector (lazy expansion)" $ do
        let vec = setRelation 0 MRIntroduced B.empty
        getRelation 0 vec `shouldBe` MRIntroduced

      it "ignores negative index" $ do
        let vec = setRelation (-1) MRIntroduced B.empty
        vec `shouldBe` B.empty

      it "expands vector to required length" $ do
        let vec = setRelation 5 MRSubjectConnected B.empty
        B.length vec `shouldBe` 6
        getRelation 5 vec `shouldBe` MRSubjectConnected
        -- Other positions should be MRNew (0)
        getRelation 0 vec `shouldBe` MRNew
        getRelation 10 vec `shouldBe` MRNew
        B.length vec `shouldBe` 6

      it "updates existing relation without affecting others" $ do
        -- Start: [01][01][00][00]
        let vec1 = setRelation 0 MRIntroduced B.empty
        let vec2 = setRelation 1 MRIntroduced vec1
        -- Update: [01][10][00][00]
        let vec3 = setRelation 1 MRSubjectConnected vec2
        getRelation 0 vec3 `shouldBe` MRIntroduced
        getRelation 1 vec3 `shouldBe` MRSubjectConnected

      it "updates relation in specific byte of multi-byte vector" $ do
        let vec1 = setRelation 0 MRIntroduced B.empty
        let vec2 = setRelation 10 MRSubjectConnected vec1
        B.length vec2 `shouldBe` 11
        getRelation 0 vec2 `shouldBe` MRIntroduced
        getRelation 10 vec2 `shouldBe` MRSubjectConnected
        forM_ [1..9] $ \i -> getRelation i vec2 `shouldBe` MRNew

      it "handles setting relation at last position in byte" $ do
        let vec = setRelation 3 MRSubjectConnected B.empty
        getRelation 3 vec `shouldBe` MRSubjectConnected

      it "preserves vector when setting same value" $ do
        let vec1 = setRelation 0 MRIntroduced B.empty
        let vec2 = setRelation 0 MRIntroduced vec1
        vec2 `shouldBe` vec1
        getRelation 0 vec2 `shouldBe` MRIntroduced

      it "preserves reserved bits and direction" $ do
        let v = B.pack [0xF8] -- reserved=1111, direction=1, status=000
        getRelation 0 v `shouldBe` MRNew
        let v' = setRelation 0 MRConnected v
        getRelation 0 v' `shouldBe` MRConnected
        B.unpack v' `shouldBe` [0xFC] -- reserved=1111, direction=1, status=100

    describe "setNewRelation" $ do
      it "sets new relation with direction" $ do
        let vec = setNewRelation 0 IDReferencedIntroduced MRSubjectConnected B.empty
        getRelation' 0 vec `shouldBe` (IDReferencedIntroduced, MRSubjectConnected)
        B.unpack vec `shouldBe` [0x0A] -- direction=1, status=010

      it "preserves reserved bits" $ do
        let v = B.pack [0xF0] -- reserved=1111, direction=0, status=000
        let v' = setNewRelation 0 IDReferencedIntroduced MRConnected v
        getRelation 0 v' `shouldBe` MRConnected
        B.unpack v' `shouldBe` [0xFC] -- reserved=1111, direction=1, status=100

    describe "setRelations" $ do
      it "returns same vector for empty list" $ do
        let vec = B.pack [0x42]
        setRelations [] vec `shouldBe` vec

      it "sets multiple relations in empty vector" $ do
        let updates = [(0, MRIntroduced), (1, MRSubjectConnected), (2, MRReferencedConnected), (3, MRConnected)]
        let vec = setRelations updates B.empty
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 1 vec `shouldBe` MRSubjectConnected
        getRelation 2 vec `shouldBe` MRReferencedConnected
        getRelation 3 vec `shouldBe` MRConnected
        getRelation 4 vec `shouldBe` MRNew -- Unset position

      it "sets multiple relations 1" $ do
        let updates = [(0, MRIntroduced), (1, MRSubjectConnected), (2, MRSubjectConnected), (3, MRIntroduced)]
        let vec = setRelations updates B.empty
        B.length vec `shouldBe` 4
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 1 vec `shouldBe` MRSubjectConnected
        getRelation 2 vec `shouldBe` MRSubjectConnected
        getRelation 3 vec `shouldBe` MRIntroduced

      it "sets multiple relations 2" $ do
        let updates = [(0, MRIntroduced), (5, MRSubjectConnected), (10, MRIntroduced)]
        let vec = setRelations updates B.empty
        B.length vec `shouldBe` 11
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 5 vec `shouldBe` MRSubjectConnected
        getRelation 10 vec `shouldBe` MRIntroduced
        getRelation 7 vec `shouldBe` MRNew -- Unset position between

      it "handles sparse updates (few indices in large range)" $ do
        -- Sparse: 3 updates in large group
        let updates = [(0, MRIntroduced), (100, MRSubjectConnected), (5000, MRIntroduced)]
        let vec = setRelations updates B.empty
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 100 vec `shouldBe` MRSubjectConnected
        getRelation 5000 vec `shouldBe` MRIntroduced
        getRelation 50 vec `shouldBe` MRNew -- Untouched position

      it "handles dense updates (many consecutive indices)" $ do
        -- Dense: many consecutive updates
        let updates = [(i, if even i then MRIntroduced else MRSubjectConnected) | i <- [0 .. 99]]
        let vec = setRelations updates B.empty
        all (\i -> getRelation i vec == (if even i then MRIntroduced else MRSubjectConnected)) [0 .. 99] `shouldBe` True

      it "handles unsorted input correctly" $ do
        let updates = [(10, MRSubjectConnected), (2, MRIntroduced), (5, MRSubjectConnected), (0, MRIntroduced)]
        let vec = setRelations updates B.empty
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 2 vec `shouldBe` MRIntroduced
        getRelation 5 vec `shouldBe` MRSubjectConnected
        getRelation 10 vec `shouldBe` MRSubjectConnected

      it "handles duplicate indices (last one wins)" $ do
        let updates = [(0, MRIntroduced), (0, MRSubjectConnected), (0, MRIntroduced)]
        let vec = setRelations updates B.empty
        getRelation 0 vec `shouldBe` MRIntroduced

      it "preserves existing relations not in update list" $ do
        let vec1 = setRelation 0 MRSubjectConnected B.empty
        let vec2 = setRelation 5 MRIntroduced vec1
        let updates = [(10, MRSubjectConnected)]
        let vec3 = setRelations updates vec2
        getRelation 0 vec3 `shouldBe` MRSubjectConnected
        getRelation 5 vec3 `shouldBe` MRIntroduced
        getRelation 10 vec3 `shouldBe` MRSubjectConnected

    describe "setNewRelations" $ do
      it "sets multiple new relations with direction" $ do
        let updates = [(0, IDSubjectIntroduced, MRIntroduced), (1, IDReferencedIntroduced, MRSubjectConnected)]
        let vec = setNewRelations updates B.empty
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 1 vec `shouldBe` MRSubjectConnected
        B.unpack vec `shouldBe` [0x01, 0x0A] -- [dir=0,status=001], [dir=1,status=010]

    describe "edge cases and invariants" $ do
      it "round-trip: set then get returns same value" $ do
        let vec1 = setRelation 42 MRSubjectConnected B.empty
        getRelation 42 vec1 `shouldBe` MRSubjectConnected

      it "multiple round-trips preserve values" $ do
        let vec1 = setRelation 0 MRIntroduced B.empty
        let vec2 = setRelation 1 MRSubjectConnected vec1
        let vec3 = setRelation 2 MRReferencedConnected vec2
        let vec4 = setRelation 3 MRConnected vec3
        getRelation 0 vec4 `shouldBe` MRIntroduced
        getRelation 1 vec4 `shouldBe` MRSubjectConnected
        getRelation 2 vec4 `shouldBe` MRReferencedConnected
        getRelation 3 vec4 `shouldBe` MRConnected

      it "setRelations equivalent to multiple setRelation calls" $ do
        let updates = [(0, MRIntroduced), (5, MRSubjectConnected), (10, MRConnected)]
        let vecBatch = setRelations updates B.empty
        let vecSeq = setRelation 10 MRConnected $ setRelation 5 MRSubjectConnected $ setRelation 0 MRIntroduced B.empty
        vecBatch `shouldBe` vecSeq
        getRelation 0 vecBatch `shouldBe` getRelation 0 vecSeq
        getRelation 5 vecBatch `shouldBe` getRelation 5 vecSeq
        getRelation 10 vecBatch `shouldBe` getRelation 10 vecSeq

      it "handles large group size (10000 members)" $ do
        let updates = [(0, MRIntroduced), (5000, MRSubjectConnected), (9999, MRIntroduced)]
        let vec = setRelations updates B.empty
        B.length vec `shouldBe` 10000
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 5000 vec `shouldBe` MRSubjectConnected
        getRelation 9999 vec `shouldBe` MRIntroduced

      it "all status values can be stored and retrieved" $ do
        let vec1 = setRelation 0 MRNew B.empty
        let vec2 = setRelation 1 MRIntroduced vec1
        let vec3 = setRelation 2 MRSubjectConnected vec2
        let vec4 = setRelation 3 MRReferencedConnected vec3
        let vec5 = setRelation 4 MRConnected vec4
        getRelation 0 vec5 `shouldBe` MRNew
        getRelation 1 vec5 `shouldBe` MRIntroduced
        getRelation 2 vec5 `shouldBe` MRSubjectConnected
        getRelation 3 vec5 `shouldBe` MRReferencedConnected
        getRelation 4 vec5 `shouldBe` MRConnected

      it "vector length is minimal (lazy expansion)" $ do
        let vec = setRelation 3 MRSubjectConnected B.empty
        B.length vec `shouldBe` 4

      it "setRelation preserves existing direction" $ do
        let vec1 = setNewRelation 0 IDReferencedIntroduced MRIntroduced B.empty
        let vec2 = setRelation 0 MRConnected vec1
        getRelation 0 vec2 `shouldBe` MRConnected
        B.unpack vec2 `shouldBe` [0x0C] -- direction=1 preserved, status=100

    describe "setRelationConnected" $ do
      it "MRSubjectConnected on MRIntroduced -> MRSubjectConnected" $ do
        let vec1 = setRelation 0 MRIntroduced B.empty
        let vec2 = setRelationConnected 0 MRSubjectConnected vec1
        getRelation 0 vec2 `shouldBe` MRSubjectConnected

      it "MRReferencedConnected on MRIntroduced -> MRReferencedConnected" $ do
        let vec1 = setRelation 0 MRIntroduced B.empty
        let vec2 = setRelationConnected 0 MRReferencedConnected vec1
        getRelation 0 vec2 `shouldBe` MRReferencedConnected

      it "MRSubjectConnected on MRReferencedConnected -> MRConnected (complementary)" $ do
        let vec1 = setRelation 0 MRReferencedConnected B.empty
        let vec2 = setRelationConnected 0 MRSubjectConnected vec1
        getRelation 0 vec2 `shouldBe` MRConnected

      it "MRReferencedConnected on MRSubjectConnected -> MRConnected (complementary)" $ do
        let vec1 = setRelation 0 MRSubjectConnected B.empty
        let vec2 = setRelationConnected 0 MRReferencedConnected vec1
        getRelation 0 vec2 `shouldBe` MRConnected

      it "MRSubjectConnected on MRSubjectConnected -> no change" $ do
        let vec1 = setRelation 0 MRSubjectConnected B.empty
        let vec2 = setRelationConnected 0 MRSubjectConnected vec1
        vec2 `shouldBe` vec1

      it "MRReferencedConnected on MRReferencedConnected -> no change" $ do
        let vec1 = setRelation 0 MRReferencedConnected B.empty
        let vec2 = setRelationConnected 0 MRReferencedConnected vec1
        vec2 `shouldBe` vec1

      it "MRSubjectConnected on MRConnected -> no change" $ do
        let vec1 = setRelation 0 MRConnected B.empty
        let vec2 = setRelationConnected 0 MRSubjectConnected vec1
        vec2 `shouldBe` vec1

      it "MRReferencedConnected on MRConnected -> no change" $ do
        let vec1 = setRelation 0 MRConnected B.empty
        let vec2 = setRelationConnected 0 MRReferencedConnected vec1
        vec2 `shouldBe` vec1

      it "invalid status (MRConnected) -> no change" $ do
        let vec1 = setRelation 0 MRIntroduced B.empty
        let vec2 = setRelationConnected 0 MRConnected vec1
        vec2 `shouldBe` vec1

      it "invalid status (MRNew) -> no change" $ do
        let vec1 = setRelation 0 MRIntroduced B.empty
        let vec2 = setRelationConnected 0 MRNew vec1
        vec2 `shouldBe` vec1

      it "setRelationConnected preserves direction when updating" $ do
        let vec1 = setNewRelation 0 IDReferencedIntroduced MRIntroduced B.empty
        let vec2 = setRelationConnected 0 MRSubjectConnected vec1
        getRelation' 0 vec2 `shouldBe` (IDReferencedIntroduced, MRSubjectConnected)
