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
        let vec = B.pack [0, 0, 1, 2]
        getRelation 0 vec `shouldBe` MRNew
        getRelation 1 vec `shouldBe` MRNew
        getRelation 2 vec `shouldBe` MRIntroduced
        getRelation 3 vec `shouldBe` MRConnected

      it "reads multiple relations 2" $ do
        let vec = B.pack [1, 1, 0, 0, 2, 2, 0, 0]
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 1 vec `shouldBe` MRIntroduced
        getRelation 4 vec `shouldBe` MRConnected
        getRelation 5 vec `shouldBe` MRConnected

      it "ignore reserved bits" $ do
        let vec = B.pack [0xF9] -- 11111001
        getRelation 0 vec `shouldBe` MRIntroduced

    describe "setRelation" $ do
      it "sets relation in empty vector (lazy expansion)" $ do
        let vec = setRelation 0 MRIntroduced B.empty
        getRelation 0 vec `shouldBe` MRIntroduced

      it "ignores negative index" $ do
        let vec = setRelation (-1) MRIntroduced B.empty
        vec `shouldBe` B.empty

      it "expands vector to required length" $ do
        let vec = setRelation 5 MRConnected B.empty
        B.length vec `shouldBe` 6
        getRelation 5 vec `shouldBe` MRConnected
        -- Other positions should be MRNew (0)
        getRelation 0 vec `shouldBe` MRNew
        getRelation 10 vec `shouldBe` MRNew
        B.length vec `shouldBe` 6

      it "updates existing relation without affecting others" $ do
        -- Start: [01][01][00][00]
        let vec1 = setRelation 0 MRIntroduced B.empty
        let vec2 = setRelation 1 MRIntroduced vec1
        -- Update: [01][10][00][00]
        let vec3 = setRelation 1 MRConnected vec2
        getRelation 0 vec3 `shouldBe` MRIntroduced
        getRelation 1 vec3 `shouldBe` MRConnected

      it "updates relation in specific byte of multi-byte vector" $ do
        let vec1 = setRelation 0 MRIntroduced B.empty
        let vec2 = setRelation 10 MRConnected vec1
        B.length vec2 `shouldBe` 11
        getRelation 0 vec2 `shouldBe` MRIntroduced
        getRelation 10 vec2 `shouldBe` MRConnected
        forM_ [1..9] $ \i -> getRelation i vec2 `shouldBe` MRNew

      it "handles setting relation at last position in byte" $ do
        let vec = setRelation 3 MRConnected B.empty
        getRelation 3 vec `shouldBe` MRConnected

      it "preserves vector when setting same value" $ do
        let vec1 = setRelation 0 MRIntroduced B.empty
        let vec2 = setRelation 0 MRIntroduced vec1
        vec2 `shouldBe` vec1
        getRelation 0 vec2 `shouldBe` MRIntroduced

      it "preserves reserved bits" $ do
        let v = B.pack [0xF8] -- 11111000
        getRelation 0 v `shouldBe` MRNew
        let v' = setRelation 0 MRIntroduced v
        getRelation 0 v' `shouldBe` MRIntroduced
        B.unpack v' `shouldBe` [0xF9] -- 11111001

    describe "setRelations" $ do
      it "returns same vector for empty list" $ do
        let vec = B.pack [0x42]
        setRelations [] vec `shouldBe` vec

      it "sets multiple relations in empty vector" $ do
        let updates = [(0, MRIntroduced), (1, MRConnected), (2, MRIntroduced)]
        let vec = setRelations updates B.empty
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 1 vec `shouldBe` MRConnected
        getRelation 2 vec `shouldBe` MRIntroduced
        getRelation 3 vec `shouldBe` MRNew -- Unset position

      it "sets multiple relations 1" $ do
        let updates = [(0, MRIntroduced), (1, MRConnected), (2, MRConnected), (3, MRIntroduced)]
        let vec = setRelations updates B.empty
        B.length vec `shouldBe` 4
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 1 vec `shouldBe` MRConnected
        getRelation 2 vec `shouldBe` MRConnected
        getRelation 3 vec `shouldBe` MRIntroduced

      it "sets multiple relations 2" $ do
        let updates = [(0, MRIntroduced), (5, MRConnected), (10, MRIntroduced)]
        let vec = setRelations updates B.empty
        B.length vec `shouldBe` 11
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 5 vec `shouldBe` MRConnected
        getRelation 10 vec `shouldBe` MRIntroduced
        getRelation 7 vec `shouldBe` MRNew -- Unset position between

      it "handles sparse updates (few indices in large range)" $ do
        -- Sparse: 3 updates in large group
        let updates = [(0, MRIntroduced), (100, MRConnected), (5000, MRIntroduced)]
        let vec = setRelations updates B.empty
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 100 vec `shouldBe` MRConnected
        getRelation 5000 vec `shouldBe` MRIntroduced
        getRelation 50 vec `shouldBe` MRNew -- Untouched position

      it "handles dense updates (many consecutive indices)" $ do
        -- Dense: many consecutive updates
        let updates = [(i, if even i then MRIntroduced else MRConnected) | i <- [0 .. 99]]
        let vec = setRelations updates B.empty
        all (\i -> getRelation i vec == (if even i then MRIntroduced else MRConnected)) [0 .. 99] `shouldBe` True

      it "handles unsorted input correctly" $ do
        let updates = [(10, MRConnected), (2, MRIntroduced), (5, MRConnected), (0, MRIntroduced)]
        let vec = setRelations updates B.empty
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 2 vec `shouldBe` MRIntroduced
        getRelation 5 vec `shouldBe` MRConnected
        getRelation 10 vec `shouldBe` MRConnected

      it "handles duplicate indices (last one wins)" $ do
        let updates = [(0, MRIntroduced), (0, MRConnected), (0, MRIntroduced)]
        let vec = setRelations updates B.empty
        getRelation 0 vec `shouldBe` MRIntroduced

      it "preserves existing relations not in update list" $ do
        let vec1 = setRelation 0 MRConnected B.empty
        let vec2 = setRelation 5 MRIntroduced vec1
        let updates = [(10, MRConnected)]
        let vec3 = setRelations updates vec2
        getRelation 0 vec3 `shouldBe` MRConnected
        getRelation 5 vec3 `shouldBe` MRIntroduced
        getRelation 10 vec3 `shouldBe` MRConnected

    describe "edge cases and invariants" $ do
      it "round-trip: set then get returns same value" $ do
        let vec1 = setRelation 42 MRConnected B.empty
        getRelation 42 vec1 `shouldBe` MRConnected

      it "multiple round-trips preserve values" $ do
        let vec1 = setRelation 0 MRIntroduced B.empty
        let vec2 = setRelation 1 MRConnected vec1
        let vec3 = setRelation 2 MRIntroduced vec2
        getRelation 0 vec3 `shouldBe` MRIntroduced
        getRelation 1 vec3 `shouldBe` MRConnected
        getRelation 2 vec3 `shouldBe` MRIntroduced

      it "setRelations equivalent to multiple setRelation calls" $ do
        let updates = [(0, MRIntroduced), (5, MRConnected), (10, MRIntroduced)]
        let vecBatch = setRelations updates B.empty
        let vecSeq = setRelation 10 MRIntroduced $ setRelation 5 MRConnected $ setRelation 0 MRIntroduced B.empty
        vecBatch `shouldBe` vecSeq
        getRelation 0 vecBatch `shouldBe` getRelation 0 vecSeq
        getRelation 5 vecBatch `shouldBe` getRelation 5 vecSeq
        getRelation 10 vecBatch `shouldBe` getRelation 10 vecSeq

      it "handles large group size (10000 members)" $ do
        let updates = [(0, MRIntroduced), (5000, MRConnected), (9999, MRIntroduced)]
        let vec = setRelations updates B.empty
        B.length vec `shouldBe` 10000
        getRelation 0 vec `shouldBe` MRIntroduced
        getRelation 5000 vec `shouldBe` MRConnected
        getRelation 9999 vec `shouldBe` MRIntroduced

      it "all status values can be stored and retrieved" $ do
        let vec1 = setRelation 0 MRNew B.empty
        let vec2 = setRelation 1 MRIntroduced vec1
        let vec3 = setRelation 2 MRConnected vec2
        getRelation 0 vec3 `shouldBe` MRNew
        getRelation 1 vec3 `shouldBe` MRIntroduced
        getRelation 2 vec3 `shouldBe` MRConnected

      it "vector length is minimal (lazy expansion)" $ do
        let vec = setRelation 3 MRConnected B.empty
        B.length vec `shouldBe` 4
