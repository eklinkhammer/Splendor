module Splendor.Core.Rules.NobleLogicSpec (spec) where

import Test.Hspec
import Splendor.Core.Types
import Splendor.Core.Rules.NobleLogic
import Splendor.Core.TestHelpers

spec :: Spec
spec = do
  describe "NobleLogic" $ do
    let noble1 = mkNoble "n1" [(Diamond, 3), (Ruby, 3)]
        noble2 = mkNoble "n2" [(Emerald, 4)]
        noble3 = mkNoble "n3" [(Diamond, 2), (Sapphire, 2)]

    describe "eligibleNobles" $ do
      it "meets one noble exactly" $ do
        let p = playerWithCards "p1"
              [ mkCard "c1" Tier1 [] Diamond 0
              , mkCard "c2" Tier1 [] Diamond 0
              , mkCard "c3" Tier1 [] Diamond 0
              , mkCard "c4" Tier1 [] Ruby 0
              , mkCard "c5" Tier1 [] Ruby 0
              , mkCard "c6" Tier1 [] Ruby 0
              ]
        eligibleNobles p [noble1, noble2] `shouldBe` [noble1]

      it "exceeds requirement: still eligible" $ do
        let p = playerWithCards "p1"
              [ mkCard "c1" Tier1 [] Diamond 0
              , mkCard "c2" Tier1 [] Diamond 0
              , mkCard "c3" Tier1 [] Diamond 0
              , mkCard "c4" Tier1 [] Diamond 0  -- 4 Diamond > 3 needed
              , mkCard "c5" Tier1 [] Ruby 0
              , mkCard "c6" Tier1 [] Ruby 0
              , mkCard "c7" Tier1 [] Ruby 0
              ]
        eligibleNobles p [noble1] `shouldBe` [noble1]

      it "missing one color: not eligible" $ do
        let p = playerWithCards "p1"
              [ mkCard "c1" Tier1 [] Diamond 0
              , mkCard "c2" Tier1 [] Diamond 0
              , mkCard "c3" Tier1 [] Diamond 0
              ]
        -- Needs 3 Diamond + 3 Ruby, has 3 Diamond + 0 Ruby
        eligibleNobles p [noble1] `shouldBe` []

      it "meets none: empty list" $ do
        let p = emptyPlayer "p1"
        eligibleNobles p [noble1, noble2, noble3] `shouldBe` []

      it "meets multiple: returns all" $ do
        let p = playerWithCards "p1"
              [ mkCard "c1" Tier1 [] Diamond 0
              , mkCard "c2" Tier1 [] Diamond 0
              , mkCard "c3" Tier1 [] Diamond 0
              , mkCard "c4" Tier1 [] Ruby 0
              , mkCard "c5" Tier1 [] Ruby 0
              , mkCard "c6" Tier1 [] Ruby 0
              , mkCard "c7" Tier1 [] Sapphire 0
              , mkCard "c8" Tier1 [] Sapphire 0
              ]
        -- Meets noble1 (3D, 3R) and noble3 (2D, 2S)
        eligibleNobles p [noble1, noble3] `shouldBe` [noble1, noble3]

      it "empty noble list: empty result" $ do
        let p = playerWithCards "p1" [mkCard "c1" Tier1 [] Diamond 0]
        eligibleNobles p [] `shouldBe` []

      it "zero bonuses: no nobles" $ do
        let p = emptyPlayer "p1"
        eligibleNobles p [noble1, noble2, noble3] `shouldBe` []
