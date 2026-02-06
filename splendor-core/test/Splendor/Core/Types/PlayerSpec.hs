module Splendor.Core.Types.PlayerSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec
import Splendor.Core.Types
import Splendor.Core.TestHelpers

spec :: Spec
spec = do
  describe "Player helpers" $ do
    describe "playerBonuses" $ do
      it "returns empty map for no purchased cards" $ do
        let p = emptyPlayer "p1"
        playerBonuses p `shouldBe` Map.empty

      it "counts bonuses per color correctly" $ do
        let cards = [ mkCard "c1" Tier1 [] Diamond 0
                    , mkCard "c2" Tier1 [] Diamond 0
                    , mkCard "c3" Tier1 [] Ruby 0
                    , mkCard "c4" Tier2 [] Emerald 0
                    ]
            p = playerWithCards "p1" cards
        playerBonuses p `shouldBe` Map.fromList [(Diamond, 2), (Ruby, 1), (Emerald, 1)]

      it "handles single card" $ do
        let p = playerWithCards "p1" [mkCard "c1" Tier1 [] Sapphire 0]
        playerBonuses p `shouldBe` Map.fromList [(Sapphire, 1)]

    describe "playerPrestige" $ do
      it "returns 0 for empty player" $ do
        playerPrestige (emptyPlayer "p1") `shouldBe` 0

      it "sums card prestige" $ do
        let cards = [ mkCard "c1" Tier1 [] Diamond 1
                    , mkCard "c2" Tier2 [] Ruby 3
                    ]
            p = playerWithCards "p1" cards
        playerPrestige p `shouldBe` 4

      it "sums cards and nobles" $ do
        let cards = [ mkCard "c1" Tier1 [] Diamond 2 ]
            noble = mkNoble "n1" [(Diamond, 3)]
            p = (playerWithCards "p1" cards) { playerNobles = [noble] }
        playerPrestige p `shouldBe` 5  -- 2 from card + 3 from noble

    describe "playerTokenCount" $ do
      it "returns 0 for empty player" $ do
        playerTokenCount (emptyPlayer "p1") `shouldBe` 0

      it "counts all token types" $ do
        let tokens = mkGems [(GemToken Ruby, 3), (GemToken Diamond, 2), (GoldToken, 1)]
            p = playerWithTokens "p1" tokens
        playerTokenCount p `shouldBe` 6

      it "handles single token type" $ do
        let p = playerWithTokens "p1" (singleGem (GemToken Onyx) 5)
        playerTokenCount p `shouldBe` 5
