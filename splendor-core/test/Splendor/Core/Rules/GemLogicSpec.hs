module Splendor.Core.Rules.GemLogicSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec
import Splendor.Core.Types
import Splendor.Core.Rules.GemLogic
import Splendor.Core.TestHelpers

spec :: Spec
spec = do
  describe "GemLogic" $ do
    describe "effectiveCost" $ do
      it "no bonuses: cost unchanged" $ do
        let cost = mkCost [(Ruby, 3), (Diamond, 2)]
        effectiveCost cost Map.empty `shouldBe` cost

      it "partial bonuses reduce only matching colors" $ do
        let cost = mkCost [(Ruby, 3), (Diamond, 2)]
            bonuses = Map.fromList [(Ruby, 1)]
        effectiveCost cost bonuses `shouldBe` mkCost [(Ruby, 2), (Diamond, 2)]

      it "bonuses exceeding cost floor at 0 per color" $ do
        let cost = mkCost [(Ruby, 2)]
            bonuses = Map.fromList [(Ruby, 5)]
            result = effectiveCost cost bonuses
        -- Ruby: max 0 (2-5) = 0; key may remain with value 0
        gemCount result (GemToken Ruby) `shouldBe` 0
        totalGems result `shouldBe` 0

      it "gold in cost unaffected by bonuses" $ do
        let cost = mkGems [(GemToken Ruby, 2), (GoldToken, 1)]
            bonuses = Map.fromList [(Ruby, 1)]
            result = effectiveCost cost bonuses
        gemCount result GoldToken `shouldBe` 1
        gemCount result (GemToken Ruby) `shouldBe` 1

      it "zero-cost card stays zero" $ do
        effectiveCost emptyGems (Map.fromList [(Diamond, 3)]) `shouldBe` emptyGems

    describe "computePayment" $ do
      it "exact colored tokens: pays exactly, no gold" $ do
        let tokens = mkGems [(GemToken Ruby, 3), (GemToken Diamond, 2)]
            cost   = mkCost [(Ruby, 3), (Diamond, 2)]
        computePayment tokens cost `shouldBe` Just (mkGems [(GemToken Ruby, 3), (GemToken Diamond, 2)])

      it "shortfall on one color: gold covers difference" $ do
        let tokens = mkGems [(GemToken Ruby, 1), (GoldToken, 2)]
            cost   = mkCost [(Ruby, 3)]
        computePayment tokens cost `shouldBe` Just (mkGems [(GemToken Ruby, 1), (GoldToken, 2)])

      it "shortfall on multiple colors: gold covers total" $ do
        let tokens = mkGems [(GemToken Ruby, 1), (GemToken Diamond, 0), (GoldToken, 3)]
            cost   = mkCost [(Ruby, 2), (Diamond, 2)]
        -- Ruby shortfall: 1, Diamond shortfall: 2, total gold: 3
        computePayment tokens cost `shouldBe` Just (mkGems [(GemToken Ruby, 1), (GoldToken, 3)])

      it "insufficient colored + gold: returns Nothing" $ do
        let tokens = mkGems [(GemToken Ruby, 1), (GoldToken, 1)]
            cost   = mkCost [(Ruby, 4)]
        computePayment tokens cost `shouldBe` Nothing

      it "zero effective cost: returns Just emptyGems" $ do
        computePayment (mkGems [(GemToken Ruby, 5)]) emptyGems `shouldBe` Just emptyGems

      it "payment never exceeds what's needed per color" $ do
        let tokens = mkGems [(GemToken Ruby, 10)]
            cost   = mkCost [(Ruby, 2)]
        case computePayment tokens cost of
          Just payment -> gemCount payment (GemToken Ruby) `shouldBe` 2
          Nothing      -> expectationFailure "Expected Just payment"

    describe "gemsFromTake" $ do
      it "TakeDifferent [c1,c2,c3]: 1 each, total 3" $ do
        let gems = gemsFromTake (TakeDifferent [Ruby, Diamond, Emerald])
        gemCount gems (GemToken Ruby) `shouldBe` 1
        gemCount gems (GemToken Diamond) `shouldBe` 1
        gemCount gems (GemToken Emerald) `shouldBe` 1
        totalGems gems `shouldBe` 3

      it "TakeDifferent [c1]: 1 gem, total 1" $ do
        let gems = gemsFromTake (TakeDifferent [Onyx])
        gemCount gems (GemToken Onyx) `shouldBe` 1
        totalGems gems `shouldBe` 1

      it "TakeTwoSame c: 2 of that color" $ do
        let gems = gemsFromTake (TakeTwoSame Sapphire)
        gemCount gems (GemToken Sapphire) `shouldBe` 2
        totalGems gems `shouldBe` 2

    describe "bankGemsForPlayerCount" $ do
      it "2 players -> 4 gems per color" $
        bankGemsForPlayerCount 2 `shouldBe` 4

      it "3 players -> 5 gems per color" $
        bankGemsForPlayerCount 3 `shouldBe` 5

      it "4 players -> 7 gems per color" $
        bankGemsForPlayerCount 4 `shouldBe` 7
