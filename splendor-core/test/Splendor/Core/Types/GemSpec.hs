module Splendor.Core.Types.GemSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Map.Strict qualified as Map
import Splendor.Core.Types.Gem

spec :: Spec
spec = do
  describe "GemCollection" $ do
    describe "emptyGems" $ do
      it "has zero total" $ do
        totalGems emptyGems `shouldBe` 0

      it "returns 0 for any token type" $ do
        gemCount emptyGems (GemToken Diamond) `shouldBe` 0
        gemCount emptyGems GoldToken `shouldBe` 0

    describe "singleGem" $ do
      it "creates a collection with one type" $ do
        let g = singleGem (GemToken Ruby) 3
        gemCount g (GemToken Ruby) `shouldBe` 3
        totalGems g `shouldBe` 3

      it "singleGem 0 is empty" $ do
        singleGem (GemToken Diamond) 0 `shouldBe` emptyGems

    describe "addGems" $ do
      it "adds two collections" $ do
        let a = singleGem (GemToken Ruby) 2
            b = singleGem (GemToken Ruby) 3
        gemCount (addGems a b) (GemToken Ruby) `shouldBe` 5

      it "combines different colors" $ do
        let a = singleGem (GemToken Ruby) 2
            b = singleGem (GemToken Diamond) 3
            result = addGems a b
        gemCount result (GemToken Ruby) `shouldBe` 2
        gemCount result (GemToken Diamond) `shouldBe` 3
        totalGems result `shouldBe` 5

      it "identity with emptyGems" $ do
        let a = singleGem (GemToken Emerald) 4
        addGems a emptyGems `shouldBe` a
        addGems emptyGems a `shouldBe` a

    describe "subtractGems" $ do
      it "subtracts collections" $ do
        let a = singleGem (GemToken Ruby) 5
            b = singleGem (GemToken Ruby) 3
        gemCount (subtractGems a b) (GemToken Ruby) `shouldBe` 2

      it "can go negative" $ do
        let a = singleGem (GemToken Ruby) 2
            b = singleGem (GemToken Ruby) 5
        gemCount (subtractGems a b) (GemToken Ruby) `shouldBe` (-3)

    describe "hasEnoughGems" $ do
      it "returns True when enough" $ do
        let bank = addGems (singleGem (GemToken Ruby) 4) (singleGem (GemToken Diamond) 3)
            need = addGems (singleGem (GemToken Ruby) 2) (singleGem (GemToken Diamond) 1)
        hasEnoughGems bank need `shouldBe` True

      it "returns False when not enough" $ do
        let bank = singleGem (GemToken Ruby) 1
            need = singleGem (GemToken Ruby) 2
        hasEnoughGems bank need `shouldBe` False

      it "empty requirement is always satisfied" $ do
        hasEnoughGems emptyGems emptyGems `shouldBe` True

    describe "nonNegative" $ do
      it "True for empty" $ do
        nonNegative emptyGems `shouldBe` True

      it "True for positive" $ do
        nonNegative (singleGem (GemToken Ruby) 3) `shouldBe` True

      it "False for negative" $ do
        let neg = GemCollection (Map.singleton (GemToken Ruby) (-1))
        nonNegative neg `shouldBe` False

    describe "gemColors" $ do
      it "extracts only gem colors, not gold" $ do
        let g = addGems (singleGem (GemToken Ruby) 2) (singleGem GoldToken 1)
        gemColors g `shouldBe` Map.fromList [(Ruby, 2)]
