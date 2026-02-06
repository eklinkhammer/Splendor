module Splendor.Core.CardDataSpec (spec) where

import Data.List (nub)
import Data.Map.Strict qualified as Map
import Test.Hspec
import Splendor.Core.Types
import Splendor.Core.CardData

spec :: Spec
spec = do
  describe "CardData" $ do
    describe "tier1Cards" $ do
      it "has 40 cards" $
        length tier1Cards `shouldBe` 40

      it "all cards are Tier1" $
        all (\c -> cardTier c == Tier1) tier1Cards `shouldBe` True

    describe "tier2Cards" $ do
      it "has 30 cards" $
        length tier2Cards `shouldBe` 30

      it "all cards are Tier2" $
        all (\c -> cardTier c == Tier2) tier2Cards `shouldBe` True

    describe "tier3Cards" $ do
      it "has 20 cards" $
        length tier3Cards `shouldBe` 20

      it "all cards are Tier3" $
        all (\c -> cardTier c == Tier3) tier3Cards `shouldBe` True

    describe "allCards" $ do
      it "has 90 total cards" $
        length allCards `shouldBe` 90

      it "all card IDs are unique" $
        let ids = map cardId allCards
        in length (nub ids) `shouldBe` length ids

      it "all card costs are non-negative" $
        all (\c -> nonNegative (cardCost c)) allCards `shouldBe` True

    describe "allNobles" $ do
      it "has 10 nobles" $
        length allNobles `shouldBe` 10

      it "all nobles have prestige 3" $
        all (\n -> noblePrestige n == 3) allNobles `shouldBe` True

      it "all noble IDs are unique" $
        let ids = map nobleId allNobles
        in length (nub ids) `shouldBe` length ids

      it "all noble requirements are non-negative" $
        all (\n -> all (>= 0) (Map.elems (nobleRequirement n))) allNobles `shouldBe` True
