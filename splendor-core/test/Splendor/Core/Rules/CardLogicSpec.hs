module Splendor.Core.Rules.CardLogicSpec (spec) where

import Test.Hspec
import Splendor.Core.Types
import Splendor.Core.Rules.CardLogic
import Splendor.Core.TestHelpers

spec :: Spec
spec = do
  describe "CardLogic" $ do
    let card1 = mkCard "c1" Tier1 [(Ruby, 2)] Diamond 0
        card2 = mkCard "c2" Tier1 [(Emerald, 3)] Sapphire 1
        card3 = mkCard "c3" Tier1 [(Onyx, 1)] Ruby 0
        card4 = mkCard "c4" Tier1 [(Diamond, 2)] Emerald 0

    describe "findCardInDisplay" $ do
      it "finds a card present in display" $ do
        let row = mkTierRow [] [card1, card2, card3]
        findCardInDisplay "c2" row `shouldBe` Just card2

      it "returns Nothing for absent card" $ do
        let row = mkTierRow [] [card1, card2]
        findCardInDisplay "c99" row `shouldBe` Nothing

      it "returns Nothing for empty display" $ do
        let row = mkTierRow [] []
        findCardInDisplay "c1" row `shouldBe` Nothing

    describe "findCardInReserve" $ do
      it "finds a card present in reserve" $ do
        let p = (emptyPlayer "p1") { playerReserved = [card1, card2] }
        findCardInReserve "c1" p `shouldBe` Just card1

      it "returns Nothing for absent card" $ do
        let p = (emptyPlayer "p1") { playerReserved = [card1] }
        findCardInReserve "c99" p `shouldBe` Nothing

    describe "removeCardFromDisplay" $ do
      it "removes card and returns updated row" $ do
        let row = mkTierRow [card4] [card1, card2, card3]
        case removeCardFromDisplay "c2" row of
          Just (removed, newRow) -> do
            cardId removed `shouldBe` "c2"
            length (tierDisplay newRow) `shouldBe` 2
            map cardId (tierDisplay newRow) `shouldBe` ["c1", "c3"]
          Nothing -> expectationFailure "Expected Just"

      it "returns Nothing for absent card" $ do
        let row = mkTierRow [] [card1, card2]
        removeCardFromDisplay "c99" row `shouldBe` Nothing

    describe "refillDisplay" $ do
      it "refills display to 4 from deck" $ do
        let row = mkTierRow [card3, card4] [card1, card2]
            result = refillDisplay row
        length (tierDisplay result) `shouldBe` 4
        length (tierDeck result) `shouldBe` 0

      it "4 in display: no change" $ do
        let row = mkTierRow [card4] [card1, card2, card3, card4]
        refillDisplay row `shouldBe` row

      it "deck has fewer cards than needed" $ do
        let row = mkTierRow [card3] [card1, card2]
            result = refillDisplay row
        length (tierDisplay result) `shouldBe` 3
        null (tierDeck result) `shouldBe` True

      it "both empty: no change" $ do
        let row = mkTierRow [] []
        refillDisplay row `shouldBe` row

    describe "canReserve" $ do
      it "0 reserved: True" $ do
        canReserve (emptyPlayer "p1") `shouldBe` True

      it "2 reserved: True" $ do
        let p = (emptyPlayer "p1") { playerReserved = [card1, card2] }
        canReserve p `shouldBe` True

      it "3 reserved: False" $ do
        let p = (emptyPlayer "p1") { playerReserved = [card1, card2, card3] }
        canReserve p `shouldBe` False
