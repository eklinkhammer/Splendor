module Splendor.Core.Rules.ActionValidationSpec (spec) where

import Test.Hspec
import Splendor.Core.Types
import Splendor.Core.Rules.ActionValidation
import Splendor.Core.TestHelpers

-- | Helper to make a board with specific bank and display cards
mkTestBoard :: GemCollection -> [Card] -> Board
mkTestBoard bank displayCards = Board
  { boardTier1 = mkTierRow [] displayCards
  , boardTier2 = mkTierRow [] []
  , boardTier3 = mkTierRow [] []
  , boardNobles = []
  , boardBank = bank
  }

-- | Helper to make a board with deck cards too
mkTestBoardWithDeck :: GemCollection -> [Card] -> [Card] -> Board
mkTestBoardWithDeck bank displayCards deckCards = Board
  { boardTier1 = mkTierRow deckCards displayCards
  , boardTier2 = mkTierRow [] []
  , boardTier3 = mkTierRow [] []
  , boardNobles = []
  , boardBank = bank
  }

spec :: Spec
spec = do
  describe "ActionValidation" $ do
    let card1 = mkCard "c1" Tier1 [(Ruby, 2)] Diamond 0
        card2 = mkCard "c2" Tier1 [(Emerald, 3)] Sapphire 1
        card3 = mkCard "c3" Tier1 [(Onyx, 1)] Ruby 0
        card4 = mkCard "c4" Tier1 [(Diamond, 2)] Emerald 0
        freeCard = mkCard "free" Tier1 [] Diamond 0

    describe "validateAction - phase checks" $ do
      it "Finished phase: Left GameNotInProgress" $ do
        let result = GameResult "p1" "Alice" 15
            gs = (mkGameState [emptyPlayer "p1"] (mkTestBoard twoPlayerBank []))
                   { gsPhase = Finished result }
        validateAction gs "p1" (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
          `shouldBe` Left GameNotInProgress

      it "MustReturnGems phase: Left GemReturnRequired" $ do
        let gs = (mkGameState [emptyPlayer "p1"] (mkTestBoard twoPlayerBank []))
                   { gsTurnPhase = MustReturnGems 2 }
        validateAction gs "p1" (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
          `shouldBe` Left GemReturnRequired

      it "wrong player: Left NotYourTurn" $ do
        let gs = mkGameState [emptyPlayer "p1", emptyPlayer "p2"] (mkTestBoard twoPlayerBank [card1])
        validateAction gs "p2" (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
          `shouldBe` Left NotYourTurn

    describe "validateAction - TakeDifferent" $ do
      it "3 different colors available: Right" $ do
        let gs = mkGameState [emptyPlayer "p1"] (mkTestBoard twoPlayerBank [])
        validateAction gs "p1" (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
          `shouldSatisfy` isRight'

      it "duplicate colors: Left InvalidGemTake" $ do
        let gs = mkGameState [emptyPlayer "p1"] (mkTestBoard twoPlayerBank [])
        validateAction gs "p1" (TakeGems (TakeDifferent [Ruby, Ruby, Emerald]))
          `shouldSatisfy` isInvalidGemTake

      it "fewer than 3 when 3+ available: Left InvalidGemTake" $ do
        let gs = mkGameState [emptyPlayer "p1"] (mkTestBoard twoPlayerBank [])
        validateAction gs "p1" (TakeGems (TakeDifferent [Ruby, Diamond]))
          `shouldSatisfy` isInvalidGemTake

      it "fewer than 3 when only 2 available in bank: Right" $ do
        let bank = mkGems [(GemToken Ruby, 1), (GemToken Diamond, 1), (GoldToken, 5)]
            gs = mkGameState [emptyPlayer "p1"] (mkTestBoard bank [])
        validateAction gs "p1" (TakeGems (TakeDifferent [Ruby, Diamond]))
          `shouldSatisfy` isRight'

      it "color with 0 in bank: Left InvalidGemTake" $ do
        let bank = mkGems [(GemToken Ruby, 1), (GemToken Diamond, 1), (GoldToken, 5)]
            gs = mkGameState [emptyPlayer "p1"] (mkTestBoard bank [])
        -- Emerald has 0 in bank
        validateAction gs "p1" (TakeGems (TakeDifferent [Ruby, Emerald]))
          `shouldSatisfy` isInvalidGemTake

    describe "validateAction - TakeTwoSame" $ do
      it "color has >= 4: Right" $ do
        let gs = mkGameState [emptyPlayer "p1"] (mkTestBoard twoPlayerBank [])
        validateAction gs "p1" (TakeGems (TakeTwoSame Ruby)) `shouldSatisfy` isRight'

      it "color has 3: Left InvalidGemTake" $ do
        let bank = mkGems [(GemToken Ruby, 3), (GoldToken, 5)]
            gs = mkGameState [emptyPlayer "p1"] (mkTestBoard bank [])
        validateAction gs "p1" (TakeGems (TakeTwoSame Ruby)) `shouldSatisfy` isInvalidGemTake

    describe "validateAction - BuyCard" $ do
      it "valid purchase with exact payment: Right" $ do
        let tokens = mkGems [(GemToken Ruby, 3)]
            p = playerWithTokens "p1" tokens
            board = mkTestBoard twoPlayerBank [card1]  -- card1 costs Ruby 2
            gs = mkGameState [p] board
            payment = mkGems [(GemToken Ruby, 2)]
        validateAction gs "p1" (BuyCard (FromDisplay "c1") payment) `shouldSatisfy` isRight'

      it "card not found: Left CardNotFound" $ do
        let tokens = mkGems [(GemToken Ruby, 5)]
            p = playerWithTokens "p1" tokens
            board = mkTestBoard twoPlayerBank [card1]
            gs = mkGameState [p] board
        validateAction gs "p1" (BuyCard (FromDisplay "c99") emptyGems)
          `shouldBe` Left (CardNotFound "c99")

      it "player lacks tokens: Left InvalidPayment" $ do
        let p = emptyPlayer "p1"  -- no tokens
            board = mkTestBoard twoPlayerBank [card1]
            gs = mkGameState [p] board
            payment = mkGems [(GemToken Ruby, 2)]
        validateAction gs "p1" (BuyCard (FromDisplay "c1") payment)
          `shouldSatisfy` isInvalidPayment

      it "overpayment on a color: Left InvalidPayment" $ do
        let tokens = mkGems [(GemToken Ruby, 5)]
            p = playerWithTokens "p1" tokens
            board = mkTestBoard twoPlayerBank [card1]  -- costs Ruby 2
            gs = mkGameState [p] board
            payment = mkGems [(GemToken Ruby, 3)]  -- overpaying by 1
        validateAction gs "p1" (BuyCard (FromDisplay "c1") payment)
          `shouldSatisfy` isInvalidPayment

      it "gold != shortfall: Left CannotAfford" $ do
        let tokens = mkGems [(GemToken Ruby, 1), (GoldToken, 3)]
            p = playerWithTokens "p1" tokens
            board = mkTestBoard twoPlayerBank [card1]  -- costs Ruby 2
            gs = mkGameState [p] board
            -- Ruby shortfall = 1, but paying 2 gold
            payment = mkGems [(GemToken Ruby, 1), (GoldToken, 2)]
        validateAction gs "p1" (BuyCard (FromDisplay "c1") payment)
          `shouldBe` Left (CannotAfford "c1")

      it "zero-cost card with emptyGems payment: Right" $ do
        let p = emptyPlayer "p1"
            board = mkTestBoard twoPlayerBank [freeCard]
            gs = mkGameState [p] board
        validateAction gs "p1" (BuyCard (FromDisplay "free") emptyGems) `shouldSatisfy` isRight'

    describe "validateAction - ReserveCard" $ do
      it "under reserve limit: Right" $ do
        let p = emptyPlayer "p1"
            board = mkTestBoard twoPlayerBank [card1]
            gs = mkGameState [p] board
        validateAction gs "p1" (ReserveCard (FromDisplay "c1")) `shouldSatisfy` isRight'

      it "at reserve limit (3): Left ReserveLimit" $ do
        let p = (emptyPlayer "p1") { playerReserved = [card1, card2, card3] }
            board = mkTestBoard twoPlayerBank [card4]
            gs = mkGameState [p] board
        validateAction gs "p1" (ReserveCard (FromDisplay "c4"))
          `shouldBe` Left ReserveLimit

    describe "legalActions" $ do
      it "initial state produces actions" $ do
        let board = mkTestBoardWithDeck twoPlayerBank [card1, card2, card3, card4] []
            gs = mkGameState [emptyPlayer "p1", emptyPlayer "p2"] board
        length (legalActions gs) `shouldSatisfy` (> 0)

      it "MustReturnGems phase: empty list" $ do
        let gs = (mkGameState [emptyPlayer "p1"] (mkTestBoard twoPlayerBank []))
                   { gsTurnPhase = MustReturnGems 2 }
        legalActions gs `shouldBe` []

      it "no bank gems: no TakeGems actions" $ do
        let board = mkTestBoard emptyGems [card1, card2, card3, card4]
            gs = mkGameState [emptyPlayer "p1"] board
            actions = legalActions gs
        all (not . isTakeGemsAction) actions `shouldBe` True

      it "player at reserve limit: no ReserveCard actions" $ do
        let p = (emptyPlayer "p1") { playerReserved = [card1, card2, card3] }
            board = mkTestBoard twoPlayerBank [card4]
            gs = mkGameState [p] board
            actions = legalActions gs
        all (not . isReserveAction) actions `shouldBe` True

    describe "legalGemReturns" $ do
      it "not in MustReturnGems: empty list" $ do
        let gs = mkGameState [playerWithTokens "p1" (singleGem (GemToken Ruby) 5)] (mkTestBoard twoPlayerBank [])
        legalGemReturns gs `shouldBe` []

      it "MustReturnGems 1: correct number of options" $ do
        let tokens = mkGems [(GemToken Ruby, 3), (GemToken Diamond, 2)]
            p = playerWithTokens "p1" tokens
            gs = (mkGameState [p] (mkTestBoard twoPlayerBank []))
                   { gsTurnPhase = MustReturnGems 1 }
            returns = legalGemReturns gs
        -- Can return 1 Ruby or 1 Diamond = 2 options
        length returns `shouldBe` 2

      it "each returned collection sums to exactly n" $ do
        let tokens = mkGems [(GemToken Ruby, 2), (GemToken Diamond, 1), (GoldToken, 1)]
            p = playerWithTokens "p1" tokens
            gs = (mkGameState [p] (mkTestBoard twoPlayerBank []))
                   { gsTurnPhase = MustReturnGems 2 }
            returns = legalGemReturns gs
        all (\gc -> totalGems gc == 2) returns `shouldBe` True

-- Helpers for pattern matching on Either
isRight' :: Either a b -> Bool
isRight' (Right _) = True
isRight' _ = False

isInvalidGemTake :: Either ActionError a -> Bool
isInvalidGemTake (Left (InvalidGemTake _)) = True
isInvalidGemTake _ = False

isInvalidPayment :: Either ActionError a -> Bool
isInvalidPayment (Left (InvalidPayment _)) = True
isInvalidPayment _ = False

isTakeGemsAction :: Action -> Bool
isTakeGemsAction (TakeGems _) = True
isTakeGemsAction _ = False

isReserveAction :: Action -> Bool
isReserveAction (ReserveCard _) = True
isReserveAction _ = False
