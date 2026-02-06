module Splendor.Core.Rules.WinConditionSpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import Splendor.Core.Types
import Splendor.Core.Rules.WinCondition
import Splendor.Core.TestHelpers

-- | Helper to create a player with specific prestige via purchased cards
--   prestige is spread: first card gets the prestige, rest get 0
playerWithPrestige :: PlayerId -> Int -> Int -> Player
playerWithPrestige pid prestige numCards =
  let cards = [ mkCard (pid <> "-c" <> T.pack (show i)) Tier1 [] Diamond
                (if i == 1 then prestige else 0)
              | i <- [1..numCards] ]
  in playerWithCards pid cards

spec :: Spec
spec = do
  describe "WinCondition" $ do
    describe "checkWinCondition" $ do
      it "no player at 15: False" $ do
        let p1 = playerWithPrestige "p1" 10 3
            p2 = playerWithPrestige "p2" 12 4
        checkWinCondition [p1, p2] `shouldBe` False

      it "one player at exactly 15: True" $ do
        let p1 = playerWithPrestige "p1" 15 5
            p2 = playerWithPrestige "p2" 10 3
        checkWinCondition [p1, p2] `shouldBe` True

      it "one player above 15: True" $ do
        let p1 = playerWithPrestige "p1" 18 6
            p2 = playerWithPrestige "p2" 10 3
        checkWinCondition [p1, p2] `shouldBe` True

      it "empty player list: False" $ do
        checkWinCondition [] `shouldBe` False

    describe "determineWinner" $ do
      it "no player at 15+: Nothing" $ do
        let p1 = playerWithPrestige "p1" 10 3
            p2 = playerWithPrestige "p2" 12 4
        determineWinner [p1, p2] `shouldBe` Nothing

      it "single player at 15: correct GameResult" $ do
        let p1 = playerWithPrestige "p1" 15 5
            p2 = playerWithPrestige "p2" 10 3
        case determineWinner [p1, p2] of
          Just result -> do
            winnerId result `shouldBe` "p1"
            finalPrestige result `shouldBe` 15
          Nothing -> expectationFailure "Expected a winner"

      it "tiebreak: higher prestige wins" $ do
        let p1 = playerWithPrestige "p1" 16 5
            p2 = playerWithPrestige "p2" 15 5
        case determineWinner [p1, p2] of
          Just result -> winnerId result `shouldBe` "p1"
          Nothing -> expectationFailure "Expected a winner"

      it "tiebreak: same prestige, fewer purchased cards wins" $ do
        let p1 = playerWithPrestige "p1" 15 5  -- 15 prestige, 5 cards
            p2 = playerWithPrestige "p2" 15 3  -- 15 prestige, 3 cards
        case determineWinner [p1, p2] of
          Just result -> winnerId result `shouldBe` "p2"
          Nothing -> expectationFailure "Expected a winner"

      it "true tie: same prestige, same card count, first in list wins" $ do
        let p1 = playerWithPrestige "p1" 15 4  -- 15 prestige, 4 cards
            p2 = playerWithPrestige "p2" 15 4  -- 15 prestige, 4 cards
        case determineWinner [p1, p2] of
          Just result -> winnerId result `shouldBe` "p1"
          Nothing -> expectationFailure "Expected a winner"

      it "true tie reversed order: first candidate in list wins" $ do
        let p1 = playerWithPrestige "p1" 15 4
            p2 = playerWithPrestige "p2" 15 4
        case determineWinner [p2, p1] of
          Just result -> winnerId result `shouldBe` "p2"
          Nothing -> expectationFailure "Expected a winner"

      it "result has correct fields" $ do
        let p1 = playerWithPrestige "p1" 17 4
        case determineWinner [p1] of
          Just result -> do
            winnerId result `shouldBe` "p1"
            winnerName result `shouldBe` "p1"
            finalPrestige result `shouldBe` 17
          Nothing -> expectationFailure "Expected a winner"
