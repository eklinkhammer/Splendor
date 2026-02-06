module Splendor.AI.RandomSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.List (nub)
import Test.Hspec
import Splendor.AI.Agent
import Splendor.AI.Random
import Splendor.Core.Types

spec :: Spec
spec = do
  describe "RandomAgent" $ do
    describe "agentName" $ do
      it "returns \"Random\"" $
        agentName RandomAgent `shouldBe` "Random"

    describe "chooseAction" $ do
      it "returns the single element from a singleton list" $ do
        let action = TakeGems (TakeDifferent [Ruby, Diamond, Emerald])
        result <- chooseAction RandomAgent dummyGS [action]
        result `shouldBe` action

      it "returns a member of a multi-element list" $ do
        let actions = [ TakeGems (TakeDifferent [Ruby, Diamond, Emerald])
                      , TakeGems (TakeTwoSame Ruby)
                      , TakeGems (TakeDifferent [Sapphire, Onyx])
                      ]
        result <- chooseAction RandomAgent dummyGS actions
        result `shouldSatisfy` (`elem` actions)

      it "all results over 100 trials are members of the list" $ do
        let actions = [ TakeGems (TakeDifferent [Ruby])
                      , TakeGems (TakeDifferent [Diamond])
                      , TakeGems (TakeDifferent [Emerald])
                      ]
        results <- mapM (\_ -> chooseAction RandomAgent dummyGS actions) [1..100 :: Int]
        all (`elem` actions) results `shouldBe` True

      it "produces more than one distinct value over 100 trials" $ do
        let actions = [ TakeGems (TakeDifferent [Ruby])
                      , TakeGems (TakeDifferent [Diamond])
                      , TakeGems (TakeDifferent [Emerald])
                      ]
        results <- mapM (\_ -> chooseAction RandomAgent dummyGS actions) [1..100 :: Int]
        length (nub results) `shouldSatisfy` (> 1)

    describe "chooseGemReturn" $ do
      it "returns a member of the options list" $ do
        let opts = [ singleGem (GemToken Ruby) 1
                   , singleGem (GemToken Diamond) 1
                   ]
        result <- chooseGemReturn RandomAgent dummyGS opts
        result `shouldSatisfy` (`elem` opts)

    describe "chooseNoble" $ do
      it "returns a member of the nobles list" $ do
        let n1 = Noble "n1" (Map.fromList [(Diamond, 3)]) 3
            n2 = Noble "n2" (Map.fromList [(Ruby, 3)]) 3
        result <- chooseNoble RandomAgent dummyGS [n1, n2]
        result `shouldSatisfy` (`elem` [n1, n2])

-- | Minimal game state for passing to agent methods (agents ignore it)
dummyGS :: GameState
dummyGS = GameState
  { gsGameId        = "dummy"
  , gsPlayers       = []
  , gsBoard         = Board
      { boardTier1  = TierRow [] []
      , boardTier2  = TierRow [] []
      , boardTier3  = TierRow [] []
      , boardNobles = []
      , boardBank   = emptyGems
      }
  , gsCurrentPlayer = 0
  , gsTurnNumber    = 1
  , gsPhase         = InProgress
  , gsTurnPhase     = AwaitingAction
  }
