module Splendor.AI.MCTS.SimulationSpec (spec) where

import Data.Map.Strict qualified as Map
import System.Random (mkStdGen)
import System.Timeout (timeout)
import Test.Hspec
import Splendor.AI.MCTS.Simulation (simulate, heuristicScore)
import Splendor.AI.MCTS.Tree (newTree, MCTSNode(..))
import Splendor.Core.Setup (initGameState)
import Splendor.Core.Types

-- | Create a real 2-player game state for testing.
testGameState :: GameState
testGameState = initGameState (mkStdGen 42) "test-game" 2 ["Alice", "Bob"]

-- | Create a card with the given prestige (for manipulating player prestige).
mkPrestigeCard :: Int -> Card
mkPrestigeCard p = Card
  { cardId       = "test-card"
  , cardTier     = Tier1
  , cardCost     = emptyGems
  , cardBonus    = Diamond
  , cardPrestige = p
  }

-- | Set player prestige by giving them a prestige card.
setPrestige :: Player -> Int -> Player
setPrestige p n = p { playerPurchased = [mkPrestigeCard n] }

spec :: Spec
spec = do
  describe "heuristicScore" $ do
    it "returns 0.5 for empty players" $ do
      let gs = testGameState { gsPlayers = [] }
      heuristicScore gs 0 `shouldBe` 0.5

    it "returns 0.5 when all players have zero prestige" $
      heuristicScore testGameState 0 `shouldBe` 0.5

    it "returns 1.0 when perspective player leads" $ do
      let ps = gsPlayers testGameState
      case ps of
        (p0:p1:rest) -> do
          let gs = testGameState { gsPlayers = setPrestige p0 10 : setPrestige p1 5 : rest }
          heuristicScore gs 0 `shouldBe` 1.0
        _ -> expectationFailure "need at least 2 players"

    it "returns correct partial ratio" $ do
      let ps = gsPlayers testGameState
      case ps of
        (p0:p1:rest) -> do
          let gs = testGameState { gsPlayers = setPrestige p0 6 : setPrestige p1 10 : rest }
          heuristicScore gs 0 `shouldBe` 0.6
        _ -> expectationFailure "need at least 2 players"

    it "returns 0.5 for out-of-bounds index" $
      heuristicScore testGameState 99 `shouldBe` 0.5

    it "returns 0.5 for negative index" $
      heuristicScore testGameState (-1) `shouldBe` 0.5

    it "returns 1.0 for single player with nonzero prestige" $ do
      case gsPlayers testGameState of
        (p:_) -> do
          let gs = testGameState { gsPlayers = [setPrestige p 5] }
          heuristicScore gs 0 `shouldBe` 1.0
        [] -> expectationFailure "need at least one player"

  describe "simulate" $ do
    it "returns 1.0 for finished game when perspective player wins" $ do
      let p0 = case gsPlayers testGameState of
                 (p:_) -> p
                 []    -> error "no players"
          gs = testGameState { gsPhase = Finished (GameResult (playerId p0) "Alice" 15) }
          node = (newTree gs) { nodeTerminal = True }
      result <- simulate node 0
      result `shouldBe` 1.0

    it "returns 0.0 for finished game when opponent wins" $ do
      let ps = gsPlayers testGameState
      case ps of
        (_:p1:_) -> do
          let gs = testGameState { gsPhase = Finished (GameResult (playerId p1) "Bob" 15) }
              node = (newTree gs) { nodeTerminal = True }
          result <- simulate node 0
          result `shouldBe` 0.0
        _ -> expectationFailure "need at least 2 players"

    it "returns value in [0,1] for active game" $ do
      let node = newTree testGameState
      result <- simulate node 0
      result `shouldSatisfy` (\v -> v >= 0 && v <= 1)

    it "returns deterministic result for terminal node" $ do
      let p0 = case gsPlayers testGameState of
                 (p:_) -> p
                 []    -> error "no players"
          gs = testGameState { gsPhase = Finished (GameResult (playerId p0) "Alice" 15) }
          node = (newTree gs) { nodeTerminal = True }
      r1 <- simulate node 0
      r2 <- simulate node 0
      r1 `shouldBe` r2

    it "returns value in [0,1] from MustReturnGems phase" $ do
      let ps = gsPlayers testGameState
      case ps of
        (p:rest) -> do
          let p' = p { playerTokens = GemCollection (Map.fromList [(GemToken Ruby, 6), (GemToken Diamond, 6)]) }
              gs = testGameState { gsPlayers = p' : rest, gsTurnPhase = MustReturnGems 1 }
              node = newTree gs
          result <- simulate node 0
          result `shouldSatisfy` (\v -> v >= 0 && v <= 1)
        [] -> expectationFailure "need players"

    it "completes within 5 seconds" $ do
      let node = newTree testGameState
      result <- timeout 5000000 (simulate node 0)
      result `shouldSatisfy` \case
        Just _ -> True
        Nothing -> False
