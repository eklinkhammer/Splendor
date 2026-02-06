module Splendor.AI.MCTSSpec (spec) where

import Data.Map.Strict qualified as Map
import System.Random (mkStdGen)
import Test.Hspec
import Splendor.AI.Agent
import Splendor.AI.MCTS
import Splendor.AI.MCTS.Tree
import Splendor.AI.MCTS.Selection (select, ucb1)
import Splendor.AI.MCTS.Expansion (expandNode)
import Splendor.AI.MCTS.Backpropagation (backpropagate)
import Splendor.Core.Rules.ActionValidation (legalActions, legalGemReturns)
import Splendor.Core.Setup (initGameState)
import Splendor.Core.Types

-- | Create a real 2-player game state for testing.
testGameState :: GameState
testGameState = initGameState (mkStdGen 42) "test-game" 2 ["Alice", "Bob"]

spec :: Spec
spec = do
  describe "Tree" $ do
    describe "newTree" $ do
      it "creates unexpanded non-terminal node for InProgress game" $ do
        let node = newTree testGameState
        nodeExpanded node `shouldBe` False
        nodeTerminal node `shouldBe` False
        nodeVisits node `shouldBe` 0
        nodeWins node `shouldBe` 0.0
        nodeChildren node `shouldBe` []

      it "creates terminal node for Finished game" $ do
        let gs = testGameState { gsPhase = Finished (GameResult "player-1" "Alice" 15) }
            node = newTree gs
        nodeTerminal node `shouldBe` True

    describe "bestChild" $ do
      it "returns Nothing for leaf node" $ do
        bestChild (newTree testGameState) `shouldBe` Nothing

    describe "isLeaf" $ do
      it "returns True for fresh node" $
        isLeaf (newTree testGameState) `shouldBe` True

    describe "nodeAtPath" $ do
      it "returns root for empty path" $ do
        let node = newTree testGameState
        case nodeAtPath node [] of
          Just n  -> nodeState n `shouldBe` testGameState
          Nothing -> expectationFailure "expected Just"

      it "returns Nothing for invalid index" $ do
        let node = newTree testGameState
        nodeAtPath node [0] `shouldBe` Nothing

  describe "Selection" $ do
    describe "ucb1" $ do
      it "returns infinity for unvisited child" $
        ucb1 1.414 100 0 0 `shouldBe` (1.0 / 0.0)

      it "returns finite value for visited child" $ do
        let score = ucb1 1.414 100 10 5.0
        score `shouldSatisfy` (\s -> s > 0 && not (isInfinite s))

      it "exploitation component is wins/visits" $ do
        -- With explorationC = 0, should just be exploitation
        let score = ucb1 0.0 100 10 7.0
        score `shouldBe` 0.7

    describe "select" $ do
      it "returns empty path for unexpanded root" $ do
        let node = newTree testGameState
        select 1.414 node `shouldBe` []

      it "prefers unvisited children" $ do
        let expanded = expandNode (newTree testGameState)
            -- Give first child some visits
            cs = nodeChildren expanded
        case cs of
          (c:rest) -> do
            let visitedChild = c { childNode = (childNode c) { nodeVisits = 10, nodeWins = 5.0 } }
                node' = expanded { nodeChildren = visitedChild : rest, nodeVisits = 10 }
                path = select 1.414 node'
            -- Should select an unvisited child (index > 0)
            case path of
              [idx] -> idx `shouldSatisfy` (> 0)
              _     -> expectationFailure "expected single-step path to unvisited child"
          [] -> expectationFailure "expanded node should have children"

  describe "Expansion" $ do
    describe "expandNode" $ do
      it "generates children matching legalActions count" $ do
        let node = expandNode (newTree testGameState)
            expectedCount = length (legalActions testGameState)
        length (nodeChildren node) `shouldBe` expectedCount
        nodeExpanded node `shouldBe` True

      it "does not re-expand an already expanded node" $ do
        let node = expandNode (newTree testGameState)
            node' = expandNode node
        length (nodeChildren node') `shouldBe` length (nodeChildren node)

      it "generates gem return children for MustReturnGems phase" $ do
        -- Create a state that requires gem return
        let gs = testGameState { gsTurnPhase = MustReturnGems 1 }
            -- Give current player some gems to return
            players = gsPlayers gs
        case players of
          (p:rest) -> do
            let p' = p { playerTokens = GemCollection (Map.fromList [(GemToken Ruby, 6), (GemToken Diamond, 6)]) }
                gs' = gs { gsPlayers = p' : rest }
                node = expandNode (newTree gs')
                expectedCount = length (legalGemReturns gs')
            length (nodeChildren node) `shouldBe` expectedCount
          [] -> expectationFailure "need at least one player"

  describe "Backpropagation" $ do
    describe "backpropagate" $ do
      it "increments visit counts along path" $ do
        let node = expandNode (newTree testGameState)
            updated = backpropagate 1.0 0 [0] node
        nodeVisits updated `shouldBe` 1
        case nodeChildren updated of
          (c:_) -> nodeVisits (childNode c) `shouldBe` 1
          []    -> expectationFailure "expected children"

      it "adds wins for perspective player's nodes" $ do
        let node = expandNode (newTree testGameState)
            -- perspective=0, node has playerIdx=0, result=1.0
            updated = backpropagate 1.0 0 [] node
        nodeWins updated `shouldBe` 1.0

      it "adds (1-result) for opponent's nodes" $ do
        let node = expandNode (newTree testGameState)
            -- perspective=1 but node has playerIdx=0, result=1.0
            updated = backpropagate 1.0 1 [] node
        nodeWins updated `shouldBe` 0.0

  describe "runMCTS" $ do
    it "terminates with 10 iterations on real game state" $ do
      let config = defaultMCTSConfig { mctsIterations = 10, mctsTimeoutMs = 10000 }
      root <- runMCTS config testGameState 0
      nodeVisits root `shouldBe` 10

    it "produces expanded root with children" $ do
      let config = defaultMCTSConfig { mctsIterations = 10, mctsTimeoutMs = 10000 }
      root <- runMCTS config testGameState 0
      nodeExpanded root `shouldBe` True
      nodeChildren root `shouldSatisfy` (not . null)

  describe "MCTSAgent" $ do
    let agent = MCTSAgent { mctsConfig = defaultMCTSConfig { mctsIterations = 10, mctsTimeoutMs = 10000 } }

    describe "chooseAction" $ do
      it "returns a member of legal actions" $ do
        let actions = legalActions testGameState
        result <- chooseAction agent testGameState actions
        result `shouldSatisfy` (`elem` actions)

    describe "chooseGemReturn" $ do
      it "returns the single option from singleton list" $ do
        let opt = singleGem (GemToken Ruby) 1
        result <- chooseGemReturn agent testGameState [opt]
        result `shouldBe` opt

      it "returns a member of options" $ do
        -- Must use a game state in MustReturnGems phase for MCTS to produce MoveGemReturn children
        let gs = testGameState { gsTurnPhase = MustReturnGems 1 }
            players = gsPlayers gs
        case players of
          (p:rest) -> do
            let p' = p { playerTokens = GemCollection (Map.fromList [(GemToken Ruby, 6), (GemToken Diamond, 6)]) }
                gs' = gs { gsPlayers = p' : rest }
                opts = legalGemReturns gs'
            result <- chooseGemReturn agent gs' opts
            result `shouldSatisfy` (`elem` opts)
          [] -> expectationFailure "need players"

    describe "chooseNoble" $ do
      it "returns the single noble from singleton list" $ do
        let n = Noble "n1" (Map.fromList [(Diamond, 3)]) 3
        result <- chooseNoble agent testGameState [n]
        result `shouldBe` n

      it "returns a member of nobles list" $ do
        let n1 = Noble "n1" (Map.fromList [(Diamond, 3)]) 3
            n2 = Noble "n2" (Map.fromList [(Ruby, 3)]) 3
        result <- chooseNoble agent testGameState [n1, n2]
        result `shouldSatisfy` (`elem` [n1, n2])
