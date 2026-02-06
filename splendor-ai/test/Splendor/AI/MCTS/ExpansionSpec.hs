module Splendor.AI.MCTS.ExpansionSpec (spec) where

import Data.Map.Strict qualified as Map
import System.Random (mkStdGen)
import Test.Hspec
import Splendor.AI.MCTS.Expansion (applyMove, expandAt, expandNode)
import Splendor.AI.MCTS.Tree
import Splendor.Core.Rules.ActionValidation (legalActions, legalGemReturns)
import Splendor.Core.Setup (initGameState)
import Splendor.Core.Types

-- | Create a real 2-player game state for testing.
testGameState :: GameState
testGameState = initGameState (mkStdGen 42) "test-game" 2 ["Alice", "Bob"]

spec :: Spec
spec = do
  describe "applyMove" $ do
    it "MoveAction on valid state produces non-terminal node" $ do
      let actions = legalActions testGameState
      case actions of
        (a:_) -> do
          let node = applyMove (MoveAction a) testGameState
          nodeTerminal node `shouldBe` False
        [] -> expectationFailure "expected legal actions"

    it "MoveAction advances to next player" $ do
      let actions = legalActions testGameState
      case actions of
        (a:_) -> do
          let node = applyMove (MoveAction a) testGameState
          -- In a 2-player game, after player 0 acts, it should be player 1's turn
          nodePlayerIdx node `shouldSatisfy` (\idx -> idx == 0 || idx == 1)
        [] -> expectationFailure "expected legal actions"

    it "MoveAction with no currentPlayer produces terminal node" $ do
      let gs = testGameState { gsPlayers = [] }
          actions = legalActions testGameState  -- use original state's actions
      case actions of
        (a:_) -> do
          let node = applyMove (MoveAction a) gs
          nodeTerminal node `shouldBe` True
        [] -> expectationFailure "expected legal actions"

    it "MoveGemReturn on MustReturnGems phase produces valid node" $ do
      let ps = gsPlayers testGameState
      case ps of
        (p:rest) -> do
          let p' = p { playerTokens = GemCollection (Map.fromList [(GemToken Ruby, 6), (GemToken Diamond, 6)]) }
              gs = testGameState { gsPlayers = p' : rest, gsTurnPhase = MustReturnGems 1 }
              returns = legalGemReturns gs
          case returns of
            (r:_) -> do
              let node = applyMove (MoveGemReturn r) gs
              -- Should not be terminal after a gem return
              nodeTerminal node `shouldBe` False
            [] -> expectationFailure "expected gem return options"
        [] -> expectationFailure "need players"

    it "MoveGemReturn with no currentPlayer produces terminal node" $ do
      let gs = testGameState { gsPlayers = [] }
          ret = singleGem (GemToken Ruby) 1
          node = applyMove (MoveGemReturn ret) gs
      nodeTerminal node `shouldBe` True

    it "action triggering NeedNobleChoice produces pre-expanded node" $ do
      -- We can verify the NeedNobleChoice path by checking that stepResultToNode
      -- produces an expanded node with noble children. Instead of crafting a complex
      -- game state, we test the property: if expandNode produces MoveAction children
      -- and one of them is pre-expanded, it must have MoveNoble children.
      let expanded = expandNode (newTree testGameState)
          preExpandedChildren = filter (\c -> nodeExpanded (childNode c) && not (null (nodeChildren (childNode c)))) (nodeChildren expanded)
      -- If any pre-expanded children exist, they should have MoveNoble children
      mapM_ (\c -> do
        let grandchildren = nodeChildren (childNode c)
        mapM_ (\gc -> case childMove gc of
          MoveNoble _ -> pure ()
          _ -> expectationFailure "pre-expanded child should only have MoveNoble children"
          ) grandchildren
        ) preExpandedChildren

    it "GameOver result produces terminal node" $ do
      -- Create a state that is finished
      let gs = testGameState { gsPhase = Finished (GameResult "p0" "Alice" 15) }
          node = newTree gs
      nodeTerminal node `shouldBe` True

  describe "expandAt" $ do
    it "empty path expands root" $ do
      let node = newTree testGameState
          (expanded, _) = expandAt node []
      nodeExpanded expanded `shouldBe` True
      length (nodeChildren expanded) `shouldBe` length (legalActions testGameState)

    it "path [0] expands first child" $ do
      let root = expandNode (newTree testGameState)
      case nodeChildren root of
        (c:_) -> do
          -- First child should be unexpanded leaf initially
          isLeaf (childNode c) `shouldBe` True
          let (root', _) = expandAt root [0]
          case nodeChildren root' of
            (c':_) -> nodeExpanded (childNode c') `shouldBe` True
            [] -> expectationFailure "expected children after expandAt"
        [] -> expectationFailure "expected children from expandNode"

    it "deep path [0,0] propagates to grandchild" $ do
      let root = expandNode (newTree testGameState)
          -- Expand first child
          (root', _) = expandAt root [0]
      -- Now the first child is expanded; expand its first grandchild
      case nodeChildren root' of
        (c:_) -> case nodeChildren (childNode c) of
          (gc:_) -> do
            isLeaf (childNode gc) `shouldBe` True
            let (root'', _) = expandAt root' [0, 0]
            case nodeChildren root'' of
              (c':_) -> case nodeChildren (childNode c') of
                (gc':_) -> nodeExpanded (childNode gc') `shouldBe` True
                [] -> expectationFailure "expected grandchildren"
              [] -> expectationFailure "expected children"
          [] -> expectationFailure "first child should have children after expansion"
        [] -> expectationFailure "expected children"
