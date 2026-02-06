module Splendor.AI.MCTS.ExpansionSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
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
          currentIdx = gsCurrentPlayer testGameState
      case actions of
        (a:_) -> do
          let node = applyMove (MoveAction a) testGameState
          -- After acting, it should be the other player's turn
          nodePlayerIdx node `shouldNotBe` currentIdx
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

    it "action triggering NeedNobleChoice produces pre-expanded node with MoveNoble children" $ do
      -- Craft a state where the current player qualifies for 2 nobles after any action.
      -- Give player 4 Diamond + 4 Ruby bonuses (via purchased cards), place two nobles
      -- requiring 3 Diamond and 3 Ruby respectively.
      let mkBonusCard :: T.Text -> GemColor -> Card
          mkBonusCard cid color = Card cid Tier1 emptyGems color 0
          diamondCards = [mkBonusCard (T.pack $ "d" ++ show i) Diamond | i <- [1..4 :: Int]]
          rubyCards    = [mkBonusCard (T.pack $ "r" ++ show i) Ruby    | i <- [1..4 :: Int]]
          noble1 = Noble "noble-d" (Map.fromList [(Diamond, 3)]) 3
          noble2 = Noble "noble-r" (Map.fromList [(Ruby, 3)]) 3
          ps = gsPlayers testGameState
      case ps of
        (p:rest) -> do
          let p' = p { playerPurchased = diamondCards ++ rubyCards }
              board = gsBoard testGameState
              board' = board { boardNobles = [noble1, noble2] }
              gs = testGameState { gsPlayers = p' : rest, gsBoard = board' }
              expanded = expandNode (newTree gs)
              -- Some children should be pre-expanded (NeedNobleChoice path)
              preExpandedChildren = filter
                (\c -> nodeExpanded (childNode c) && not (null (nodeChildren (childNode c))))
                (nodeChildren expanded)
          -- Must have at least one pre-expanded child
          preExpandedChildren `shouldSatisfy` (not . null)
          -- All pre-expanded children should only have MoveNoble grandchildren
          mapM_ (\c -> do
            let grandchildren = nodeChildren (childNode c)
            grandchildren `shouldSatisfy` (not . null)
            mapM_ (\gc -> case childMove gc of
              MoveNoble _ -> pure ()
              _ -> expectationFailure "pre-expanded child should only have MoveNoble children"
              ) grandchildren
            ) preExpandedChildren
        [] -> expectationFailure "need players"

    it "GameOver result produces terminal node" $ do
      -- Create a state that is finished
      let gs = testGameState { gsPhase = Finished (GameResult "p0" "Alice" 15) }
          node = newTree gs
      nodeTerminal node `shouldBe` True

    it "MoveNoble with pending noble choice produces valid node" $ do
      -- Craft a state that triggers NeedNobleChoice, then use the resulting
      -- game state and noble to test MoveNoble directly.
      let mkBonusCard :: T.Text -> GemColor -> Card
          mkBonusCard cid color = Card cid Tier1 emptyGems color 0
          diamondCards = [mkBonusCard (T.pack $ "d" ++ show i) Diamond | i <- [1..4 :: Int]]
          rubyCards    = [mkBonusCard (T.pack $ "r" ++ show i) Ruby    | i <- [1..4 :: Int]]
          noble1 = Noble "noble-d" (Map.fromList [(Diamond, 3)]) 3
          noble2 = Noble "noble-r" (Map.fromList [(Ruby, 3)]) 3
      case gsPlayers testGameState of
        (p:rest) -> do
          let p' = p { playerPurchased = diamondCards ++ rubyCards }
              board = gsBoard testGameState
              board' = board { boardNobles = [noble1, noble2] }
              gs = testGameState { gsPlayers = p' : rest, gsBoard = board' }
              -- Expand to find a child that was pre-expanded (NeedNobleChoice path)
              expanded = expandNode (newTree gs)
              preExpanded = filter
                (\c -> nodeExpanded (childNode c) && not (null (nodeChildren (childNode c))))
                (nodeChildren expanded)
          case preExpanded of
            (c:_) -> do
              -- The child's state is the post-action state awaiting noble choice.
              -- Its grandchildren are MoveNoble nodes — verify they're valid.
              let grandchildren = nodeChildren (childNode c)
              case grandchildren of
                (gc:_) -> do
                  nodeTerminal (childNode gc) `shouldBe` False
                  childMove gc `shouldSatisfy` (\m -> case m of MoveNoble _ -> True; _ -> False)
                [] -> expectationFailure "expected MoveNoble grandchildren"
            [] -> expectationFailure "expected pre-expanded children from NeedNobleChoice"
        [] -> expectationFailure "need players"

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
