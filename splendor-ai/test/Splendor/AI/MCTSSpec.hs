module Splendor.AI.MCTSSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Random (StdGen, mkStdGen)
import Test.Hspec
import Splendor.AI.Agent
import Splendor.AI.MCTS
import Splendor.AI.MCTS.Tree
import Splendor.AI.MCTS.Selection (select, ucb1)
import Splendor.AI.MCTS.Expansion (expandNode, expandAt)
import Splendor.AI.MCTS.Simulation (simulate)
import Splendor.AI.MCTS.Backpropagation (backpropagate)
import Splendor.AI.Random (RandomAgent(..))
import Splendor.Core.Engine (StepResult(..), applyAction, applyGemReturn, applyNobleChoice)
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

      it "creates non-terminal node for FinalRound phase" $ do
        let gs = testGameState { gsPhase = FinalRound }
            node = newTree gs
        nodeTerminal node `shouldBe` False
        nodeExpanded node `shouldBe` False

    describe "bestChild" $ do
      it "returns Nothing for leaf node" $ do
        bestChild (newTree testGameState) `shouldBe` Nothing

      it "selects child with most visits" $ do
        let expanded = expandNode (newTree testGameState)
            cs = nodeChildren expanded
        case cs of
          (c1:c2:rest) -> do
            let c1' = c1 { childNode = (childNode c1) { nodeVisits = 20, nodeWins = 10.0 } }
                c2' = c2 { childNode = (childNode c2) { nodeVisits = 10, nodeWins = 8.0 } }
                node = expanded { nodeChildren = c1' : c2' : rest }
            case bestChild node of
              Just best -> nodeVisits (childNode best) `shouldBe` 20
              Nothing -> expectationFailure "expected a best child"
          _ -> expectationFailure "expected at least 2 children"

      it "returns deterministic result with tied visits" $ do
        let expanded = expandNode (newTree testGameState)
            cs = nodeChildren expanded
        case cs of
          (c1:c2:rest) -> do
            let c1' = c1 { childNode = (childNode c1) { nodeVisits = 10, nodeWins = 5.0 } }
                c2' = c2 { childNode = (childNode c2) { nodeVisits = 10, nodeWins = 8.0 } }
                node = expanded { nodeChildren = c1' : c2' : rest }
            r1 <- pure $ bestChild node
            r2 <- pure $ bestChild node
            r1 `shouldBe` r2
          _ -> expectationFailure "expected at least 2 children"

    describe "isLeaf" $ do
      it "returns True for fresh node" $
        isLeaf (newTree testGameState) `shouldBe` True

      it "returns False for expanded node with children" $ do
        let node = expandNode (newTree testGameState)
        isLeaf node `shouldBe` False

    describe "nodeAtPath" $ do
      it "returns root for empty path" $ do
        let node = newTree testGameState
        case nodeAtPath node [] of
          Just n  -> nodeState n `shouldBe` testGameState
          Nothing -> expectationFailure "expected Just"

      it "returns Nothing for invalid index" $ do
        let node = newTree testGameState
        nodeAtPath node [0] `shouldBe` Nothing

      it "returns valid child at path [0] after expansion" $ do
        let node = expandNode (newTree testGameState)
        case nodeAtPath node [0] of
          Just child -> do
            -- The child should have a different game state (next player's turn)
            nodePlayerIdx child `shouldNotBe` nodePlayerIdx node
          Nothing -> expectationFailure "expected Just for valid path [0]"

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

      it "descends into visited expanded child to find unvisited grandchild" $ do
        let expanded = expandNode (newTree testGameState)
            cs = nodeChildren expanded
        case cs of
          (c:rest) -> do
            -- Expand the first child and give it high wins/visits ratio
            let expandedChild = expandNode (childNode c)
                visitedChild = c { childNode = expandedChild { nodeVisits = 50, nodeWins = 45.0 } }
                -- Give rest high visits but low win rate so UCB1 prefers first child
                rest' = map (\r -> r { childNode = (childNode r) { nodeVisits = 50, nodeWins = 5.0 } }) rest
                totalVisits = 50 + 50 * length rest
                node' = expanded { nodeChildren = visitedChild : rest', nodeVisits = totalVisits }
                path = select 1.414 node'
            -- Should descend into first child (index 0) and pick an unvisited grandchild
            length path `shouldBe` 2
            case path of
              (idx:_) -> idx `shouldBe` 0
              _ -> expectationFailure "expected path of length 2"
          [] -> expectationFailure "expanded node should have children"

      it "picks by UCB1 score when all children are visited" $ do
        let expanded = expandNode (newTree testGameState)
            -- Give all children visits, but one with much higher win rate
            cs = nodeChildren expanded
            totalVisits = 10 * length cs
            visitedCs = zipWith (\i c ->
              c { childNode = (childNode c)
                    { nodeVisits = 10
                    , nodeWins = if i == (0 :: Int) then 9.0 else 1.0
                    }
                }) [0..] cs
            node' = expanded { nodeChildren = visitedCs, nodeVisits = totalVisits }
            path = select 1.414 node'
        -- Should select child 0 (highest UCB1 due to 9/10 win rate)
        case path of
          [idx] -> idx `shouldBe` 0
          _     -> expectationFailure "expected single-step path"

  describe "MCTS integration" $ do
    it "select-expand-simulate-backpropagate cycle updates tree correctly" $ do
      let root = expandNode (newTree testGameState)
          path = select 1.414 root
          (expanded, leafState) = expandAt root path
      result <- simulate (newTree leafState) 0
      let updated = backpropagate result 0 path expanded
      nodeVisits updated `shouldBe` 1
      case nodeAtPath updated path of
        Just leaf -> do
          nodeVisits leaf `shouldBe` 1
          nodeWins leaf `shouldSatisfy` (\w -> w == 0.0 || w == 1.0)
        Nothing -> expectationFailure "expected node at path"

    it "expansion auto-creates noble choice children for NeedNobleChoice" $ do
      -- Craft a state where the current player qualifies for 2 nobles after any action.
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
              expanded = expandNode (newTree gs)
              preExpandedChildren = filter
                (\c -> nodeExpanded (childNode c) && not (null (nodeChildren (childNode c))))
                (nodeChildren expanded)
          -- Must have at least one pre-expanded child with MoveNoble grandchildren
          preExpandedChildren `shouldSatisfy` (not . null)
          mapM_ (\c -> do
            let grandchildren = nodeChildren (childNode c)
            grandchildren `shouldSatisfy` (not . null)
            mapM_ (\gc -> childMove gc `shouldSatisfy` isNobleMove') grandchildren
            ) preExpandedChildren
        [] -> expectationFailure "need players"

    it "terminal node stops selection and expansion, backprop records result" $ do
      let gs = testGameState { gsPhase = Finished (GameResult "player-1" "Alice" 15) }
          node = newTree gs
      select 1.414 node `shouldBe` []
      let (expanded, _) = expandAt node []
      nodeChildren expanded `shouldBe` []
      nodeExpanded expanded `shouldBe` True
      let updated = backpropagate 1.0 0 [] expanded
      nodeVisits updated `shouldBe` 1

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

      it "expandAt with path [0] expands the correct child" $ do
        let root = expandNode (newTree testGameState)
            (expanded, _) = expandAt root [0]
        case nodeChildren expanded of
          (c0:c1:_) -> do
            nodeExpanded (childNode c0) `shouldBe` True
            nodeChildren (childNode c0) `shouldSatisfy` (not . null)
            nodeExpanded (childNode c1) `shouldBe` False
          _ -> expectationFailure "expected at least 2 children"

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

      it "adds 0.0 for non-perspective player's nodes" $ do
        let node = expandNode (newTree testGameState)
            -- perspective=1 but node has playerIdx=0, result=1.0
            -- perspective-only attribution: non-perspective nodes get 0.0
            updated = backpropagate 1.0 1 [] node
        nodeWins updated `shouldBe` 0.0

      it "adds 0.0 for non-perspective nodes even with partial result" $ do
        let node = expandNode (newTree testGameState)
            -- perspective=1 but node has playerIdx=0, result=0.7 (heuristic)
            -- Old behavior: 1.0 - 0.7 = 0.3. New: 0.0 (perspective-only)
            updated = backpropagate 0.7 1 [] node
        nodeWins updated `shouldBe` 0.0

      it "increments visits but not wins for result=0.0 (loss)" $ do
        let node = expandNode (newTree testGameState)
            -- perspective=0, node has playerIdx=0, result=0.0 (loss)
            updated = backpropagate 0.0 0 [0] node
        nodeVisits updated `shouldBe` 1
        nodeWins updated `shouldBe` 0.0
        case nodeChildren updated of
          (c:_) -> do
            nodeVisits (childNode c) `shouldBe` 1
            nodeWins (childNode c) `shouldBe` 0.0
          [] -> expectationFailure "expected children"

      it "propagates through 3-level path, updating all nodes" $ do
        let root = expandNode (newTree testGameState)
        case nodeChildren root of
          (c:_) -> do
            let expandedChild = expandNode (childNode c)
                root' = root { nodeChildren = c { childNode = expandedChild } : drop 1 (nodeChildren root) }
            case nodeChildren expandedChild of
              (gc:_) -> do
                let expandedGC = expandNode (childNode gc)
                    ec' = expandedChild { nodeChildren = gc { childNode = expandedGC } : drop 1 (nodeChildren expandedChild) }
                    root'' = root' { nodeChildren = (c { childNode = ec' }) : drop 1 (nodeChildren root') }
                    updated = backpropagate 1.0 0 [0, 0, 0] root''
                -- All 3 levels plus root should have 1 visit
                nodeVisits updated `shouldBe` 1
                case nodeChildren updated of
                  (c':_) -> do
                    nodeVisits (childNode c') `shouldBe` 1
                    case nodeChildren (childNode c') of
                      (gc':_) -> do
                        nodeVisits (childNode gc') `shouldBe` 1
                        case nodeChildren (childNode gc') of
                          (ggc:_) -> nodeVisits (childNode ggc) `shouldBe` 1
                          [] -> expectationFailure "expected great-grandchildren"
                      [] -> expectationFailure "expected grandchildren"
                  [] -> expectationFailure "expected children"
              [] -> expectationFailure "expected grandchildren after expansion"
          [] -> expectationFailure "expanded root should have children"

      it "propagates visits along deeper path [0,0]" $ do
        let root = expandNode (newTree testGameState)
            -- Expand first child to get grandchildren
            cs = nodeChildren root
        case cs of
          (c:_) -> do
            let expandedChild = expandNode (childNode c)
                root' = root { nodeChildren = c { childNode = expandedChild } : drop 1 cs }
                updated = backpropagate 1.0 0 [0, 0] root'
            -- Root should have 1 visit
            nodeVisits updated `shouldBe` 1
            -- First child should have 1 visit
            case nodeChildren updated of
              (c':_) -> do
                nodeVisits (childNode c') `shouldBe` 1
                -- First grandchild should have 1 visit
                case nodeChildren (childNode c') of
                  (gc:_) -> nodeVisits (childNode gc) `shouldBe` 1
                  [] -> expectationFailure "expected grandchildren"
              [] -> expectationFailure "expected children"
          [] -> expectationFailure "expanded node should have children"

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

    it "50 iterations bestChild has more visits than 10 iterations" $ do
      let config10 = defaultMCTSConfig { mctsIterations = 10, mctsTimeoutMs = 10000 }
          config50 = defaultMCTSConfig { mctsIterations = 50, mctsTimeoutMs = 10000 }
      root10 <- runMCTS config10 testGameState 0
      root50 <- runMCTS config50 testGameState 0
      let visits10 = case bestChild root10 of
            Just c  -> nodeVisits (childNode c)
            Nothing -> 0
          visits50 = case bestChild root50 of
            Just c  -> nodeVisits (childNode c)
            Nothing -> 0
      visits50 `shouldSatisfy` (>= visits10)

    it "bestChild after 50 iterations selects the most-visited child" $ do
      let config = defaultMCTSConfig { mctsIterations = 50, mctsTimeoutMs = 10000 }
      root <- runMCTS config testGameState 0
      case bestChild root of
        Just best -> do
          let bestVisits = nodeVisits (childNode best)
          mapM_ (\c ->
            nodeVisits (childNode c) `shouldSatisfy` (<= bestVisits)
            ) (nodeChildren root)
        Nothing -> expectationFailure "expected a best child"

    it "stops early when timeout is reached before max iterations" $ do
      let config = defaultMCTSConfig { mctsIterations = 100000, mctsTimeoutMs = 50 }
      t0 <- getCurrentTime
      root <- runMCTS config testGameState 0
      t1 <- getCurrentTime
      let elapsed = realToFrac (diffUTCTime t1 t0) :: Double
      nodeVisits root `shouldSatisfy` (< 100000)
      nodeVisits root `shouldSatisfy` (> 0)
      elapsed `shouldSatisfy` (< 5.0)

    it "timeout of 1ms terminates with fewer than max iterations" $ do
      let config = defaultMCTSConfig { mctsIterations = 100000, mctsTimeoutMs = 1 }
      root <- runMCTS config testGameState 0
      nodeVisits root `shouldSatisfy` (< 100000)

    it "full MCTS loop produces bestChild with visits" $ do
      let config = defaultMCTSConfig { mctsIterations = 50, mctsTimeoutMs = 10000 }
      root <- runMCTS config testGameState 0
      case bestChild root of
        Just c  -> nodeVisits (childNode c) `shouldSatisfy` (> 0)
        Nothing -> expectationFailure "expected a best child"

  describe "MCTSAgent" $ do
    let agent = MCTSAgent { mctsConfig = defaultMCTSConfig { mctsIterations = 10, mctsTimeoutMs = 10000 } }

    describe "chooseAction" $ do
      it "returns a member of legal actions" $ do
        let actions = legalActions testGameState
        result <- chooseAction agent testGameState actions
        result `shouldSatisfy` (`elem` actions)

      it "returns singleton action without running MCTS" $ do
        let actions = legalActions testGameState
        case actions of
          (a:_) -> do
            result <- chooseAction agent testGameState [a]
            result `shouldBe` a
          [] -> expectationFailure "expected at least one legal action"

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

      it "always picks highest prestige noble" $ do
        let n1 = Noble "n1" (Map.fromList [(Diamond, 3)]) 3
            n2 = Noble "n2" (Map.fromList [(Ruby, 3)]) 5
            n3 = Noble "n3" (Map.fromList [(Emerald, 3)]) 4
        result <- chooseNoble agent testGameState [n1, n2, n3]
        result `shouldBe` n2

  describe "MCTS vs Random" $ do
    it "MCTS and random play 20 complete games without errors" $ do
      let mctsAgent = MCTSAgent { mctsConfig = defaultMCTSConfig { mctsIterations = 100, mctsTimeoutMs = 10000 } }
          randomAgent = RandomAgent
      results <- mapM (\seed -> playGameDebug mctsAgent randomAgent (mkStdGen seed)) [1..20]
      -- All games should complete without errors
      mapM_ (\(mResult, mErr) -> do
        mErr `shouldBe` Nothing
        mResult `shouldSatisfy` (/= Nothing)
        ) results
      -- All 20 games should produce valid outcomes
      let outcomes = [r | (Just r, _) <- results]
      length outcomes `shouldBe` 20

-- | Play a full game: player 0 = MCTS, player 1 = Random.
--   Returns (True if MCTS wins, error message or Nothing)
playGameDebug :: MCTSAgent -> RandomAgent -> StdGen -> IO (Maybe Bool, Maybe String)
playGameDebug mcts rand gen = do
  let gs = initGameState gen "test-game" 2 ["MCTS", "Random"]
  case gsPlayers gs of
    (p0:_) -> loopD gs (playerId p0) 0
    []     -> pure (Nothing, Just "no players in initial game state")
  where
    loopD :: GameState -> PlayerId -> Int -> IO (Maybe Bool, Maybe String)
    loopD gs player0Id depth
      | depth > 10000 = pure (Nothing, Just "max depth exceeded")
      | otherwise = case gsPhase gs of
          Finished result -> pure (Just (winnerId result == player0Id), Nothing)
          _ -> case gsTurnPhase gs of
            MustReturnGems _ -> do
              let opts = legalGemReturns gs
              case (currentPlayer gs, opts) of
                (Just player, _:_) -> do
                  ret <- if gsCurrentPlayer gs == 0
                         then chooseGemReturn mcts gs opts
                         else chooseGemReturn rand gs opts
                  case applyGemReturn gs (playerId player) ret of
                    Advanced gs' -> loopD gs' player0Id (depth+1)
                    NeedNobleChoice gs' nobles -> handleNobleD gs' player0Id nobles depth
                    GameOver gs' _ -> loopD gs' player0Id (depth+1)
                    other -> pure (Nothing, Just $ "unexpected after gemReturn: " ++ take 50 (show other))
                (Nothing, _) -> pure (Nothing, Just "no current player in MustReturnGems")
                (_, []) -> pure (Nothing, Just "no legal gem returns")
            AwaitingAction -> do
              let actions = legalActions gs
              case (currentPlayer gs, actions) of
                (Just player, _:_) -> do
                  act <- if gsCurrentPlayer gs == 0
                         then chooseAction mcts gs actions
                         else chooseAction rand gs actions
                  case applyAction gs (playerId player) act of
                    Advanced gs' -> loopD gs' player0Id (depth+1)
                    NeedGemReturn gs' n -> loopD (gs' { gsTurnPhase = MustReturnGems n }) player0Id (depth+1)
                    NeedNobleChoice gs' nobles -> handleNobleD gs' player0Id nobles depth
                    GameOver gs' _ -> loopD gs' player0Id (depth+1)
                    StepError err -> pure (Nothing, Just $ "StepError: " ++ show err)
                (Just _player, []) ->
                  -- No legal actions: determine winner by current prestige
                  pure (Just (isPlayer0WinByPrestige (gsPlayers gs) player0Id), Nothing)
                (Nothing, _) -> pure (Nothing, Just "no current player in AwaitingAction")

    handleNobleD :: GameState -> PlayerId -> [Noble] -> Int -> IO (Maybe Bool, Maybe String)
    handleNobleD gs player0Id nobles depth = do
      case currentPlayer gs of
        Just player -> do
          noble <- if gsCurrentPlayer gs == 0
                   then chooseNoble mcts gs nobles
                   else chooseNoble rand gs nobles
          case applyNobleChoice gs (playerId player) (nobleId noble) of
            Advanced gs' -> loopD gs' player0Id (depth+1)
            GameOver gs' _ -> loopD gs' player0Id (depth+1)
            other -> pure (Nothing, Just $ "unexpected after nobleChoice: " ++ take 50 (show other))
        Nothing -> pure (Nothing, Just "no current player in handleNoble")

-- | Check if player 0 has the most prestige.
isPlayer0WinByPrestige :: [Player] -> PlayerId -> Bool
isPlayer0WinByPrestige players p0id =
  case players of
    [] -> False
    _  -> let maxPrestige = maximum (map playerPrestige players)
              p0Prestige = case filter (\p -> playerId p == p0id) players of
                             (p:_) -> playerPrestige p
                             []    -> 0
          in p0Prestige == maxPrestige && maxPrestige > 0

-- | Check if a Move is a MoveNoble (for shouldSatisfy).
isNobleMove' :: Move -> Bool
isNobleMove' (MoveNoble _) = True
isNobleMove' _ = False
