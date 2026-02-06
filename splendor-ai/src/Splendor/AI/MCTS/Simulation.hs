module Splendor.AI.MCTS.Simulation
  ( simulate
  , simulateFromState
  , heuristicScore
  ) where

import System.Random (randomRIO)
import Splendor.Core.Engine (StepResult(..), applyAction, applyGemReturn, applyNobleChoice)
import Splendor.Core.Rules.ActionValidation (legalActions, legalGemReturns)
import Splendor.Core.Types
import Splendor.AI.MCTS.Tree (MCTSNode(..))

-- | Random rollout from a node's game state.
--   Returns 1.0 for a win by perspectivePlayer, 0.0 for a loss.
--   Uses prestige-ratio heuristic if max depth (200 plies) is reached.
simulate :: MCTSNode -> Int -> IO Double
simulate node = simulateFromState (nodeState node)

-- | Random rollout from a game state directly.
--   Returns 1.0 for a win by perspectivePlayer, 0.0 for a loss.
--   Uses prestige-ratio heuristic if max depth (200 plies) is reached.
simulateFromState :: GameState -> Int -> IO Double
simulateFromState gs perspectivePlayer = rollout gs 0
  where
    maxDepth :: Int
    maxDepth = 200

    rollout :: GameState -> Int -> IO Double
    rollout gs' depth
      | depth >= maxDepth = pure (heuristicScore gs' perspectivePlayer)
      | otherwise = case gsPhase gs' of
          Finished result ->
            if winnerId result == perspectivePlayerId gs'
            then pure 1.0
            else pure 0.0
          _ -> case gsTurnPhase gs' of
            MustReturnGems _ -> do
              let returns = legalGemReturns gs'
              case returns of
                [] -> pure (heuristicScore gs' perspectivePlayer)
                _  -> do
                  ret <- pickRandom returns
                  case currentPlayer gs' of
                    Nothing -> pure 0.5
                    Just player ->
                      case applyGemReturn gs' (playerId player) ret of
                        Advanced gs'' -> rollout gs'' (depth + 1)
                        NeedNobleChoice gs'' nobles -> do
                          noble <- pickRandom nobles
                          case applyNobleChoice gs'' (playerId player) (nobleId noble) of
                            Advanced gs''' -> rollout gs''' (depth + 1)
                            GameOver gs''' _ -> rollout gs''' (depth + 1)
                            _ -> pure 0.5
                        GameOver gs'' _ -> rollout gs'' (depth + 1)
                        _ -> pure 0.5
            AwaitingAction -> do
              let actions = legalActions gs'
              case actions of
                [] -> pure (heuristicScore gs' perspectivePlayer)
                _  -> do
                  action <- pickRandom actions
                  case currentPlayer gs' of
                    Nothing -> pure 0.5
                    Just player -> handleStepResult gs' player action depth

    handleStepResult :: GameState -> Player -> Action -> Int -> IO Double
    handleStepResult gs' player action depth =
      case applyAction gs' (playerId player) action of
        Advanced gs'' -> rollout gs'' (depth + 1)
        NeedGemReturn gs'' n -> do
          let gs''' = gs'' { gsTurnPhase = MustReturnGems n }
          rollout gs''' (depth + 1)
        NeedNobleChoice gs'' nobles -> do
          noble <- pickRandom nobles
          case applyNobleChoice gs'' (playerId player) (nobleId noble) of
            Advanced gs''' -> rollout gs''' (depth + 1)
            GameOver gs''' _ -> rollout gs''' (depth + 1)
            _ -> pure 0.5
        GameOver gs'' _ -> rollout gs'' (depth + 1)
        StepError _ -> pure 0.5

    perspectivePlayerId :: GameState -> PlayerId
    perspectivePlayerId gs' =
      let ps = gsPlayers gs'
      in if perspectivePlayer >= 0 && perspectivePlayer < length ps
         then playerId (ps !! perspectivePlayer)
         else ""

-- | Heuristic score based on prestige ratio when rollout hits depth limit.
heuristicScore :: GameState -> Int -> Double
heuristicScore gs perspectiveIdx =
  let ps = gsPlayers gs
  in if null ps || perspectiveIdx < 0 || perspectiveIdx >= length ps
     then 0.5
     else
       let myPrestige = playerPrestige (ps !! perspectiveIdx)
           maxPrestige = maximum (map playerPrestige ps)
       in if maxPrestige == 0
          then 0.5
          else fromIntegral myPrestige / fromIntegral maxPrestige

pickRandom :: [a] -> IO a
pickRandom [] = error "pickRandom: empty list"
pickRandom xs = do
  i <- randomRIO (0, length xs - 1)
  pure (xs !! i)
