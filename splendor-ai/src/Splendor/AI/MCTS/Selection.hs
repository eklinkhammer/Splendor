module Splendor.AI.MCTS.Selection
  ( select
  , ucb1
  ) where

import Splendor.AI.MCTS.Tree

-- | Select a leaf node using UCB1 scores. Returns the path from root to leaf.
select :: Double -> MCTSNode -> NodePath
select explorationC = go []
  where
    go path node
      | isLeaf node || isTerminal node = reverse path
      | otherwise =
          let parentVisits = nodeVisits node
              cs = nodeChildren node
              scored = zip [0..] (map (scoreChild explorationC parentVisits . childNode) cs)
              bestIdx = fst $ foldl1 (\a b -> if snd a >= snd b then a else b) scored
          in go (bestIdx : path) (childNode (cs !! bestIdx))

-- | Compute UCB1 score for a child node.
--   Returns infinity for unvisited nodes so they are always explored first.
ucb1 :: Double -> Int -> Int -> Double -> Double
ucb1 _ _ 0 _ = 1.0 / 0.0  -- infinity for unvisited
ucb1 explorationC parentVisits childVisits childWins =
  let exploitation = childWins / fromIntegral childVisits
      exploration  = explorationC * sqrt (log (fromIntegral parentVisits) / fromIntegral childVisits)
  in exploitation + exploration

-- | Score a child node using UCB1.
scoreChild :: Double -> Int -> MCTSNode -> Double
scoreChild explorationC parentVisits child =
  ucb1 explorationC parentVisits (nodeVisits child) (nodeWins child)
