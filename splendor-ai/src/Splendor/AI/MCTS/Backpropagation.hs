module Splendor.AI.MCTS.Backpropagation
  ( backpropagate
  ) where

import Splendor.AI.MCTS.Tree

-- | Backpropagate a simulation result along the path from root to leaf.
--   Each node along the path gets +1 visit.
--   Win attribution: if the node's player matches the perspective player,
--   add the result; otherwise add (1 - result).
backpropagate :: Double -> Int -> NodePath -> MCTSNode -> MCTSNode
backpropagate result perspectivePlayer path = go path
  where
    go :: NodePath -> MCTSNode -> MCTSNode
    go [] node =
      let winDelta = if nodePlayerIdx node == perspectivePlayer then result else 1.0 - result
      in node
        { nodeVisits = nodeVisits node + 1
        , nodeWins   = nodeWins node + winDelta
        }
    go (i:rest) node =
      let winDelta = if nodePlayerIdx node == perspectivePlayer then result else 1.0 - result
          cs = nodeChildren node
          child = cs !! i
          updatedChild = child { childNode = go rest (childNode child) }
          newChildren = take i cs ++ [updatedChild] ++ drop (i + 1) cs
      in node
        { nodeVisits   = nodeVisits node + 1
        , nodeWins     = nodeWins node + winDelta
        , nodeChildren = newChildren
        }
