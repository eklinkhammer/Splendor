module Splendor.AI.MCTS.Backpropagation
  ( backpropagate
  ) where

import Splendor.AI.MCTS.Tree

-- | Backpropagate a simulation result along the path from root to leaf.
--   Each node along the path gets +1 visit.
--   Win attribution: perspective-only. Only the perspective player's nodes
--   receive the result; all other players' nodes receive 0.0.
--   This is correct for multiplayer (3-4 player) games where opponents
--   are independent agents rather than a single adversary.
backpropagate :: Double -> Int -> NodePath -> MCTSNode -> MCTSNode
backpropagate result perspectivePlayer path = go path
  where
    go :: NodePath -> MCTSNode -> MCTSNode
    go [] node =
      let winDelta = if nodePlayerIdx node == perspectivePlayer then result else 0.0
      in node
        { nodeVisits = nodeVisits node + 1
        , nodeWins   = nodeWins node + winDelta
        }
    go (i:rest) node =
      let winDelta = if nodePlayerIdx node == perspectivePlayer then result else 0.0
          cs = nodeChildren node
          child = cs !! i
          updatedChild = child { childNode = go rest (childNode child) }
          newChildren = take i cs ++ [updatedChild] ++ drop (i + 1) cs
      in node
        { nodeVisits   = nodeVisits node + 1
        , nodeWins     = nodeWins node + winDelta
        , nodeChildren = newChildren
        }
