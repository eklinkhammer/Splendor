module Splendor.AI.MCTS.Tree
  ( MCTSNode(..)
  , newTree
  ) where

import Splendor.Core.Types (GameState)

data MCTSNode = MCTSNode
  { nodeState    :: GameState
  , nodeVisits   :: Int
  , nodeWins     :: Double
  , nodeChildren :: [MCTSNode]
  }

newTree :: GameState -> MCTSNode
newTree gs = MCTSNode
  { nodeState    = gs
  , nodeVisits   = 0
  , nodeWins     = 0.0
  , nodeChildren = []
  }
