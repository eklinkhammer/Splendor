module Splendor.AI.MCTS.Tree
  ( Move(..)
  , MCTSChild(..)
  , MCTSNode(..)
  , NodePath
  , newTree
  , bestChild
  , isLeaf
  , isTerminal
  , nodeAtPath
  ) where

import Splendor.Core.Types

-- | Unified move type covering all decision points in Splendor.
data Move
  = MoveAction Action
  | MoveGemReturn GemCollection
  | MoveNoble NobleId
  deriving stock (Eq, Show)

-- | Edge-labeled child: a move leading to a subtree.
data MCTSChild = MCTSChild
  { childMove :: Move
  , childNode :: MCTSNode
  } deriving stock (Eq, Show)

-- | MCTS tree node.
data MCTSNode = MCTSNode
  { nodeState     :: GameState
  , nodeVisits    :: Int
  , nodeWins      :: Double
  , nodeChildren  :: [MCTSChild]
  , nodeExpanded  :: Bool
  , nodeTerminal  :: Bool
  , nodePlayerIdx :: Int  -- ^ Whose turn it is at this node
  } deriving stock (Eq, Show)

-- | Path from root to a node, as list of child indices.
type NodePath = [Int]

-- | Create a fresh unexpanded root node.
newTree :: GameState -> MCTSNode
newTree gs = MCTSNode
  { nodeState     = gs
  , nodeVisits    = 0
  , nodeWins      = 0.0
  , nodeChildren  = []
  , nodeExpanded  = False
  , nodeTerminal  = isGameOver gs
  , nodePlayerIdx = gsCurrentPlayer gs
  }

-- | Select the child with the most visits (robust child selection).
bestChild :: MCTSNode -> Maybe MCTSChild
bestChild node = case nodeChildren node of
  [] -> Nothing
  cs -> Just $ foldl1 (\a b -> if nodeVisits (childNode a) >= nodeVisits (childNode b) then a else b) cs

-- | A node is a leaf if it has no children.
isLeaf :: MCTSNode -> Bool
isLeaf = null . nodeChildren

-- | Check if a node represents a terminal game state.
isTerminal :: MCTSNode -> Bool
isTerminal = nodeTerminal

-- | Navigate to a node at a given path. Returns Nothing if path is invalid.
nodeAtPath :: MCTSNode -> NodePath -> Maybe MCTSNode
nodeAtPath node [] = Just node
nodeAtPath node (i:rest)
  | i < 0 || i >= length (nodeChildren node) = Nothing
  | otherwise = nodeAtPath (childNode (nodeChildren node !! i)) rest

-- | Check if the game state represents a finished game.
isGameOver :: GameState -> Bool
isGameOver gs = case gsPhase gs of
  Finished _ -> True
  _          -> False
