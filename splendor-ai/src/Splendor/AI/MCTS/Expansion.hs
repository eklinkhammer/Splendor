module Splendor.AI.MCTS.Expansion
  ( expandAt
  , expandNode
  , applyMove
  ) where

import Splendor.Core.Engine (StepResult(..), applyAction, applyGemReturn, applyNobleChoice)
import Splendor.Core.Rules.ActionValidation (legalActions, legalGemReturns)
import Splendor.Core.Types
import Splendor.AI.MCTS.Tree

-- | Expand a leaf node at the given path. Returns the updated root and the leaf's game state.
expandAt :: MCTSNode -> NodePath -> (MCTSNode, GameState)
expandAt node [] =
  let expanded = expandNode node
  in (expanded, nodeState expanded)
expandAt node (i:rest) =
  let cs = nodeChildren node
      child = cs !! i
      (updatedChild, leafState) = expandAt (childNode child) rest
      newChild = child { childNode = updatedChild }
      newChildren = take i cs ++ [newChild] ++ drop (i + 1) cs
  in (node { nodeChildren = newChildren }, leafState)

-- | Expand a single node by generating all legal moves as children.
expandNode :: MCTSNode -> MCTSNode
expandNode node
  | nodeExpanded node = node
  | nodeTerminal node = node { nodeExpanded = True }
  | otherwise =
      let gs = nodeState node
          children = case gsTurnPhase gs of
            MustReturnGems _ ->
              [ MCTSChild (MoveGemReturn ret) (applyMove (MoveGemReturn ret) gs)
              | ret <- legalGemReturns gs
              ]
            AwaitingAction ->
              [ MCTSChild (MoveAction act) (applyMove (MoveAction act) gs)
              | act <- legalActions gs
              ]
      in node { nodeChildren = children, nodeExpanded = True }

-- | Apply a move to a game state, producing a new MCTSNode.
--   Handles NeedNobleChoice by pre-expanding noble choices inline.
applyMove :: Move -> GameState -> MCTSNode
applyMove (MoveAction action) gs =
  case currentPlayer gs of
    Nothing -> terminalNode gs
    Just player ->
      stepResultToNode (applyAction gs (playerId player) action)

applyMove (MoveGemReturn gems) gs =
  case currentPlayer gs of
    Nothing -> terminalNode gs
    Just player ->
      stepResultToNode (applyGemReturn gs (playerId player) gems)

applyMove (MoveNoble nid) gs =
  case currentPlayer gs of
    Nothing -> terminalNode gs
    Just player ->
      stepResultToNode (applyNobleChoice gs (playerId player) nid)

-- | Convert a StepResult into an MCTSNode.
--   NeedNobleChoice is pre-expanded since it has trivial branching (2-3 options).
stepResultToNode :: StepResult -> MCTSNode
stepResultToNode (Advanced gs') = newTree gs'
stepResultToNode (NeedGemReturn gs' n) =
  let gs'' = gs' { gsTurnPhase = MustReturnGems n }
  in newTree gs''
stepResultToNode (NeedNobleChoice gs' nobles) =
  -- Pre-expand noble choices: create a node with children for each noble
  let parentNode = (newTree gs') { nodeExpanded = True }
      nobleChildren =
        [ MCTSChild (MoveNoble (nobleId noble)) (applyMove (MoveNoble (nobleId noble)) gs')
        | noble <- nobles
        ]
  in parentNode { nodeChildren = nobleChildren }
stepResultToNode (GameOver gs' _result) =
  (newTree gs') { nodeTerminal = True }
stepResultToNode (StepError err) =
  error $ "stepResultToNode: StepError from legal move — engine bug: " ++ show err

-- | Helper to create a terminal node from a game state.
terminalNode :: GameState -> MCTSNode
terminalNode gs = (newTree gs) { nodeTerminal = True }
