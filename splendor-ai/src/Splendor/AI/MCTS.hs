module Splendor.AI.MCTS
  ( MCTSAgent(..)
  , MCTSConfig(..)
  , defaultMCTSConfig
  , runMCTS
  ) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Splendor.Core.Types
import Splendor.AI.Agent
import Splendor.AI.MCTS.Tree
import Splendor.AI.MCTS.Selection (select)
import Splendor.AI.MCTS.Expansion (expandAt)
import Splendor.AI.MCTS.Simulation (simulateFromState)
import Splendor.AI.MCTS.Backpropagation (backpropagate)

data MCTSConfig = MCTSConfig
  { mctsIterations     :: Int
  , mctsExplorationC   :: Double
  , mctsTimeoutMs      :: Int
  } deriving stock (Show)

defaultMCTSConfig :: MCTSConfig
defaultMCTSConfig = MCTSConfig
  { mctsIterations   = 1000
  , mctsExplorationC = 1.414
  , mctsTimeoutMs    = 5000
  }

data MCTSAgent = MCTSAgent
  { mctsConfig :: MCTSConfig
  }

-- | Run the MCTS search loop, returning the root node after all iterations.
runMCTS :: MCTSConfig -> GameState -> Int -> IO MCTSNode
runMCTS config gs perspectivePlayer = do
  let root = newTree gs
  startTime <- getCurrentTime
  loop root 0 startTime
  where
    maxIter = mctsIterations config
    timeoutSec = fromIntegral (mctsTimeoutMs config) / 1000.0 :: Double

    loop :: MCTSNode -> Int -> UTCTime -> IO MCTSNode
    loop node iter startT
      | iter >= maxIter = pure node
      | otherwise = do
          -- Check timeout every 10 iterations for performance
          timedOut <- if iter > 0 && iter `mod` 10 == 0
            then do
              now <- getCurrentTime
              let elapsed = realToFrac (diffUTCTime now startT) :: Double
              pure (elapsed > timeoutSec)
            else pure False
          if timedOut
            then pure node
            else do
              let path = select (mctsExplorationC config) node
                  (expanded, leafState) = expandAt node path
              result <- simulateFromState leafState perspectivePlayer
              let updated = backpropagate result perspectivePlayer path expanded
              loop updated (iter + 1) startT

instance Agent MCTSAgent where
  agentName :: MCTSAgent -> Text
  agentName _ = "MCTS"

  chooseAction :: MCTSAgent -> GameState -> [Action] -> IO Action
  chooseAction _ _ [] = error "chooseAction: no legal actions"
  chooseAction _ _ [a] = pure a  -- skip search for singleton
  chooseAction agent gs (fallback:_) = do
    let perspective = gsCurrentPlayer gs
    root <- runMCTS (mctsConfig agent) gs perspective
    case bestChild root of
      Just (MCTSChild (MoveAction act) _) -> pure act
      -- Unexpected move type or no children — fall back to first legal action
      _ -> pure fallback

  chooseGemReturn :: MCTSAgent -> GameState -> [GemCollection] -> IO GemCollection
  chooseGemReturn _ _ [] = error "chooseGemReturn: no options"
  chooseGemReturn _ _ [r] = pure r
  chooseGemReturn agent gs (fallback:_) = do
    -- Use a shorter search for gem return decisions
    let shortConfig = (mctsConfig agent) { mctsIterations = 100, mctsTimeoutMs = 500 }
        perspective = gsCurrentPlayer gs
    root <- runMCTS shortConfig gs perspective
    case bestChild root of
      Just (MCTSChild (MoveGemReturn ret) _) -> pure ret
      -- Unexpected move type or no children — fall back to first option
      _ -> pure fallback

  chooseNoble :: MCTSAgent -> GameState -> [Noble] -> IO Noble
  chooseNoble _ _ [] = error "chooseNoble: no nobles"
  chooseNoble _ _ [n] = pure n
  chooseNoble _ _ nobles =
    -- Pick highest prestige noble (all standard nobles are 3 prestige, so effectively first)
    pure $ foldl1 (\a b -> if noblePrestige a >= noblePrestige b then a else b) nobles
