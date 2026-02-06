module Splendor.AI.MCTS
  ( MCTSAgent(..)
  , MCTSConfig(..)
  , defaultMCTSConfig
  ) where

import Data.Text (Text)
import Splendor.Core.Types
import Splendor.AI.Agent

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

instance Agent MCTSAgent where
  agentName :: MCTSAgent -> Text
  agentName _ = "MCTS"

  chooseAction :: MCTSAgent -> GameState -> [Action] -> IO Action
  chooseAction _ _ _ = error "MCTS chooseAction not implemented"

  chooseGemReturn :: MCTSAgent -> GameState -> [GemCollection] -> IO GemCollection
  chooseGemReturn _ _ _ = error "MCTS chooseGemReturn not implemented"

  chooseNoble :: MCTSAgent -> GameState -> [Noble] -> IO Noble
  chooseNoble _ _ _ = error "MCTS chooseNoble not implemented"
