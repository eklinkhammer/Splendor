module Splendor.AI.Agent
  ( Agent(..)
  ) where

import Data.Text (Text)
import Splendor.Core.Types

class Agent a where
  agentName       :: a -> Text
  chooseAction    :: a -> GameState -> [Action] -> IO Action
  chooseGemReturn :: a -> GameState -> [GemCollection] -> IO GemCollection
  chooseNoble     :: a -> GameState -> [Noble] -> IO Noble
