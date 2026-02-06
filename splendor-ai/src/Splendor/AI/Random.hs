module Splendor.AI.Random
  ( RandomAgent(..)
  ) where

import Data.Text (Text)
import System.Random (randomRIO)
import Splendor.Core.Types
import Splendor.AI.Agent

data RandomAgent = RandomAgent

instance Agent RandomAgent where
  agentName :: RandomAgent -> Text
  agentName _ = "Random"

  chooseAction :: RandomAgent -> GameState -> [Action] -> IO Action
  chooseAction _ _ actions = pickRandom actions

  chooseGemReturn :: RandomAgent -> GameState -> [GemCollection] -> IO GemCollection
  chooseGemReturn _ _ options = pickRandom options

  chooseNoble :: RandomAgent -> GameState -> [Noble] -> IO Noble
  chooseNoble _ _ nobles = pickRandom nobles

pickRandom :: [a] -> IO a
pickRandom [] = error "pickRandom: empty list"
pickRandom xs = do
  i <- randomRIO (0, length xs - 1)
  pure (xs !! i)
