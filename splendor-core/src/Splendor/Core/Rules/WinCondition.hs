module Splendor.Core.Rules.WinCondition
  ( winThreshold
  , checkWinCondition
  , determineWinner
  ) where

import Data.List (sortBy)
import Data.Ord (Down(..), comparing)
import Splendor.Core.Types.Player (Player(..), playerPrestige)
import Splendor.Core.Types.GameState (GameResult(..))

-- | Prestige threshold to trigger final round
winThreshold :: Int
winThreshold = 15

-- | Check if any player has reached the win threshold
checkWinCondition :: [Player] -> Bool
checkWinCondition = any (\p -> playerPrestige p >= winThreshold)

-- | Determine the winner among players with >= winThreshold prestige.
--   Tiebreaker: fewest purchased cards.
determineWinner :: [Player] -> Maybe GameResult
determineWinner players =
  case filter (\p -> playerPrestige p >= winThreshold) players of
    [] -> Nothing
    candidates ->
      let sorted = sortBy (comparing (Down . playerPrestige)
                          <> comparing (length . playerPurchased))
                          candidates
      in case sorted of
        [] -> Nothing
        (winner:_) -> Just GameResult
           { winnerId = playerId winner
           , winnerName = playerName winner
           , finalPrestige = playerPrestige winner
           }
