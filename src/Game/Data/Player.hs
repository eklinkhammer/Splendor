module Game.Data.Player (
  Player (..)
) where

import Game.Data.Gems
import Game.Data.Card

data Player = Player {
  cards :: [Card],
  name :: String,
  reservedCards :: [Card],
  gems :: Gems
} deriving (Show, Eq)
