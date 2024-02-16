module Game.Data.Card (
  Card (..)
) where

import Game.Data.Gems

data Card = Card {
  cost :: Gems,
  value :: Gems,
  point :: Int
} deriving (Show, Eq)
