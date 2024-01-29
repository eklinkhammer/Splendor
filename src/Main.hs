module Main where

import Game.Data.Gems

import GHC.Generics (Generic)
import Data.List (groupBy, sort)

data Player = Player {
  name :: String,
  gems :: Gems,
  cards :: [Card],
  reservedCards :: [Card]
} deriving (Show, Eq)

data Card = Card {
  cost :: Gems,
  points :: Int,
  value :: Gems
} deriving (Show, Eq)

data Board = Board {
  decks :: ([Card], [Card], [Card]),
  faceup :: ([Card], [Card], [Card]),
  bank :: Gems
} deriving (Show, Eq)

data Game = Game {
  players :: [Player],
  board :: Board
} deriving (Show, Eq)

main :: IO ()
main = do
  let player = Player "Eric" startingHand [] []
  putStrLn $ show player
