module Main where

import Game.Data.Gems
import Game.Data.Game
import GHC.Generics (Generic)
import Data.List (groupBy, sort)

main :: IO ()
main = do
  let player = Player "Eric" startingHand [] []
  putStrLn $ show player
