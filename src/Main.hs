module Main where

import Game.Data.Gems
import Game.Data.Game
import GHC.Generics (Generic)
import Data.List (groupBy, sort)
import Game.Data.Player (Player, initPlayer)

main :: IO ()
main = do
  let player = initPlayer "Eric"
  putStrLn $ show player
