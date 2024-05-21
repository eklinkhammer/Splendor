module Game.Data.Player (
  Player (..),
  initPlayer
) where

import Game.Data.Gems
import Game.Data.Card

data Player = Player {
  activeCards :: [Card],
  name :: String,
  reservedCards :: [Card],
  playerGems :: Gems
} deriving (Show, Eq)

initPlayer :: String -> Player
initPlayer name = Player [] name [] startingHand

instance GemHolder Player where
  gems = playerGems
  updateGems a gems = a { playerGems = gems }
