module Game.Data.Player (
  Player (..),
  initPlayer
) where

import Game.Data.Card
import Game.Data.Gems
import Game.Data.GemHolder

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

startingHand :: Gems
startingHand = initGems 0 0

cardBuyingPower :: Player -> Gems
cardBuyingPower = totalValue . activeCards

addActiveCard :: Player -> Card -> Player
addActiveCard p c = let currentCards = activeCards p in p { activeCards = c:currentCards }
