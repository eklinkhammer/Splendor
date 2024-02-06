module Game.Data.Gems (
  Color (..),
  Gold (..),
  Gems (..),
  Token (..),
  startingBank,
  startingHand,
  singleWild,
  sumGems
) where

import Data.Map (Map)
import qualified Data.Map as Map

data Color = Red | White | Black | Green | Blue
  deriving (Show, Eq, Enum, Ord)

data Gold = Gold
  deriving (Show, Eq, Enum, Ord)

data Token = Gem Color | Wild Gold
  deriving (Show, Eq, Ord)

type Gems = Map Token Int

numberOfWildsInGame = 5
startingGemsPerPlayer = 0
startingWildsPerPlayer = 0

startingBank :: Int -> Gems
startingBank gemCount = initGems gemCount numberOfWildsInGame

startingHand :: Gems
startingHand = initGems startingGemsPerPlayer startingWildsPerPlayer

initGems :: Int -> Int -> Gems
initGems gemCount wildCount = Map.fromList (gems ++ wilds)
   where
     wilds = [(Wild Gold, wildCount)]
     gems = zipWith (,) [Gem Red, Gem Blue, Gem Black, Gem Green, Gem White] (repeat gemCount)

singleWild :: Gems
singleWild = Map.fromList [(Wild Gold, 1)]

sumGems :: Gems -> Int
sumGems = sum . Map.elems

