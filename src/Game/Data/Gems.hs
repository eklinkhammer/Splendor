{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Game.Data.Gems where

import Data.Map (Map)
import qualified Data.Map as Map

data Color = Red | White | Black | Green | Blue
  deriving (Show, Eq, Enum, Ord)

data Gold = Gold
  deriving (Show, Eq, Enum, Ord)

data Token = Gem Color | Wild Gold
  deriving (Show, Eq, Ord)

type Gems = Map Token Int

instance Num Gems where
  (+) = Map.unionWith (+)
  (-) = Map.unionWith (-)
  signum = Map.map signum
  (*) = Map.unionWith (*)
  abs = Map.map abs
  fromInteger = startingBank . fromInteger 

class GemHolder a where
  gems :: a -> Gems
  updateGems :: a -> Gems -> a
  allGems :: (Int -> Bool) -> a -> Bool
  allGems test = all test . Map.elems . gems

instance GemHolder Gems where
  gems = id
  updateGems _ = id

addGems :: (GemHolder a, GemHolder b) => a -> b -> a
addGems a b = updateGems a (gems a + gems b)

removeGems :: (GemHolder a, GemHolder b) => a -> b -> a
removeGems a b = updateGems a (gems a - gems b)

canTake :: (GemHolder a, GemHolder b) => a -> b -> Bool
canTake a = all (\x -> x >= 0) . Map.elems . gems . removeGems a

-- Can a purchase b, returns the price a would have to pay
-- Prioritizes keeping wilds
canBuy :: (GemHolder a, GemHolder b) => a -> b -> Maybe Gems
canBuy a b = if sufficientWilds then (Just price) else Nothing
  where
    withoutCost = gems $ removeGems a b
    wildDeficit = sumGems $ Map.map (min 0) withoutCost
    sufficientWilds = wildCount withoutCost + wildDeficit >= 0
    withoutCostOrWilds = Map.adjust (\w -> w + wildDeficit) (Wild Gold) withoutCost
    withoutCostOrWildsPositive = Map.map (max 0) withoutCostOrWilds
    price = gems $ removeGems a withoutCostOrWildsPositive

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

sumGems :: (GemHolder a) => a -> Int
sumGems = sum . Map.elems . gems

wildCount :: (GemHolder a) => a -> Int
wildCount a = (gems a) Map.! (Wild Gold) 
