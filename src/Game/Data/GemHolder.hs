{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}


-- A GemHolder is any data type that has Gems
module Game.Data.GemHolder where

import Game.Data.Gems hiding (GemHolder)

import Data.Map (Map)
import qualified Data.Map as Map

class GemHolder a where
  gems :: a -> Gems
  updateGems :: a -> Gems -> a

instance GemHolder Gems where
  gems = id
  updateGems _ = id

addGems :: (GemHolder a, GemHolder b) => a -> b -> a
addGems a b = updateGems a (gems a + gems b)

removeGems :: (GemHolder a, GemHolder b) => a -> b -> a
removeGems a b = updateGems a (gems a - gems b)

allGemsNonNegative :: (GemHolder a) => a -> Bool
allGemsNonNegative = all (\x -> x >= 0) . Map.elems . gems

canTake :: (GemHolder a, GemHolder b) => a -> b -> Bool
canTake a = allGemsNonNegative . removeGems a

-- Can a purchase b, returns the price a would have to pay
-- Prioritizes keeping wilds
canBuy :: (GemHolder a, GemHolder b) => a -> b -> Maybe Gems
canBuy a b = if sufficientWilds then Just (gems gemTotalAfterSpendingWilds) else Nothing
  where
    gemTotalWithoutSpendingWilds = gems $ removeGems a b
    requiredWilds = sumGems $ Map.map (min 0) gemTotalWithoutSpendingWilds
    sufficientWilds = wildCount gemTotalWithoutSpendingWilds - requiredWilds >= 0
    allPositiveGemTotals = Map.map (max 0) gemTotalWithoutSpendingWilds
    wildCost = nWilds requiredWilds
    gemTotalAfterSpendingWilds = removeGems allPositiveGemTotals wildCost

singleWild :: Gems
singleWild = nWilds 1

nWilds :: Int -> Gems
nWilds n = Map.fromList [(Wild Gold, n)]

sumGems :: (GemHolder a) => a -> Int
sumGems = sum . Map.elems . gems

wildCount :: (GemHolder a) => a -> Int
wildCount a = (gems a) Map.! (Wild Gold) 
