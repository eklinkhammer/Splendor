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
  fromInteger x = initGems (fromIntegral x) 0 

initGems :: Int -> Int -> Gems
initGems gemCount wildCount = Map.fromList (gems ++ wilds)
  where
    wilds = [(Wild Gold, wildCount)]
    gems = zipWith (,) [Gem Red, Gem Blue, Gem Black, Gem Green, Gem White] (repeat gemCount)
