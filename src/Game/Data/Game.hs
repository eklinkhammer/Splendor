module Game.Data.Game (
  Game (..),
  isActionPossible
) where

import Data.Game.Gems
import Data.Game.Card
import Data.Game.Player
import Data.Game.Action
import qualified Data.Map as Map

type Deck = ([Card], [Card])

data Game = Game {
  players :: [Player],
  topDeck :: Deck,
  middleDeck :: Deck,
  bottomDeck :: Deck,
  bank :: Gems
} deriving (Show, Eq)


isActionPossible :: Action -> Player -> Game -> Boolean
isActionPossible (Purchase card) player game = canPlayerPurchase
isActionPossible (Reserve card) player game = canPlayerReserve
isActionPossible (Take gems) player game = canPlayerTakeGems
  where
    canPlayerPurchase = isCardAvailable card game && canPlayerAfford card player
    canPlayerReserve = isCardAvailable card game && canPlayerTakeGems player singleWild
    canPlayerTakeGems = doesBankHaveGems game gems && canPlayerTakeGems player gems

isCardAvailable :: Card -> Game -> Boolean
isCardAvailable card game = elem card $ allVisible game

canPlayerTakeGems :: Player -> Gems -> Boolean
canPlayerTakeGems player gems = isLegalAmountOfGems && playerHasSpace
  where
    isLegalAmountOfGems = takeDifferent || takeWild || takeTwo
    takeTwo = noWild && oneDuplicate && count == 2
    takeDifferent = noWild && noDuplicates && count <= 3
    takeWild = oneWild && count == 1
    oneWild = gems Map.! (Wild Gold) == 1
    noWild = gems Map.! (Wild Gold) == 0
    noDuplicates = all (\(_,x) -> x == 1 || x == 0) $ Map.elems gems
    playerHasSpace = sumGems (gems player) + count <= 10
    count = sumGems gems
   
doesBankHaveGems :: Game -> Gems -> Boolean
doesBankHaveGems game gems = tokensExist + enoughLeft
  where
    tokensExist = all (\x -> x >= 0) $ Map.unionWith (-) (bank game) gems
    enoughLeft = leaveFour allValuesInBankForDoubles
    allValuesInBankForDoubles = Map.mapWithKey (\k v -> (bank gems) Map.! k) $ filter (\x -> x > 1) gems

leaveFour :: [Maybe Int] -> Boolean
leaveFour [] = True
leaveFour (x:xs) = case x of
  (Just n) -> n >= 4 && leaveFour xs
  Nothing -> False

allVisible :: Game -> [Card]
allVisible game = [topDeck game, middleDeck game, bottomDeck game] >>= snd
