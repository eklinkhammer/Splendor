module Game.Data.Game (
  Game (..),
  isActionPossible
) where

import Game.Data.Gems
import Game.Data.Card
import Game.Data.Player
import Game.Data.Action
import qualified Data.Map as Map

type Deck = ([Card], [Card])

data Game = Game {
  players :: [Player],
  topDeck :: Deck,
  middleDeck :: Deck,
  bottomDeck :: Deck,
  bank :: Gems
} deriving (Show, Eq)


isActionPossible :: Action -> Player -> Game -> Bool
isActionPossible (Purchase card) player game = canPlayerPurchase card game player
isActionPossible (Reserve card) player game = canPlayerReserve card game player
isActionPossible (Take gems) player game = playerAbleToMake game gems player

canPlayerPurchase :: Card -> Game -> Player -> Bool
canPlayerPurchase card game player = isCardAvailable card game && canPlayerAfford card player

canPlayerAfford :: Card -> Player -> Bool
canPlayerAfford card player = False

canPlayerReserve :: Card -> Game -> Player -> Bool
canPlayerReserve card game player = isCardAvailable card game && canPlayerTakeGems player singleWild

playerAbleToMake :: Game -> Gems -> Player -> Bool
playerAbleToMake game gems player = doesBankHaveGems game gems && canPlayerTakeGems player gems

isCardAvailable :: Card -> Game -> Bool
isCardAvailable card game = elem card $ allVisible game

canPlayerTakeGems :: Player -> Gems -> Bool
canPlayerTakeGems player gems = isLegalAmountOfGems && playerHasSpace
  where
    isLegalAmountOfGems = takeDifferent || takeWild || takeTwo
    takeTwo = noWild && oneDuplicate && count == 2
    takeDifferent = noWild && noDuplicates && count <= 3
    takeWild = oneWild && count == 1
    oneWild = gems Map.! (Wild Gold) == 1
    noWild = gems Map.! (Wild Gold) == 0
    noDuplicates = all (\x -> x == 1 || x == 0) $ Map.elems gems
    oneDuplicate = True
    playerHasSpace = sumGems (playerGems player) + count <= 10
    count = sumGems gems
   
doesBankHaveGems :: Game -> Gems -> Bool
doesBankHaveGems game gems = tokensExist && enoughLeft
  where
    tokensExist = all (\x -> x >= 0) $ Map.unionWith (-) (bank game) gems
    enoughLeft = all (\x -> x > 4) $ Map.elems allValuesInBankForDoubles
    allValuesInBankForDoubles = Map.intersection (bank game) gemsChosenMoreThanOnce
    gemsChosenMoreThanOnce = Map.filter (\x -> x > 1) gems

leaveFour :: [Maybe Int] -> Bool
leaveFour [] = True
leaveFour (x:xs) = case x of
  (Just n) -> n >= 4 && leaveFour xs
  Nothing -> False

allVisible :: Game -> [Card]
allVisible game = [topDeck game, middleDeck game, bottomDeck game] >>= snd
