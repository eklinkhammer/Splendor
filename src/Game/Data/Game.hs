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

activePlayer :: Game -> Player
activePlayer = head . players

-- Updates game state. First player is assumed to be active.  
doAction :: Game -> Action -> Game
doAction game (Purchase card) = undefined
doAction game (Reserve card) = undefined
doAction game (Take gems) = takeGems game gems
  where
    player = activePlayer game
     
takeGems :: Game -> Gems -> Game
takeGems game gems = game { bank = updatedBank, players = updatePlayers game updatedPlayer }
  where
    updatedBank = Map.unionWith (-) (bank game) gems
    player = activePlayer game
    updatedPlayer = player { playerGems = Map.unionWith (+) gems (playerGems player) }


updatePlayers :: Game -> Player -> Game
updatePlayers game playerAfterAction = game { players = (tail $ players game) ++ [playerAfterAction] }

isActionPossible :: Action -> Player -> Game -> Bool
isActionPossible (Purchase card) player game = canPlayerPurchase card game player
isActionPossible (Reserve card) player game = canPlayerReserve card game player
isActionPossible (Take gems) player game = playerAbleToMake game gems player

canPlayerPurchase :: Card -> Game -> Player -> Bool
canPlayerPurchase card game player = isCardAvailable card game && canPlayerAfford card player

canPlayerAfford :: Card -> Player -> Bool
canPlayerAfford card player = wildsRequiredForPurchase card player <= (playerGems player Map.! (Wild Gold))

payForCard :: Player -> Card -> Player
payForCard player card = player { playerGems = newGems }
  where
    withoutCost = removeCardCost card player
    noNegatives = Map.map (max 0) withoutCost
    newGems = Map.adjust (\g -> g - (wildsRequiredForPurchase card player)) (Wild Gold) noNegatives
removeCardCost :: Card -> Player -> Gems
removeCardCost card player = Map.unionWith (-) (playerGems player) (cost card)

wildsRequiredForPurchase :: Card -> Player -> Int
wildsRequiredForPurchase c = sum . Map.elems . Map.filter (\x -> x < 0) . removeCardCost c

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

allVisible :: Game -> [Card]
allVisible game = [topDeck game, middleDeck game, bottomDeck game] >>= snd
