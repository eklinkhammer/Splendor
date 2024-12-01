module Game.Data.Game (
  Game (..),
  isActionPossible
) where

import Game.Data.Gems
import Game.Data.GemHolder
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

instance GemHolder Game where
  gems = bank
  updateGems bank gems = bank { bank = gems }

activePlayer :: Game -> Player
activePlayer = head . players

singlePlayerTurn :: Game -> Action -> Game
singlePlayerTurn game action = if isLegalAction game action then updatePlayers afterActionGameState
  where
    afterActionGameState = doAction game action

-- Updates game state. First player is assumed to be active.  
doAction :: Game -> Action -> Game
doAction game (Purchase card) = undefined
doAction game (Reserve card) = undefined
doAction game (Take gems) = takeGems game gems
  where
    player = activePlayer game

updatePlayerAndAdvanceActivePlayer :: Game -> Player -> Game
updatePlayerAndAdvanceActivePlayer game updatedPlayer = game { players = updatedPlayerList }
  where
    updatedPlayerList = withoutPlayerWhoJustWent ++ [updatedPlayer]
    withoutPlayerWhoJustWent = tail (players game)

takeGems :: Game -> Gems -> (Game, Player)
takeGems game gems = (updatedBank, updatedPlayer)
  where
    updatedBank = removeGems game gems
    updatedPlayer = addGems player gems
    player = activePlayer game

takeGems :: Game -> Gems -> Game
takeGems game gems = updatePlayers (game { bank = updatedBank }) updatedPlayer
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

canPlayerAfford :: Player -> Card -> Bool
canPlayerAfford player card = case canBuy player (effectiveCostForCard card) of
  Nothing -> True
  _ -> True

effectiveCostForCard :: Player -> Card -> Gems
effectiveCostForCard player card = removeNegatives $ removeGems card (cardBuyingPower player) 

removeNegatives :: Gems -> Gems
removeNegatives = Map.map (max 0)

payForCard :: Player -> Card -> Player
payForCard player card = removeGems player tokensForPurchase
  where
    costToPlayer = effectiveCostForCard player card
    tokensForPurchase = canBuy player costToPlayer

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
    oneWild = wildCount gems == 1
    noWild = wildCount gems == 0
    noDuplicates = allGems (\x -> x == 1 || x == 0) gems
    oneDuplicate = allGems (\x -> x == 2 || x == 0) gems && sumGems gems == 2
    playerHasSpace = sumGems player + count <= 10
    count = sumGems gems
   
doesBankHaveGems :: Game -> Gems -> Bool
doesBankHaveGems game gems = tokensExist && enoughLeft
  where
    tokensExist = canTake game gems
    enoughLeft = allGems (\x -> x > 4) allValuesInBankForDoubles
    allValuesInBankForDoubles = Map.intersection (bank game) gemsChosenMoreThanOnce
    gemsChosenMoreThanOnce = Map.filter (\x -> x > 1) gems

allVisible :: Game -> [Card]
allVisible game = [topDeck game, middleDeck game, bottomDeck game] >>= snd
