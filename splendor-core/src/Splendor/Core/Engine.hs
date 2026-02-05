module Splendor.Core.Engine
  ( StepResult(..)
  , applyAction
  , applyGemReturn
  , applyNobleChoice
  ) where

import Data.Text qualified as T
import Splendor.Core.Types
import Splendor.Core.Rules.GemLogic
import Splendor.Core.Rules.CardLogic
import Splendor.Core.Rules.NobleLogic
import Splendor.Core.Rules.WinCondition
import Splendor.Core.Rules.ActionValidation (validateAction)

data StepResult
  = Advanced GameState
  | NeedGemReturn GameState Int
  | NeedNobleChoice GameState [Noble]
  | GameOver GameState GameResult
  | StepError ActionError
  deriving stock (Eq, Show)

applyAction :: GameState -> PlayerId -> Action -> StepResult
applyAction gs pid action =
  case validateAction gs pid action of
    Left err -> StepError err
    Right _  -> executeAction gs action

executeAction :: GameState -> Action -> StepResult
executeAction gs (TakeGems gemTake) =
  case currentPlayer gs of
    Nothing -> StepError (InvalidState "Invalid player index")
    Just player ->
      let gems = gemsFromTake gemTake
          newPlayerTokens = addGems (playerTokens player) gems
          newBankTokens = subtractGems (boardBank (gsBoard gs)) gems
          newPlayer = player { playerTokens = newPlayerTokens }
          newBoard = (gsBoard gs) { boardBank = newBankTokens }
          gs' = updateCurrentPlayer gs newPlayer
          gs'' = gs' { gsBoard = newBoard }
          excess = totalGems newPlayerTokens - maxTokensLimit
      in if excess > 0
         then NeedGemReturn gs'' excess
         else finishTurn gs''

executeAction gs (BuyCard source payment) =
  case currentPlayer gs of
    Nothing -> StepError (InvalidState "Invalid player index")
    Just player ->
      let board = gsBoard gs
      in case resolveCardSource source player board of
        Left err -> StepError err
        Right (card, newBoard) ->
          let newPlayerTokens = subtractGems (playerTokens player) payment
              newBankTokens = addGems (boardBank newBoard) payment
              newPurchased = playerPurchased player ++ [card]
              newReserved = case source of
                FromReserve cid -> filter (\c -> cardId c /= cid) (playerReserved player)
                _ -> playerReserved player
              newPlayer = player
                { playerTokens = newPlayerTokens
                , playerPurchased = newPurchased
                , playerReserved = newReserved
                }
              newBoard' = newBoard { boardBank = newBankTokens }
              gs' = updateCurrentPlayer gs newPlayer
              gs'' = gs' { gsBoard = newBoard' }
          in finishTurn gs''

executeAction gs (ReserveCard source) =
  case currentPlayer gs of
    Nothing -> StepError (InvalidState "Invalid player index")
    Just player ->
      let board = gsBoard gs
      in case resolveReserveSource source board of
        Left err -> StepError err
        Right (card, newBoard) ->
          let goldAvailable = gemCount (boardBank newBoard) GoldToken
              (newPlayerTokens, newBankTokens)
                | goldAvailable > 0 =
                    ( addGems (playerTokens player) (singleGem GoldToken 1)
                    , subtractGems (boardBank newBoard) (singleGem GoldToken 1)
                    )
                | otherwise = (playerTokens player, boardBank newBoard)
              newPlayer = player
                { playerTokens = newPlayerTokens
                , playerReserved = playerReserved player ++ [card]
                }
              newBoard' = newBoard { boardBank = newBankTokens }
              gs' = updateCurrentPlayer gs newPlayer
              gs'' = gs' { gsBoard = newBoard' }
              excess = totalGems newPlayerTokens - maxTokensLimit
          in if excess > 0
             then NeedGemReturn gs'' excess
             else finishTurn gs''

maxTokensLimit :: Int
maxTokensLimit = 10

applyGemReturn :: GameState -> PlayerId -> GemCollection -> StepResult
applyGemReturn gs pid gems =
  case gsTurnPhase gs of
    MustReturnGems n ->
      case currentPlayer gs of
        Nothing -> StepError (InvalidState "Invalid player index")
        Just player
          | playerId player /= pid -> StepError NotYourTurn
          | totalGems gems /= n ->
              StepError (OtherError $ "Must return exactly " <> T.pack (show n) <> " gems")
          | not (hasEnoughGems (playerTokens player) gems) ->
              StepError (InvalidPayment "Don't have those gems")
          | otherwise ->
              let newPlayerTokens = subtractGems (playerTokens player) gems
                  newBankTokens = addGems (boardBank (gsBoard gs)) gems
                  newPlayer = player { playerTokens = newPlayerTokens }
                  newBoard = (gsBoard gs) { boardBank = newBankTokens }
                  gs' = updateCurrentPlayer gs newPlayer
                  gs'' = gs' { gsBoard = newBoard, gsTurnPhase = AwaitingAction }
              in finishTurn gs''
    _ -> StepError (OtherError "Not in gem return phase")

applyNobleChoice :: GameState -> PlayerId -> NobleId -> StepResult
applyNobleChoice gs pid nid =
  case currentPlayer gs of
    Nothing -> StepError (InvalidState "Invalid player index")
    Just player
      | playerId player /= pid -> StepError NotYourTurn
      | otherwise ->
          case filter (\n -> nobleId n == nid) (boardNobles (gsBoard gs)) of
            [] -> StepError (OtherError "Noble not found")
            (noble:_) ->
              let newPlayer = player { playerNobles = playerNobles player ++ [noble] }
                  newNobles = filter (\nb -> nobleId nb /= nobleId noble) (boardNobles (gsBoard gs))
                  newBoard = (gsBoard gs) { boardNobles = newNobles }
                  gs' = updateCurrentPlayer gs newPlayer
                  gs'' = gs' { gsBoard = newBoard }
              in advanceTurn gs''

finishTurn :: GameState -> StepResult
finishTurn gs =
  case currentPlayer gs of
    Nothing -> StepError (InvalidState "Invalid player index")
    Just player ->
      let nobles = checkNobleVisit player (boardNobles (gsBoard gs))
      in case nobles of
           []  -> advanceTurn gs
           [n] ->
             let newPlayer = player { playerNobles = playerNobles player ++ [n] }
                 newNobles = filter (\nb -> nobleId nb /= nobleId n) (boardNobles (gsBoard gs))
                 newBoard = (gsBoard gs) { boardNobles = newNobles }
                 gs' = updateCurrentPlayer gs newPlayer
                 gs'' = gs' { gsBoard = newBoard }
             in advanceTurn gs''
           multiple -> NeedNobleChoice gs multiple

advanceTurn :: GameState -> StepResult
advanceTurn gs =
  let playerCount = length (gsPlayers gs)
      nextPlayerIdx = if playerCount == 0
                      then 0
                      else (gsCurrentPlayer gs + 1) `mod` playerCount
      newTurnNumber = if nextPlayerIdx == 0
                      then gsTurnNumber gs + 1
                      else gsTurnNumber gs
      gs' = case gsPhase gs of
        InProgress
          | checkWinCondition (gsPlayers gs) -> gs { gsPhase = FinalRound }
        _ -> gs
      gs'' = gs'
        { gsCurrentPlayer = nextPlayerIdx
        , gsTurnNumber = newTurnNumber
        , gsTurnPhase = AwaitingAction
        }
  in case gsPhase gs' of
       FinalRound | nextPlayerIdx == 0 ->
         case determineWinner (gsPlayers gs'') of
           Just result -> GameOver (gs'' { gsPhase = Finished result }) result
           Nothing     -> Advanced gs''
       Finished result -> GameOver gs'' result
       _ -> Advanced gs''

updateCurrentPlayer :: GameState -> Player -> GameState
updateCurrentPlayer gs newPlayer =
  let idx = gsCurrentPlayer gs
      players = gsPlayers gs
      newPlayers = take idx players ++ [newPlayer] ++ drop (idx + 1) players
  in gs { gsPlayers = newPlayers }

-- | Resolve a card source for buying, returning the card and updated board.
resolveCardSource :: CardSource -> Player -> Board -> Either ActionError (Card, Board)
resolveCardSource (FromDisplay cid) _player board =
  case buyFromDisplay cid board of
    Just result -> Right result
    Nothing -> Left (CardNotFound cid)
resolveCardSource (FromReserve cid) player _board =
  case findCardInReserve cid player of
    Just card -> Right (card, _board)
    Nothing -> Left (CardNotFound cid)
resolveCardSource (FromTopOfDeck tier) _player board =
  case buyFromDeck tier board of
    Just result -> Right result
    Nothing -> Left (InvalidState "Empty deck")

-- | Resolve a card source for reserving, returning the card and updated board.
resolveReserveSource :: CardSource -> Board -> Either ActionError (Card, Board)
resolveReserveSource (FromDisplay cid) board =
  case buyFromDisplay cid board of
    Just result -> Right result
    Nothing -> Left (CardNotFound cid)
resolveReserveSource (FromTopOfDeck tier) board =
  case buyFromDeck tier board of
    Just result -> Right result
    Nothing -> Left (InvalidState "Empty deck")
resolveReserveSource (FromReserve _) _ =
  Left (InvalidState "Cannot reserve from reserve")

buyFromDisplay :: CardId -> Board -> Maybe (Card, Board)
buyFromDisplay cid board =
  case removeAndRefill cid (boardTier1 board) of
    Just (card, newRow) -> Just (card, board { boardTier1 = newRow })
    Nothing -> case removeAndRefill cid (boardTier2 board) of
      Just (card, newRow) -> Just (card, board { boardTier2 = newRow })
      Nothing -> case removeAndRefill cid (boardTier3 board) of
        Just (card, newRow) -> Just (card, board { boardTier3 = newRow })
        Nothing -> Nothing

removeAndRefill :: CardId -> TierRow -> Maybe (Card, TierRow)
removeAndRefill cid row =
  case removeCardFromDisplay cid row of
    Nothing -> Nothing
    Just (card, row') -> Just (card, refillDisplay row')

buyFromDeck :: Tier -> Board -> Maybe (Card, Board)
buyFromDeck tier board =
  let row = getTierRow tier board
  in case tierDeck row of
       (card:rest) ->
         let newRow = row { tierDeck = rest }
         in Just (card, setTierRow tier newRow board)
       [] -> Nothing

