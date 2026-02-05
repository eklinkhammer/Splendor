module Splendor.Core.Rules.ActionValidation
  ( validateAction
  , legalActions
  , legalGemReturns
  ) where

import Control.Applicative ((<|>))
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Splendor.Core.Types
import Splendor.Core.Rules.GemLogic
import Splendor.Core.Rules.CardLogic

validateAction :: GameState -> PlayerId -> Action -> Either ActionError Action
validateAction gs pid action = case gsPhase gs of
  Finished _ -> Left GameNotInProgress
  _ -> case gsTurnPhase gs of
    MustReturnGems _ -> Left GemReturnRequired
    AwaitingAction ->
      case currentPlayer gs of
        Nothing -> Left (InvalidState "Invalid player index")
        Just player
          | playerId player /= pid -> Left NotYourTurn
          | otherwise -> validateSpecific gs player action

validateSpecific :: GameState -> Player -> Action -> Either ActionError Action
validateSpecific gs _player action@(TakeGems gemTake) = do
  validateGemTake gs gemTake
  Right action
validateSpecific gs player action@(BuyCard source payment) = do
  validateBuyCard gs player source payment
  Right action
validateSpecific _gs player action@(ReserveCard _source) = do
  validateReserve player
  Right action

validateGemTake :: GameState -> GemTake -> Either ActionError ()
validateGemTake gs (TakeDifferent colors) = do
  let n = length colors
  if n < 1 || n > 3
    then Left (InvalidGemTake "Must pick 1 to 3 colors")
    else if length (nub colors) /= n
    then Left (InvalidGemTake "Must pick different colors")
    else do
      let bank = boardBank (gsBoard gs)
          availableColors = [ c | c <- allGemColors, gemCount bank (GemToken c) >= 1 ]
      -- Can only take fewer than 3 if fewer than 3 colors are available
      if n < 3 && length availableColors >= 3
        then Left (InvalidGemTake "Must take 3 different colors when 3+ are available")
        else
          mapM_ (\c ->
            if gemCount bank (GemToken c) < 1
            then Left (InvalidGemTake $ "Not enough " <> T.pack (show c))
            else Right ()
            ) colors
validateGemTake gs (TakeTwoSame c) = do
  let bank = boardBank (gsBoard gs)
  if gemCount bank (GemToken c) < 4
    then Left (InvalidGemTake "Need at least 4 gems of that color to take 2")
    else Right ()

validateBuyCard :: GameState -> Player -> CardSource -> GemCollection -> Either ActionError ()
validateBuyCard gs player source payment = do
  card <- findCard gs player source
  let bonuses = playerBonuses player
      effCost = effectiveCost (cardCost card) bonuses
  if not (nonNegative payment)
    then Left (InvalidPayment "Payment cannot be negative")
    else if not (hasEnoughGems (playerTokens player) payment)
    then Left (InvalidPayment "Player doesn't have these tokens")
    else if not (hasEnoughGems payment effCost)
    then Left (CannotAfford (cardId card))
    else
      -- Validate no overpayment: no token type exceeds the effective cost for that type
      let payMap = unGemCollection payment
          overpaid = any (\(tokenType, paid) ->
            let needed = gemCount effCost tokenType
            in paid > needed
            ) (Map.toList payMap)
      in if overpaid
         then Left (InvalidPayment "Overpayment: cannot pay more than the effective cost per token type")
         else Right ()

validateReserve :: Player -> Either ActionError ()
validateReserve player =
  if not (canReserve player)
    then Left ReserveLimit
    else Right ()

findCard :: GameState -> Player -> CardSource -> Either ActionError Card
findCard gs _player (FromDisplay cid) =
  let board = gsBoard gs
      result = findCardInDisplay cid (boardTier1 board)
           <|> findCardInDisplay cid (boardTier2 board)
           <|> findCardInDisplay cid (boardTier3 board)
  in case result of
       Nothing -> Left (CardNotFound cid)
       Just c  -> Right c
findCard _gs player (FromReserve cid) =
  case findCardInReserve cid player of
    Nothing -> Left (CardNotFound cid)
    Just c  -> Right c
findCard gs _player (FromTopOfDeck tier) =
  let row = getTierRow tier (gsBoard gs)
  in case tierDeck row of
       []    -> Left (CardNotFound "empty-deck")
       (c:_) -> Right c

legalActions :: GameState -> [Action]
legalActions gs = case gsTurnPhase gs of
  MustReturnGems _ -> []
  AwaitingAction ->
    case currentPlayer gs of
      Nothing -> []
      Just player ->
        let bank = boardBank (gsBoard gs)
            board = gsBoard gs
        in gemTakeActions bank ++ buyActions player board ++ reserveActions player board
  where
    gemTakeActions bank = takeDifferentActions bank ++ takeTwoActions bank

    takeDifferentActions bank =
      let availableColors = [ c | c <- allGemColors, gemCount bank (GemToken c) >= 1 ]
      in case length availableColors of
        n | n >= 3 ->
          -- Take exactly 3 different colors
          [ TakeGems (TakeDifferent [c1, c2, c3])
          | c1 <- availableColors
          , c2 <- availableColors
          , c3 <- availableColors
          , c1 < c2, c2 < c3
          ]
        n | n >= 1 ->
          -- Fewer than 3 available: take all available colors
          [TakeGems (TakeDifferent availableColors)]
        _ -> []

    takeTwoActions bank =
      [ TakeGems (TakeTwoSame c)
      | c <- allGemColors
      , gemCount bank (GemToken c) >= 4
      ]

    buyActions player board = concatMap (buyFromDisplayAction player) (allDisplayCards board)
                           ++ concatMap (buyFromReserveAction player) (playerReserved player)

    allDisplayCards board =
      tierDisplay (boardTier1 board)
      ++ tierDisplay (boardTier2 board)
      ++ tierDisplay (boardTier3 board)

    buyFromDisplayAction player card =
      case computePayment (playerTokens player) (effectiveCost (cardCost card) (playerBonuses player)) of
        Nothing -> []
        Just payment -> [BuyCard (FromDisplay (cardId card)) payment]

    buyFromReserveAction player card =
      case computePayment (playerTokens player) (effectiveCost (cardCost card) (playerBonuses player)) of
        Nothing -> []
        Just payment -> [BuyCard (FromReserve (cardId card)) payment]

    reserveActions player board
      | not (canReserve player) = []
      | otherwise = reserveFromDisplayActions board ++ reserveFromDeckActions board

    reserveFromDisplayActions board =
      [ ReserveCard (FromDisplay (cardId c))
      | c <- allDisplayCards board
      ]

    reserveFromDeckActions board =
      [ ReserveCard (FromTopOfDeck tier)
      | tier <- [Tier1, Tier2, Tier3]
      , not (null (tierDeck (getTierRow tier board)))
      ]

-- | Enumerate all valid gem return combinations when in MustReturnGems phase.
--   Returns empty list if not in gem return phase.
legalGemReturns :: GameState -> [GemCollection]
legalGemReturns gs = case gsTurnPhase gs of
  MustReturnGems n ->
    case currentPlayer gs of
      Nothing -> []
      Just player ->
        let tokens = playerTokens player
            heldTokens = [ (t, c) | (t, c) <- Map.toList (unGemCollection tokens), c > 0 ]
        in map gemCollectionFromList (combinations n heldTokens)
  _ -> []
  where
    -- Generate all ways to select exactly n gems from held tokens
    combinations :: Int -> [(TokenType, Int)] -> [[(TokenType, Int)]]
    combinations 0 _ = [[]]
    combinations _ [] = []
    combinations n ((tokenType, available):rest) =
      [ (tokenType, k) : combo
      | k <- [0 .. min n available]
      , combo <- combinations (n - k) rest
      ]

    gemCollectionFromList :: [(TokenType, Int)] -> GemCollection
    gemCollectionFromList = GemCollection . Map.fromList . filter (\(_, c) -> c > 0)
