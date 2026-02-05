module Splendor.Core.Rules.CardLogic
  ( findCardInDisplay
  , findCardInReserve
  , removeCardFromDisplay
  , refillDisplay
  , canReserve
  , maxReserved
  ) where

import Splendor.Core.Types.Card
import Splendor.Core.Types.Board
import Splendor.Core.Types.Player

-- | Maximum number of reserved cards per player
maxReserved :: Int
maxReserved = 3

-- | Check if player can reserve (< 3 reserved)
canReserve :: Player -> Bool
canReserve p = length (playerReserved p) < maxReserved

-- | Find a card by ID in a tier's display
findCardInDisplay :: CardId -> TierRow -> Maybe Card
findCardInDisplay cid row =
  case filter (\c -> cardId c == cid) (tierDisplay row) of
    [c] -> Just c
    _   -> Nothing

-- | Find a card by ID in player's reserved cards
findCardInReserve :: CardId -> Player -> Maybe Card
findCardInReserve cid p =
  case filter (\c -> cardId c == cid) (playerReserved p) of
    [c] -> Just c
    _   -> Nothing

-- | Remove a card from the display, return updated TierRow and the card
removeCardFromDisplay :: CardId -> TierRow -> Maybe (Card, TierRow)
removeCardFromDisplay cid row =
  case findCardInDisplay cid row of
    Nothing -> Nothing
    Just card ->
      let newDisplay = filter (\c -> cardId c /= cid) (tierDisplay row)
      in Just (card, row { tierDisplay = newDisplay })

-- | Refill display from deck after a card is taken (up to 4 visible)
refillDisplay :: TierRow -> TierRow
refillDisplay row
  | length (tierDisplay row) >= 4 = row
  | null (tierDeck row)           = row
  | otherwise =
      case tierDeck row of
        [] -> row  -- should not happen given the null check above, but safe
        (card:rest) -> refillDisplay $ row { tierDeck = rest, tierDisplay = tierDisplay row ++ [card] }
