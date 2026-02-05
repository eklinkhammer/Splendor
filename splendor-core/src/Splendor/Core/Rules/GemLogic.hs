module Splendor.Core.Rules.GemLogic
  ( effectiveCost
  , computePayment
  , gemsFromTake
  , bankGemsForPlayerCount
  ) where

import Data.Map.Strict qualified as Map
import Splendor.Core.Types.Gem
import Splendor.Core.Types.Action (GemTake(..))

-- | Reduce card cost by player's bonuses, minimum 0 per color
effectiveCost :: GemCollection -> Map.Map GemColor Int -> GemCollection
effectiveCost (GemCollection costMap) bonuses =
  GemCollection $ Map.mapWithKey reduce costMap
  where
    reduce (GemToken color) n =
      max 0 (n - Map.findWithDefault 0 color bonuses)
    reduce GoldToken n = n

-- | Compute a valid payment from player tokens for a given effective cost.
--   Uses colored gems first, then gold to cover shortfalls.
--   Returns Nothing if the player cannot afford it.
computePayment :: GemCollection -> GemCollection -> Maybe GemCollection
computePayment playerTokens effCost =
  let costList = Map.toList (unGemCollection effCost)
      goldAvailable = gemCount playerTokens GoldToken
      -- For each color, pay as much as possible with that color, track gold needed
      go [] goldUsed = Just (goldUsed, [])
      go ((tokenType, needed):rest) goldUsed =
        let available = gemCount playerTokens tokenType
            colorPay = min available needed
            shortfall = needed - colorPay
            newGoldUsed = goldUsed + shortfall
        in if newGoldUsed > goldAvailable
           then Nothing
           else case go rest newGoldUsed of
             Nothing -> Nothing
             Just (finalGold, payments) ->
               Just (finalGold, (tokenType, colorPay) : payments)
  in case go costList 0 of
       Nothing -> Nothing
       Just (goldUsed, payments) ->
         let paymentMap = Map.fromList $ filter (\(_, n) -> n > 0) $
               (GoldToken, goldUsed) : payments
         in Just (GemCollection paymentMap)

-- | Convert a GemTake action to the gems that would be taken
gemsFromTake :: GemTake -> GemCollection
gemsFromTake (TakeDifferent colors) =
  GemCollection $ Map.fromList [ (GemToken c, 1) | c <- colors ]
gemsFromTake (TakeTwoSame c) =
  singleGem (GemToken c) 2

-- | Initial bank gem counts per color based on player count
bankGemsForPlayerCount :: Int -> Int
bankGemsForPlayerCount 2 = 4
bankGemsForPlayerCount 3 = 5
bankGemsForPlayerCount _ = 7  -- 4 players
