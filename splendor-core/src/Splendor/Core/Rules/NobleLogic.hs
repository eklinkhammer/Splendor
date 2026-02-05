module Splendor.Core.Rules.NobleLogic
  ( eligibleNobles
  , checkNobleVisit
  ) where

import Data.Map.Strict qualified as Map
import Splendor.Core.Types.Gem (GemColor)
import Splendor.Core.Types.Noble (Noble(..))
import Splendor.Core.Types.Player (Player, playerBonuses)

-- | Check if a player's bonuses meet a noble's requirement
meetsRequirement :: Map.Map GemColor Int -> Noble -> Bool
meetsRequirement bonuses noble =
  all (\(color, needed) -> Map.findWithDefault 0 color bonuses >= needed)
      (Map.toList (nobleRequirement noble))

-- | Find all nobles that the player is eligible to receive
eligibleNobles :: Player -> [Noble] -> [Noble]
eligibleNobles player = filter (meetsRequirement (playerBonuses player))

-- | After a player's action, check for noble visits.
--   Returns list of eligible nobles (player chooses if multiple).
checkNobleVisit :: Player -> [Noble] -> [Noble]
checkNobleVisit = eligibleNobles
