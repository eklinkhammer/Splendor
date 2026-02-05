module Splendor.Core.Types.Player
  ( PlayerId
  , Player(..)
  , playerBonuses
  , playerPrestige
  , playerTokenCount
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Splendor.Core.Types.Card (Card(..))
import Splendor.Core.Types.Gem (GemCollection, GemColor, totalGems)
import Splendor.Core.Types.Noble (Noble(..))

type PlayerId = Text

data Player = Player
  { playerId        :: PlayerId
  , playerName      :: Text
  , playerTokens    :: GemCollection
  , playerPurchased :: [Card]
  , playerReserved  :: [Card]
  , playerNobles    :: [Noble]
  } deriving stock (Eq, Show, Read, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Count card bonuses by gem color
playerBonuses :: Player -> Map GemColor Int
playerBonuses p =
  Map.fromListWith (+) [ (cardBonus c, 1) | c <- playerPurchased p ]

-- | Total prestige from cards and nobles
playerPrestige :: Player -> Int
playerPrestige p =
  sum (map cardPrestige (playerPurchased p))
  + sum (map noblePrestige (playerNobles p))

-- | Total number of tokens held
playerTokenCount :: Player -> Int
playerTokenCount = totalGems . playerTokens
