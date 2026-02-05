module Splendor.Core.Types.Card
  ( Tier(..)
  , CardId
  , Card(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Splendor.Core.Types.Gem (GemColor, GemCollection)

data Tier = Tier1 | Tier2 | Tier3
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

type CardId = Text

data Card = Card
  { cardId       :: CardId
  , cardTier     :: Tier
  , cardCost     :: GemCollection
  , cardBonus    :: GemColor
  , cardPrestige :: Int
  } deriving stock (Eq, Show, Read, Generic)
    deriving anyclass (ToJSON, FromJSON)
