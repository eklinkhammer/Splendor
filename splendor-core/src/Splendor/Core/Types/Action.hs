module Splendor.Core.Types.Action
  ( GemTake(..)
  , CardSource(..)
  , Action(..)
  , ActionError(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Splendor.Core.Types.Card (CardId, Tier)
import Splendor.Core.Types.Gem (GemColor, GemCollection)

data GemTake
  = TakeDifferent [GemColor]   -- ^ Take 1, 2, or 3 different colors (1 each)
  | TakeTwoSame GemColor       -- ^ Take 2 of the same color (requires >= 4 in bank)
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardSource
  = FromDisplay CardId
  | FromReserve CardId
  | FromTopOfDeck Tier
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Action
  = TakeGems GemTake
  | BuyCard CardSource GemCollection   -- ^ Card source + payment
  | ReserveCard CardSource
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActionError
  = NotYourTurn
  | InvalidGemTake Text
  | CardNotFound CardId
  | CannotAfford CardId
  | ReserveLimit
  | InvalidPayment Text
  | TokenLimit
  | GemReturnRequired
  | GameNotInProgress
  | InvalidState Text
  | OtherError Text
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)
