module Splendor.Core.Types.Noble
  ( NobleId
  , Noble(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Splendor.Core.Types.Gem (GemColor)

type NobleId = Text

data Noble = Noble
  { nobleId          :: NobleId
  , nobleRequirement :: Map GemColor Int
  , noblePrestige    :: Int
  } deriving stock (Eq, Show, Read, Generic)
    deriving anyclass (ToJSON, FromJSON)
