module Game.Data.Action (
  Action (..)
) where

import Game.Data.Card (Card)
import Game.Data.Gems (Gems)
import qualified Data.Map as Map

data Action = Purchase Card | Reserve Card | Take Gems deriving (Eq, Show)

