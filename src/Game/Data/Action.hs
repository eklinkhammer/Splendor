module Game.Data.Action (
  Action (..)
) where

import Game.Data.Card (Card)
import Game.Data.Gems (Gems)

data Action = Purchase Card | Reserve Card | Take Gems deriving (Eq, Show)
