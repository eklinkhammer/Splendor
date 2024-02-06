module Action (
  Action (..)
) where

data Action = Purchase Card | Reserve Card | Take Gems deriving (Eq, Show)
