module Splendor.Core.Types.GameState
  ( GamePhase(..)
  , GameResult(..)
  , TurnPhase(..)
  , GameState(..)
  , currentPlayer
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Splendor.Core.Types.Board (Board)
import Splendor.Core.Types.Player (Player, PlayerId)

data GameResult = GameResult
  { winnerId      :: PlayerId
  , winnerName    :: Text
  , finalPrestige :: Int
  } deriving stock (Eq, Show, Read, Generic)
    deriving anyclass (ToJSON, FromJSON)

data GamePhase
  = InProgress
  | FinalRound
  | Finished GameResult
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TurnPhase
  = AwaitingAction
  | MustReturnGems Int  -- ^ Number of gems that must be returned
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data GameState = GameState
  { gsGameId        :: Text
  , gsPlayers       :: [Player]
  , gsBoard         :: Board
  , gsCurrentPlayer :: Int      -- ^ Index into gsPlayers
  , gsTurnNumber    :: Int
  , gsPhase         :: GamePhase
  , gsTurnPhase     :: TurnPhase
  } deriving stock (Eq, Show, Read, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Get the current active player (safe indexing, returns Maybe)
currentPlayer :: GameState -> Maybe Player
currentPlayer gs =
  let idx = gsCurrentPlayer gs
      ps  = gsPlayers gs
  in if idx >= 0 && idx < length ps
     then Just (ps !! idx)
     else Nothing
