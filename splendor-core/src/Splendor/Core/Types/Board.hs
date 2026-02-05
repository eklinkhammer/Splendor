module Splendor.Core.Types.Board
  ( TierRow(..)
  , Board(..)
  , getTierRow
  , setTierRow
  , PublicTierRow(..)
  , PublicBoard(..)
  , toPublicBoard
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Splendor.Core.Types.Card (Card, Tier(..))
import Splendor.Core.Types.Gem (GemCollection)
import Splendor.Core.Types.Noble (Noble)

data TierRow = TierRow
  { tierDeck    :: [Card]    -- ^ Hidden deck (not visible to players)
  , tierDisplay :: [Card]    -- ^ Face-up cards (max 4)
  } deriving stock (Eq, Show, Read, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Board = Board
  { boardTier1  :: TierRow
  , boardTier2  :: TierRow
  , boardTier3  :: TierRow
  , boardNobles :: [Noble]
  , boardBank   :: GemCollection
  } deriving stock (Eq, Show, Read, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Access a tier row by tier
getTierRow :: Tier -> Board -> TierRow
getTierRow Tier1 = boardTier1
getTierRow Tier2 = boardTier2
getTierRow Tier3 = boardTier3

-- | Set a tier row by tier
setTierRow :: Tier -> TierRow -> Board -> Board
setTierRow Tier1 row board = board { boardTier1 = row }
setTierRow Tier2 row board = board { boardTier2 = row }
setTierRow Tier3 row board = board { boardTier3 = row }

-- | Client-safe tier row view (hides deck contents, shows only count)
data PublicTierRow = PublicTierRow
  { publicDeckCount :: Int     -- ^ Number of cards remaining in deck
  , publicDisplay   :: [Card]  -- ^ Face-up cards
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Client-safe board view (hides all deck contents)
data PublicBoard = PublicBoard
  { publicTier1  :: PublicTierRow
  , publicTier2  :: PublicTierRow
  , publicTier3  :: PublicTierRow
  , publicNobles :: [Noble]
  , publicBank   :: GemCollection
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Convert a full board to a client-safe public view
toPublicBoard :: Board -> PublicBoard
toPublicBoard board = PublicBoard
  { publicTier1  = toPublicTierRow (boardTier1 board)
  , publicTier2  = toPublicTierRow (boardTier2 board)
  , publicTier3  = toPublicTierRow (boardTier3 board)
  , publicNobles = boardNobles board
  , publicBank   = boardBank board
  }

toPublicTierRow :: TierRow -> PublicTierRow
toPublicTierRow row = PublicTierRow
  { publicDeckCount = length (tierDeck row)
  , publicDisplay   = tierDisplay row
  }
