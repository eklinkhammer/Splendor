module Splendor.Core.TestHelpers
  ( mkGems
  , mkCost
  , mkCard
  , mkNoble
  , emptyPlayer
  , playerWithTokens
  , playerWithCards
  , mkTierRow
  , mkGameState
  , twoPlayerBank
  ) where

import Data.Map.Strict qualified as Map
import Splendor.Core.Types

-- | Build a GemCollection from a list of (TokenType, Int) pairs
mkGems :: [(TokenType, Int)] -> GemCollection
mkGems = GemCollection . Map.fromList . filter (\(_, n) -> n > 0)

-- | Build a cost GemCollection from a list of (GemColor, Int) pairs
mkCost :: [(GemColor, Int)] -> GemCollection
mkCost pairs = GemCollection . Map.fromList $
  [ (GemToken c, n) | (c, n) <- pairs, n > 0 ]

-- | Build a test Card with given attributes
mkCard :: CardId -> Tier -> [(GemColor, Int)] -> GemColor -> Int -> Card
mkCard cid tier costPairs bonus prestige = Card
  { cardId       = cid
  , cardTier     = tier
  , cardCost     = mkCost costPairs
  , cardBonus    = bonus
  , cardPrestige = prestige
  }

-- | Build a test Noble with given requirements
mkNoble :: NobleId -> [(GemColor, Int)] -> Noble
mkNoble nid reqs = Noble
  { nobleId          = nid
  , nobleRequirement = Map.fromList reqs
  , noblePrestige    = 3
  }

-- | An empty player with no tokens, cards, or nobles
emptyPlayer :: PlayerId -> Player
emptyPlayer pid = Player
  { playerId        = pid
  , playerName      = pid
  , playerTokens    = emptyGems
  , playerPurchased = []
  , playerReserved  = []
  , playerNobles    = []
  }

-- | A player with specific tokens
playerWithTokens :: PlayerId -> GemCollection -> Player
playerWithTokens pid tokens = (emptyPlayer pid) { playerTokens = tokens }

-- | A player with specific purchased cards
playerWithCards :: PlayerId -> [Card] -> Player
playerWithCards pid cards = (emptyPlayer pid) { playerPurchased = cards }

-- | Build a TierRow from deck and display lists
mkTierRow :: [Card] -> [Card] -> TierRow
mkTierRow deck display = TierRow
  { tierDeck    = deck
  , tierDisplay = display
  }

-- | Build a minimal GameState for testing
mkGameState :: [Player] -> Board -> GameState
mkGameState players board = GameState
  { gsGameId        = "test-game"
  , gsPlayers       = players
  , gsBoard         = board
  , gsCurrentPlayer = 0
  , gsTurnNumber    = 1
  , gsPhase         = InProgress
  , gsTurnPhase     = AwaitingAction
  }

-- | Standard 2-player bank: 4 per gem color, 5 gold
twoPlayerBank :: GemCollection
twoPlayerBank = mkGems $
  [ (GemToken c, 4) | c <- allGemColors ]
  ++ [(GoldToken, 5)]
