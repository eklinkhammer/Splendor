module Splendor.Core.Setup
  ( initGameState
  ) where

import Control.Monad.ST (runST)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import System.Random (StdGen, uniformR)
import Splendor.Core.Types
import Splendor.Core.CardData
import Splendor.Core.Rules.GemLogic (bankGemsForPlayerCount)

-- | Initialize a new game state (pure, takes RNG)
initGameState :: StdGen -> Text -> Int -> [Text] -> GameState
initGameState gen gameId playerCount playerNames =
  let (shuffledTier1, gen1) = shuffle gen tier1Cards
      (shuffledTier2, gen2) = shuffle gen1 tier2Cards
      (shuffledTier3, gen3) = shuffle gen2 tier3Cards
      (selectedNobles, _gen4) = takeRandom gen3 (playerCount + 1) allNobles

      (display1, deck1) = splitAt 4 shuffledTier1
      (display2, deck2) = splitAt 4 shuffledTier2
      (display3, deck3) = splitAt 4 shuffledTier3

      gemsPerColor = bankGemsForPlayerCount playerCount
      bankMap = Map.fromList $
        [ (GemToken c, gemsPerColor) | c <- allGemColors ]
        ++ [(GoldToken, 5)]

      players = zipWith mkPlayer [1..] playerNames
  in GameState
    { gsGameId = gameId
    , gsPlayers = players
    , gsBoard = Board
        { boardTier1 = TierRow deck1 display1
        , boardTier2 = TierRow deck2 display2
        , boardTier3 = TierRow deck3 display3
        , boardNobles = selectedNobles
        , boardBank = GemCollection bankMap
        }
    , gsCurrentPlayer = 0
    , gsTurnNumber = 1
    , gsPhase = InProgress
    , gsTurnPhase = AwaitingAction
    }

mkPlayer :: Int -> Text -> Player
mkPlayer n name = Player
  { playerId = "player-" <> T.pack (show n)
  , playerName = name
  , playerTokens = emptyGems
  , playerPurchased = []
  , playerReserved = []
  , playerNobles = []
  }

-- | Fisher-Yates shuffle using mutable vector (pure, O(n))
shuffle :: StdGen -> [a] -> ([a], StdGen)
shuffle gen [] = ([], gen)
shuffle gen [x] = ([x], gen)
shuffle gen xs = runST $ do
    let n = length xs
    v <- V.thaw (V.fromList xs)
    gen' <- go gen v (n - 1)
    result <- V.freeze v
    pure (V.toList result, gen')
  where
    go g _ 0 = pure g
    go g v i = do
      let (j, g') = uniformR (0, i) g
      VM.swap v i j
      go g' v (i - 1)

-- | Take n random elements from a list
takeRandom :: StdGen -> Int -> [a] -> ([a], StdGen)
takeRandom gen n xs =
  let (shuffled, gen') = shuffle gen xs
  in (take n shuffled, gen')
