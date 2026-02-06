module Splendor.Server.TypesSpec (spec) where

import Control.Concurrent.STM (readTVarIO)
import Data.Aeson (encode, decode)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import Test.Hspec
import Splendor.Core.Types
import Splendor.Server.Types

spec :: Spec
spec = do
  describe "toPublicGameView" $ do
    let card1 = Card "c1" Tier1 (GemCollection $ Map.fromList [(GemToken Ruby, 2)]) Diamond 1
        card2 = Card "c2" Tier2 (GemCollection $ Map.fromList [(GemToken Diamond, 3)]) Sapphire 2
        reserved1 = Card "r1" Tier1 emptyGems Ruby 0
        reserved2 = Card "r2" Tier2 emptyGems Onyx 0
        noble1 = Noble "n1" (Map.fromList [(Diamond, 3)]) 3
        tokens1 = GemCollection $ Map.fromList [(GemToken Ruby, 3), (GoldToken, 1)]
        tokens2 = GemCollection $ Map.fromList [(GemToken Diamond, 2)]
        p1 = Player
          { playerId = "p1"
          , playerName = "Alice"
          , playerTokens = tokens1
          , playerPurchased = [card1]
          , playerReserved = [reserved1]
          , playerNobles = [noble1]
          }
        p2 = Player
          { playerId = "p2"
          , playerName = "Bob"
          , playerTokens = tokens2
          , playerPurchased = [card2]
          , playerReserved = [reserved2]
          , playerNobles = []
          }
        deckCard = Card "deck1" Tier1 emptyGems Emerald 0
        board = Board
          { boardTier1  = TierRow [deckCard] [card1]
          , boardTier2  = TierRow [] [card2]
          , boardTier3  = TierRow [] []
          , boardNobles = [noble1]
          , boardBank   = GemCollection $ Map.fromList [(GemToken Ruby, 4)]
          }
        gs = GameState
          { gsGameId        = "game-1"
          , gsPlayers       = [p1, p2]
          , gsBoard         = board
          , gsCurrentPlayer = 0
          , gsTurnNumber    = 5
          , gsPhase         = InProgress
          , gsTurnPhase     = AwaitingAction
          }

    it "viewer sees own reserved cards as Just [cards]" $ do
      let view = toPublicGameView "p1" gs
          pp1 = pgvPlayers view !! 0
      ppReserved pp1 `shouldBe` Just [reserved1]

    it "viewer sees opponents' reserved as Nothing" $ do
      let view = toPublicGameView "p1" gs
          pp2 = pgvPlayers view !! 1
      ppReserved pp2 `shouldBe` Nothing

    it "reserved count is correct for self" $ do
      let view = toPublicGameView "p1" gs
          pp1 = pgvPlayers view !! 0
      ppReservedCount pp1 `shouldBe` 1

    it "reserved count is correct for opponent" $ do
      let view = toPublicGameView "p1" gs
          pp2 = pgvPlayers view !! 1
      ppReservedCount pp2 `shouldBe` 1

    it "player names are preserved" $ do
      let view = toPublicGameView "p1" gs
      map ppPlayerName (pgvPlayers view) `shouldBe` ["Alice", "Bob"]

    it "player tokens are preserved" $ do
      let view = toPublicGameView "p1" gs
          pp1 = pgvPlayers view !! 0
      ppTokens pp1 `shouldBe` tokens1

    it "purchased cards are preserved" $ do
      let view = toPublicGameView "p1" gs
          pp1 = pgvPlayers view !! 0
      ppPurchased pp1 `shouldBe` [card1]

    it "player nobles are preserved" $ do
      let view = toPublicGameView "p1" gs
          pp1 = pgvPlayers view !! 0
      ppNobles pp1 `shouldBe` [noble1]

    it "prestige is calculated" $ do
      let view = toPublicGameView "p1" gs
          pp1 = pgvPlayers view !! 0
      -- card1 prestige (1) + noble1 prestige (3) = 4
      ppPrestige pp1 `shouldBe` 4

    it "board is converted to PublicBoard (deck counts, not cards)" $ do
      let view = toPublicGameView "p1" gs
          pb = pgvBoard view
      publicDeckCount (publicTier1 pb) `shouldBe` 1
      publicDisplay (publicTier1 pb) `shouldBe` [card1]
      publicDeckCount (publicTier2 pb) `shouldBe` 0
      publicDeckCount (publicTier3 pb) `shouldBe` 0

    it "game metadata (currentPlayer, turnNumber, phase, turnPhase) preserved" $ do
      let view = toPublicGameView "p1" gs
      pgvCurrentPlayer view `shouldBe` 0
      pgvTurnNumber view `shouldBe` 5
      pgvPhase view `shouldBe` InProgress
      pgvTurnPhase view `shouldBe` AwaitingAction

    it "opponent viewing sees their own reserved" $ do
      let view = toPublicGameView "p2" gs
          pp2 = pgvPlayers view !! 1
          pp1 = pgvPlayers view !! 0
      ppReserved pp2 `shouldBe` Just [reserved2]
      ppReserved pp1 `shouldBe` Nothing

  describe "JSON round-trips" $ do
    describe "ClientMessage" $ do
      it "SubmitAction round-trips" $ do
        let msg = SubmitAction (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
        decode (encode msg) `shouldBe` Just msg

      it "ReturnGems round-trips" $ do
        let msg = ReturnGems (singleGem (GemToken Ruby) 1)
        decode (encode msg) `shouldBe` Just msg

      it "ChooseNoble round-trips" $ do
        let msg = ChooseNoble "n1"
        decode (encode msg) `shouldBe` Just msg

      it "Ping round-trips" $ do
        decode (encode Ping) `shouldBe` Just Ping

    describe "CreateLobbyRequest / JoinLobbyRequest" $ do
      it "CreateLobbyRequest round-trips" $ do
        let req = CreateLobbyRequest "Alice" "My Lobby"
        decode (encode req) `shouldBe` Just req

      it "JoinLobbyRequest round-trips" $ do
        let req = JoinLobbyRequest "Bob"
        decode (encode req) `shouldBe` Just req

    describe "Lobby types" $ do
      it "LobbySlot round-trips" $ do
        let slot = LobbySlot "s1" "Alice" False
        decode (encode slot) `shouldBe` Just slot

      it "LobbyStatus Waiting round-trips" $ do
        decode (encode Waiting) `shouldBe` Just Waiting

      it "LobbyStatus Starting round-trips" $ do
        decode (encode Starting) `shouldBe` Just Starting

      it "LobbyStatus Started round-trips" $ do
        let status = Started "game-123"
        decode (encode status) `shouldBe` Just status

      it "LobbyStatus Closed round-trips" $ do
        decode (encode Closed) `shouldBe` Just Closed

      it "Lobby round-trips" $ do
        let lobby = Lobby
              { lobbyId = "lid1"
              , lobbyName = "Test Lobby"
              , lobbySlots = [LobbySlot "s1" "Alice" False]
              , lobbyMaxPlayers = 4
              , lobbyMinPlayers = 2
              , lobbyStatus = Waiting
              , lobbyCreatedAt = UTCTime (fromGregorian 2025 1 1) 0
              }
        decode (encode lobby) `shouldBe` Just lobby

  describe "newServerState" $ do
    it "initializes all TVars to empty maps" $ do
      ss <- newServerState
      lobbies <- readTVarIO (ssLobbies ss)
      games <- readTVarIO (ssGames ss)
      sessions <- readTVarIO (ssSessions ss)
      lobbies `shouldBe` Map.empty
      Map.null games `shouldBe` True
      sessions `shouldBe` Map.empty

