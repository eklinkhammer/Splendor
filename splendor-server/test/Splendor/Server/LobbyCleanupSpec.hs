module Splendor.Server.LobbyCleanupSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime(..), addUTCTime, fromGregorian, secondsToDiffTime)
import Test.Hspec

import Splendor.Server.LobbyCleanup (removeStaleLobbies)
import Splendor.Server.Types (Lobby(..), LobbyStatus(..))

spec :: Spec
spec = describe "LobbyCleanup" $ do
  describe "removeStaleLobbies" $ do
    it "keeps fresh Waiting lobbies" $ do
      let lobbies = Map.singleton "lobby-1" (mkLobby "lobby-1" Waiting (minutesAgo 30))
      Map.size (removeStaleLobbies now lobbies) `shouldBe` 1

    it "removes old Waiting lobbies" $ do
      let lobbies = Map.singleton "lobby-1" (mkLobby "lobby-1" Waiting (minutesAgo 61))
      Map.size (removeStaleLobbies now lobbies) `shouldBe` 0

    it "removes Started lobbies immediately regardless of age" $ do
      let lobbies = Map.singleton "lobby-1" (mkLobby "lobby-1" (Started "game-1") (minutesAgo 5))
      Map.size (removeStaleLobbies now lobbies) `shouldBe` 0

    it "removes Closed lobbies immediately regardless of age" $ do
      let lobbies = Map.singleton "lobby-1" (mkLobby "lobby-1" Closed (minutesAgo 5))
      Map.size (removeStaleLobbies now lobbies) `shouldBe` 0

    it "never removes Starting lobbies regardless of age" $ do
      let lobbies = Map.singleton "lobby-1" (mkLobby "lobby-1" Starting (minutesAgo 120))
      Map.size (removeStaleLobbies now lobbies) `shouldBe` 1

    it "keeps lobbies at exactly the 60-minute boundary" $ do
      let lobbies = Map.singleton "lobby-1" (mkLobby "lobby-1" Waiting (minutesAgo 60))
      Map.size (removeStaleLobbies now lobbies) `shouldBe` 1

    it "removes lobbies just past the 60-minute boundary" $ do
      let createdAt = addUTCTime (-3601) now  -- 60 min + 1 sec ago
          lobbies = Map.singleton "lobby-1" (mkLobby "lobby-1" Waiting createdAt)
      Map.size (removeStaleLobbies now lobbies) `shouldBe` 0

    it "filters a mix of fresh, stale, and Starting lobbies" $ do
      let lobbies = Map.fromList
            [ ("fresh-waiting",  mkLobby "fresh-waiting"  Waiting          (minutesAgo 10))
            , ("old-waiting",    mkLobby "old-waiting"    Waiting          (minutesAgo 90))
            , ("old-started",    mkLobby "old-started"    (Started "g-1")  (minutesAgo 75))
            , ("old-starting",   mkLobby "old-starting"   Starting         (minutesAgo 120))
            , ("fresh-closed",   mkLobby "fresh-closed"   Closed           (minutesAgo 5))
            ]
          result = removeStaleLobbies now lobbies
      Map.size result `shouldBe` 2
      Map.member "fresh-waiting" result `shouldBe` True
      Map.member "old-starting"  result `shouldBe` True
      Map.member "old-waiting"   result `shouldBe` False
      Map.member "old-started"   result `shouldBe` False
      Map.member "fresh-closed"  result `shouldBe` False

    it "returns empty map when all lobbies are stale" $ do
      let lobbies = Map.fromList
            [ ("a", mkLobby "a" Waiting (minutesAgo 120))
            , ("b", mkLobby "b" Closed  (minutesAgo 90))
            ]
      removeStaleLobbies now lobbies `shouldBe` Map.empty

    it "handles empty lobby map" $ do
      removeStaleLobbies now Map.empty `shouldBe` Map.empty

-- | Fixed reference time for all tests.
now :: UTCTime
now = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 43200)  -- noon

-- | Create a UTCTime that is N minutes before 'now'.
minutesAgo :: NominalDiffTime -> UTCTime
minutesAgo mins = addUTCTime (negate (mins * 60)) now

-- | Build a minimal Lobby with the given id, status, and creation time.
mkLobby :: Text -> LobbyStatus -> UTCTime -> Lobby
mkLobby lid status createdAt = Lobby
  { lobbyId         = lid
  , lobbyName       = "Test Lobby"
  , lobbySlots      = []
  , lobbyMaxPlayers = 4
  , lobbyMinPlayers = 2
  , lobbyStatus     = status
  , lobbyCreatedAt  = createdAt
  }
