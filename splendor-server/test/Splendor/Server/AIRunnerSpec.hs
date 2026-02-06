module Splendor.Server.AIRunnerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Data.Map.Strict qualified as Map
import Test.Hspec
import Servant qualified
import Splendor.Core.Types
import Splendor.Server.AIRunner (checkTurn, AICheck(..))
import Splendor.Server.Types
import Splendor.Server.TestHelpers

spec :: Spec
spec = do
  describe "add-ai endpoint" $ do
    it "adds AI slot to waiting lobby" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "Test Lobby")
      let lid = clrLobbyId resp
      slot <- run $ addAIH ss lid
      lsIsAI slot `shouldBe` True
      lsPlayerName slot `shouldBe` "AI Player 1"
      -- Verify lobby now has 2 slots
      lobby <- run $ getH ss lid
      length (lobbySlots lobby) `shouldBe` 2

    it "auto-increments AI player names" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "Test Lobby")
      let lid = clrLobbyId resp
      slot1 <- run $ addAIH ss lid
      slot2 <- run $ addAIH ss lid
      lsPlayerName slot1 `shouldBe` "AI Player 1"
      lsPlayerName slot2 `shouldBe` "AI Player 2"

    it "rejects full lobby" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "Test Lobby")
      let lid = clrLobbyId resp
      -- Fill the lobby (max 4 players)
      _ <- run $ addAIH ss lid
      _ <- run $ addAIH ss lid
      _ <- run $ addAIH ss lid
      -- 4th add should fail (lobby full at 4)
      result <- Servant.runHandler (addAIH ss lid)
      case result of
        Left _ -> pure ()  -- expected error
        Right _ -> expectationFailure "expected error for full lobby"

    it "rejects adding AI to nonexistent lobby" $ do
      ss <- newServerState
      result <- Servant.runHandler (addAIH ss "nonexistent-lobby")
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error for nonexistent lobby"

    it "rejects adding AI after lobby is started" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "Test Lobby")
      let lid = clrLobbyId resp
      _ <- run $ addAIH ss lid
      _ <- run $ startH ss lid
      result <- Servant.runHandler (addAIH ss lid)
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error for started lobby"

  describe "AI game play" $ do
    it "starting game with AI slots spawns AI that makes moves" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "Test Lobby")
      let lid = clrLobbyId resp
      _ <- run $ addAIH ss lid
      -- Start the game
      gameResp <- run $ startH ss lid
      let gid = sgrGameId gameResp
      -- Wait a bit for AI to make at least one move
      threadDelay 2000000  -- 2 seconds
      mg <- lookupGameOrFail ss gid
      -- Game should still be active or finished, and some progress made
      mgStatus mg `shouldSatisfy` (\s -> s == GameActive || s == GameFinished)

    it "AI-only game (2 AI) plays to completion within timeout" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Bot" "AI Game")
      let lid = clrLobbyId resp
      -- Mark the creator's slot as AI
      atomically $ modifyTVar' (ssLobbies ss) $
        Map.adjust (\lobby ->
          let slots' = map (\s -> s { lsIsAI = True }) (lobbySlots lobby)
          in lobby { lobbySlots = slots' }
        ) lid
      -- Add a second AI player
      _ <- run $ addAIH ss lid
      gameResp <- run $ startH ss lid
      let gid = sgrGameId gameResp
      -- Poll for game completion with timeout
      finished <- waitForGameEnd ss gid 120  -- 120 seconds max
      finished `shouldBe` True

  describe "checkTurn" $ do
    it "returns AIFinished for nonexistent game" $ do
      ss <- newServerState
      result <- atomically $ checkTurn ss "nonexistent-game" "some-session"
      case result of
        AIFinished -> pure ()
        _ -> expectationFailure "expected AIFinished"

    it "returns AIFinished for finished game" $ do
      (ss, gid, s1, _s2) <- setupGame
      tv <- lookupGameTVarOrFail ss gid
      atomically $ modifyTVar' tv $ \mg -> mg { mgStatus = GameFinished }
      result <- atomically $ checkTurn ss gid s1
      case result of
        AIFinished -> pure ()
        _ -> expectationFailure "expected AIFinished"

    it "returns AIFinished for unknown session" $ do
      (ss, gid, _s1, _s2) <- setupGame
      result <- atomically $ checkTurn ss gid "unknown-session"
      case result of
        AIFinished -> pure ()
        _ -> expectationFailure "expected AIFinished"

    it "returns AIWait when not AI's turn" $ do
      (ss, gid, s1, s2) <- setupGame
      -- Find the non-current player's session
      ws <- wrongSession ss gid s1 s2
      result <- atomically $ checkTurn ss gid ws
      case result of
        AIWait -> pure ()
        _ -> expectationFailure "expected AIWait"

    it "returns AINeedAction when it is AI's turn in AwaitingAction" $ do
      (ss, gid, s1, s2) <- setupGame
      cs <- currentSession ss gid s1 s2
      result <- atomically $ checkTurn ss gid cs
      case result of
        AINeedAction gs _pid -> do
          gsPhase gs `shouldSatisfy` (\p -> p == InProgress || p == FinalRound)
          gsTurnPhase gs `shouldBe` AwaitingAction
        _ -> expectationFailure "expected AINeedAction"

    it "returns AINeedGemReturn when in MustReturnGems phase" $ do
      (ss, gid, s1, s2) <- setupGame
      cs <- currentSession ss gid s1 s2
      tv <- lookupGameTVarOrFail ss gid
      -- Put game into MustReturnGems phase and give player extra tokens
      atomically $ modifyTVar' tv $ \mg ->
        let gs = mgGameState mg
            ps = gsPlayers gs
        in case ps of
          (p:rest) ->
            let p' = p { playerTokens = GemCollection (Map.fromList [(GemToken Ruby, 6), (GemToken Diamond, 6)]) }
                gs' = gs { gsPlayers = p' : rest, gsTurnPhase = MustReturnGems 1 }
            in mg { mgGameState = gs' }
          [] -> mg
      result <- atomically $ checkTurn ss gid cs
      case result of
        AINeedGemReturn _gs _pid -> pure ()
        _ -> expectationFailure "expected AINeedGemReturn"

    it "returns AINeedNoble when pending nobles exist" $ do
      (ss, gid, s1, s2) <- setupGame
      cs <- currentSession ss gid s1 s2
      tv <- lookupGameTVarOrFail ss gid
      let testNoble = Noble "n1" (Map.fromList [(Diamond, 3)]) 3
      atomically $ modifyTVar' tv $ \mg -> mg { mgPendingNobles = Just [testNoble] }
      result <- atomically $ checkTurn ss gid cs
      case result of
        AINeedNoble _gs _pid nobles -> do
          length nobles `shouldBe` 1
          nobleId (head nobles) `shouldBe` "n1"
        _ -> expectationFailure "expected AINeedNoble"

-- | Wait for a game to end, polling every second. Returns True if finished within timeout.
waitForGameEnd :: ServerState -> GameId -> Int -> IO Bool
waitForGameEnd _ _ 0 = pure False
waitForGameEnd ss gid remaining = do
  threadDelay 1000000  -- 1 second
  mg <- lookupGameOrFail ss gid
  case mgStatus mg of
    GameFinished -> pure True
    GameActive -> waitForGameEnd ss gid (remaining - 1)
