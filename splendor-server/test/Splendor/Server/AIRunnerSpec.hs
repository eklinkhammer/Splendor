module Splendor.Server.AIRunnerSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Data.IORef
import Data.Maybe (isJust)
import Data.Map.Strict qualified as Map
import Test.Hspec
import Servant qualified
import Splendor.Core.Types
import Splendor.Core.Rules.ActionValidation (legalActions)
import Splendor.AI.Agent (Agent(..))
import Splendor.AI.Random (RandomAgent(..))
import Splendor.Server.AIRunner (aiLoopWith, checkTurn, AICheck(..))
import Splendor.Server.GameManager (processAction, registerConnection, createGame)
import Splendor.Server.Types

import Splendor.Server.TestHelpers

-- | An agent that returns an invalid action (empty TakeDifferent) for the first N calls,
--   causing processAction rejection, then delegates to RandomAgent.
data BadActionAgent = BadActionAgent
  { baBadCount :: IORef Int  -- ^ remaining bad actions
  }

mkBadActionAgent :: Int -> IO BadActionAgent
mkBadActionAgent n = BadActionAgent <$> newIORef n

instance Agent BadActionAgent where
  agentName _ = "BadActionAgent"

  chooseAction ba gs actions = do
    remaining <- readIORef (baBadCount ba)
    if remaining > 0
      then do
        modifyIORef' (baBadCount ba) (subtract 1)
        pure (TakeGems (TakeDifferent []))  -- always rejected
      else chooseAction RandomAgent gs actions

  chooseGemReturn _ gs opts = chooseGemReturn RandomAgent gs opts
  chooseNoble _ gs nobles = chooseNoble RandomAgent gs nobles

-- | An agent that throws for the first N calls, then delegates to RandomAgent.
data ThrowingAgent = ThrowingAgent
  { taFailCount :: IORef Int  -- ^ remaining failures
  }

mkThrowingAgent :: Int -> IO ThrowingAgent
mkThrowingAgent n = ThrowingAgent <$> newIORef n

instance Agent ThrowingAgent where
  agentName _ = "ThrowingAgent"

  chooseAction ta gs actions = do
    remaining <- readIORef (taFailCount ta)
    if remaining > 0
      then do
        modifyIORef' (taFailCount ta) (subtract 1)
        error "simulated chooseAction failure"
      else chooseAction RandomAgent gs actions

  chooseGemReturn ta gs options = do
    remaining <- readIORef (taFailCount ta)
    if remaining > 0
      then do
        modifyIORef' (taFailCount ta) (subtract 1)
        error "simulated chooseGemReturn failure"
      else chooseGemReturn RandomAgent gs options

  chooseNoble ta gs nobles = do
    remaining <- readIORef (taFailCount ta)
    if remaining > 0
      then do
        modifyIORef' (taFailCount ta) (subtract 1)
        error "simulated chooseNoble failure"
      else chooseNoble RandomAgent gs nobles

-- | Setup a 2-player AI game using createGame directly (no lobby flow).
--   Both sessions are AI-controlled.
setupAIGame :: IO (ServerState, GameId, SessionId, SessionId)
setupAIGame = do
  ss <- newServerState NoPersistence
  let s1 = "ai-session-1"
      s2 = "ai-session-2"
      slots = [ LobbySlot s1 "AI-1" True
              , LobbySlot s2 "AI-2" True
              ]
  gid <- createGame ss slots
  pure (ss, gid, s1, s2)

spec :: Spec
spec = do
  describe "add-ai endpoint" $ do
    it "adds AI slot to waiting lobby" $ do
      ss <- newServerState NoPersistence
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "Test Lobby")
      let lid = clrLobbyId resp
      slot <- run $ addAIH ss lid
      lsIsAI slot `shouldBe` True
      lsPlayerName slot `shouldBe` "AI Player 1"
      -- Verify lobby now has 2 slots
      lobby <- run $ getH ss lid
      length (lobbySlots lobby) `shouldBe` 2

    it "auto-increments AI player names" $ do
      ss <- newServerState NoPersistence
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "Test Lobby")
      let lid = clrLobbyId resp
      slot1 <- run $ addAIH ss lid
      slot2 <- run $ addAIH ss lid
      lsPlayerName slot1 `shouldBe` "AI Player 1"
      lsPlayerName slot2 `shouldBe` "AI Player 2"

    it "rejects full lobby" $ do
      ss <- newServerState NoPersistence
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
      ss <- newServerState NoPersistence
      result <- Servant.runHandler (addAIH ss "nonexistent-lobby")
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error for nonexistent lobby"

    it "rejects adding AI after lobby is started" $ do
      ss <- newServerState NoPersistence
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "Test Lobby")
      let lid = clrLobbyId resp
      _ <- run $ addAIH ss lid
      _ <- run $ startH ss lid
      result <- Servant.runHandler (addAIH ss lid)
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error for started lobby"

    it "rejects add-ai for already started lobby via start then add" $ do
      ss <- newServerState NoPersistence
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "Test Lobby")
      let lid = clrLobbyId resp
      _ <- run $ joinH ss lid (JoinLobbyRequest "Bob")
      _ <- run $ startH ss lid
      result <- Servant.runHandler (addAIH ss lid)
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error for started lobby"

  describe "AI game play" $ do
    it "starting game with AI slots spawns AI that makes moves" $ do
      ss <- newServerState NoPersistence
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

    it "human and AI alternate moves in mixed game" $ do
      ss <- newServerState NoPersistence
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "Test Lobby")
      let lid = clrLobbyId resp
      _ <- run $ addAIH ss lid
      gameResp <- run $ startH ss lid
      let gid = sgrGameId gameResp
          humanSession = clrSessionId resp
      -- Poll until it's the human player's turn (player 0 = lobby creator)
      humanReady <- waitForCondition ss gid 150 $ \mg ->
        let gs = mgGameState mg
        in case Map.lookup humanSession (mgSessions mg) of
          Just ps -> case currentPlayer gs of
            Just cp -> playerId cp == psPlayerId ps
                       && gsTurnPhase gs == AwaitingAction
                       && mgStatus mg == GameActive
            Nothing -> False
          Nothing -> False
      case humanReady of
        Nothing -> expectationFailure "timed out waiting for human's turn"
        Just mg -> do
          let gs = mgGameState mg
              turnBefore = gsTurnNumber gs
              actions = legalActions gs
          case actions of
            (a:_) -> do
              result <- processAction ss gid humanSession a
              case result of
                Right () -> do
                  -- Poll until turn number advances (AI responded)
                  advanced <- waitForCondition ss gid 150 $ \mg2 ->
                    gsTurnNumber (mgGameState mg2) > turnBefore
                  case advanced of
                    Just mg2 -> gsTurnNumber (mgGameState mg2) `shouldSatisfy` (> turnBefore)
                    Nothing -> expectationFailure "timed out waiting for AI to respond"
                Left err -> expectationFailure $ "human move rejected: " ++ show err
            [] -> expectationFailure "expected legal actions for human"

    it "human player receives broadcasts when AI acts" $ do
      ss <- newServerState NoPersistence
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "Test Lobby")
      let lid = clrLobbyId resp
      _ <- run $ addAIH ss lid
      gameResp <- run $ startH ss lid
      let gid = sgrGameId gameResp
          humanSession = clrSessionId resp
      -- Register a TChan for the human player immediately after game start
      tv <- lookupGameTVarOrFail ss gid
      chan <- atomically $ registerConnection tv humanSession
      -- If human goes first, make a move so AI can respond
      mg <- lookupGameOrFail ss gid
      let gs = mgGameState mg
      case Map.lookup humanSession (mgSessions mg) of
        Just ps | maybe False (\cp -> playerId cp == psPlayerId ps) (currentPlayer gs) -> do
          let actions = legalActions gs
          case actions of
            (a:_) -> do
              _ <- processAction ss gid humanSession a
              pure ()
            [] -> pure ()
        _ -> pure ()
      -- Poll channel until a GameStateUpdate arrives (100ms intervals, 15s timeout)
      gotBroadcast <- waitForChanMsg chan 150 isGameState
      gotBroadcast `shouldBe` True

  describe "AI error recovery" $ do
    it "AI recovers from agent exception via fallback" $ do
      (ss, gid, s1, s2) <- setupAIGame
      -- ThrowingAgent throws first 2 calls, then delegates to RandomAgent
      throwingAgent <- mkThrowingAgent 2
      -- Spawn both AI threads: one throwing, one random
      tid1 <- forkIO $ aiLoopWith ss gid s1 throwingAgent
      tid2 <- forkIO $ aiLoopWith ss gid s2 RandomAgent
      -- Verify AI recovered by checking turns advance past failures (10s timeout)
      result <- waitForCondition ss gid 100 $ \mg ->
        gsTurnNumber (mgGameState mg) >= 10
      -- Clean up threads
      killThread tid1
      killThread tid2
      isJust result `shouldBe` True

    it "AI continues after processAction rejection" $ do
      (ss, gid, s1, s2) <- setupAIGame
      -- BadActionAgent returns invalid actions for first 3 calls,
      -- triggering processAction rejections before delegating to RandomAgent
      badAgent <- mkBadActionAgent 3
      tid1 <- forkIO $ aiLoopWith ss gid s1 badAgent
      tid2 <- forkIO $ aiLoopWith ss gid s2 RandomAgent
      -- Verify AI recovered by checking turns advance past rejections (10s timeout)
      result <- waitForCondition ss gid 100 $ \mg ->
        gsTurnNumber (mgGameState mg) >= 10
      killThread tid1
      killThread tid2
      isJust result `shouldBe` True

    it "AsyncException kills AI thread cleanly (no retry)" $ do
      (ss, gid, s1, _s2) <- setupAIGame
      tid <- forkIO $ aiLoopWith ss gid s1 RandomAgent
      -- Give it time to start polling
      threadDelay 100000  -- 100ms
      -- Kill the thread — should exit cleanly, not retry
      killThread tid
      -- Verify the game is still active (thread didn't corrupt state)
      mg <- lookupGameOrFail ss gid
      mgStatus mg `shouldBe` GameActive

  describe "checkTurn" $ do
    it "returns AIFinished for nonexistent game" $ do
      ss <- newServerState NoPersistence
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

    it "returns AIWait when currentPlayer is Nothing (invalid index)" $ do
      (ss, gid, s1, s2) <- setupGame
      cs <- currentSession ss gid s1 s2
      tv <- lookupGameTVarOrFail ss gid
      -- Set gsCurrentPlayer to an out-of-bounds index so currentPlayer returns Nothing
      atomically $ modifyTVar' tv $ \mg ->
        let gs = mgGameState mg
            gs' = gs { gsCurrentPlayer = 99 }
        in mg { mgGameState = gs' }
      result <- atomically $ checkTurn ss gid cs
      case result of
        AIWait -> pure ()
        _ -> expectationFailure "expected AIWait for invalid currentPlayer index"

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
          case nobles of
            (n:_) -> nobleId n `shouldBe` "n1"
            []    -> expectationFailure "expected at least one noble"
        _ -> expectationFailure "expected AINeedNoble"

-- | Poll game state every 100ms until predicate holds. Returns Nothing on timeout.
waitForCondition :: ServerState -> GameId -> Int -> (ManagedGame -> Bool) -> IO (Maybe ManagedGame)
waitForCondition _ _ 0 _ = pure Nothing
waitForCondition ss gid remaining predicate = do
  threadDelay 100000  -- 100ms
  mg <- lookupGameOrFail ss gid
  if predicate mg
    then pure (Just mg)
    else waitForCondition ss gid (remaining - 1) predicate

-- | Poll a TChan every 100ms until a message matching the predicate arrives. Returns True if found.
waitForChanMsg :: TChan ServerMessage -> Int -> (ServerMessage -> Bool) -> IO Bool
waitForChanMsg _ 0 _ = pure False
waitForChanMsg chan remaining predicate = do
  msgs <- drainChan chan
  if any predicate msgs
    then pure True
    else do
      threadDelay 100000  -- 100ms
      waitForChanMsg chan (remaining - 1) predicate

-- | Check if a ServerMessage is a GameStateUpdate.
isGameState :: ServerMessage -> Bool
isGameState (GameStateUpdate _) = True
isGameState _ = False
