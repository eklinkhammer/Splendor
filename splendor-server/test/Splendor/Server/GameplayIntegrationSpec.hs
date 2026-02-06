module Splendor.Server.GameplayIntegrationSpec (spec) where

import Control.Concurrent.STM
import Data.Either (isRight, isLeft)
import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import System.Random (StdGen, newStdGen, uniformR)
import Test.Hspec

import Splendor.Core.Rules.ActionValidation (legalActions, legalGemReturns)
import Splendor.Core.Types
import Splendor.Server.GameManager
import Splendor.Server.TestHelpers
import Splendor.Server.Types

spec :: Spec
spec = do
  -- ================================================================
  -- Section 1: Full game simulation through server layer
  -- ================================================================
  describe "full game simulation through server layer" $ do
    it "2-player random game completes through server" $ do
      (ss, gid, s1, s2) <- setupGame
      gen <- newStdGen
      simulateServerGame ss gid [s1, s2] gen 2000
      mg <- lookupGameOrFail ss gid
      mgStatus mg `shouldBe` GameFinished

    it "3-player random game completes through server" $ do
      (ss, gid, s1, s2, s3) <- setupGame3
      gen <- newStdGen
      simulateServerGame ss gid [s1, s2, s3] gen 3000
      mg <- lookupGameOrFail ss gid
      mgStatus mg `shouldBe` GameFinished

    it "final game state conserves tokens" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      mg0 <- readTVarIO gameTVar
      let initialTotal = totalTokens (mgGameState mg0)
      gen <- newStdGen
      simulateServerGame ss gid [s1, s2] gen 2000
      mg <- readTVarIO gameTVar
      totalTokens (mgGameState mg) `shouldBe` initialTotal

    it "sessions cleaned up after organic game over" $ do
      (ss, gid, s1, s2) <- setupGame
      gen <- newStdGen
      simulateServerGame ss gid [s1, s2] gen 2000
      ms1 <- lookupSession ss s1
      ms2 <- lookupSession ss s2
      ms1 `shouldBe` Nothing
      ms2 `shouldBe` Nothing

    it "all processAction calls return Right during simulation" $ do
      -- This is implicitly tested by simulateServerGame (it fails on Left),
      -- but we make it explicit here.
      (ss, gid, s1, s2) <- setupGame
      gen <- newStdGen
      -- If any call returns Left, simulateServerGame will call expectationFailure
      simulateServerGame ss gid [s1, s2] gen 2000

    it "both players receive GameOverMsg at end of simulation" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      chan1 <- atomically $ registerConnection gameTVar s1
      chan2 <- atomically $ registerConnection gameTVar s2
      gen <- newStdGen
      simulateServerGame ss gid [s1, s2] gen 2000
      msgs1 <- drainChan chan1
      msgs2 <- drainChan chan2
      let hasGameOver msgs = any isGameOverMsg msgs
      hasGameOver msgs1 `shouldBe` True
      hasGameOver msgs2 `shouldBe` True

  -- ================================================================
  -- Section 2: Multi-step turn phase sequences
  -- ================================================================
  describe "multi-step turn phase sequences" $ do
    it "action→NeedGemReturn→processGemReturn→turn advances" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
      -- Give current player 8 tokens so taking 3 → 11 → NeedGemReturn 1
      atomically $ modifyTVar' gameTVar $ \mg' ->
        let gs = mgGameState mg'
            ps = gsPlayers gs
            cur = ps !! curIdx
            cur' = cur { playerTokens = mkTokens [(GemToken Sapphire, 4), (GemToken Onyx, 4)] }
            ps' = replaceAt curIdx cur' ps
            board' = (gsBoard gs) { boardBank = mkTokens
              [ (GemToken Ruby, 4), (GemToken Diamond, 4), (GemToken Emerald, 4)
              , (GemToken Sapphire, 0), (GemToken Onyx, 0), (GoldToken, 5) ] }
        in mg' { mgGameState = gs { gsPlayers = ps', gsBoard = board' } }
      result <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      result `shouldSatisfy` isRight
      -- Now in MustReturnGems phase
      mg2 <- readTVarIO gameTVar
      case gsTurnPhase (mgGameState mg2) of
        MustReturnGems 1 -> pure ()
        other -> expectationFailure $ "Expected MustReturnGems 1, got: " ++ show other
      -- Return 1 gem
      result2 <- processGemReturn ss gid session (singleGem (GemToken Ruby) 1)
      result2 `shouldSatisfy` isRight
      -- Verify turn advanced
      mg3 <- readTVarIO gameTVar
      gsTurnPhase (mgGameState mg3) `shouldBe` AwaitingAction
      gsCurrentPlayer (mgGameState mg3) `shouldNotBe` curIdx

    it "action→NeedGemReturn→invalid return→valid return" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
      atomically $ modifyTVar' gameTVar $ \mg' ->
        let gs = mgGameState mg'
            ps = gsPlayers gs
            cur = ps !! curIdx
            cur' = cur { playerTokens = mkTokens [(GemToken Sapphire, 4), (GemToken Onyx, 4)] }
            ps' = replaceAt curIdx cur' ps
            board' = (gsBoard gs) { boardBank = mkTokens
              [ (GemToken Ruby, 4), (GemToken Diamond, 4), (GemToken Emerald, 4)
              , (GemToken Sapphire, 0), (GemToken Onyx, 0), (GoldToken, 5) ] }
        in mg' { mgGameState = gs { gsPlayers = ps', gsBoard = board' } }
      _ <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      -- Invalid: return 2 when only 1 needed
      result1 <- processGemReturn ss gid session (mkTokens [(GemToken Ruby, 1), (GemToken Diamond, 1)])
      result1 `shouldSatisfy` isLeft
      -- Valid: return exactly 1
      result2 <- processGemReturn ss gid session (singleGem (GemToken Ruby) 1)
      result2 `shouldSatisfy` isRight

    it "buy card→NeedNobleChoice→processNobleChoice→turn advances" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
      let diamondCard1 = Card "dc1" Tier1 emptyGems Diamond 0
          diamondCard2 = Card "dc2" Tier1 emptyGems Diamond 0
          buyableCard  = Card "buyMe" Tier1 (singleGem (GemToken Ruby) 1) Diamond 0
          noble1 = Noble "noble-a" (Map.fromList [(Diamond, 3)]) 3
          noble2 = Noble "noble-b" (Map.fromList [(Diamond, 3)]) 3
      atomically $ modifyTVar' gameTVar $ \mg' ->
        let gs = mgGameState mg'
            ps = gsPlayers gs
            cur = ps !! curIdx
            cur' = cur
              { playerPurchased = [diamondCard1, diamondCard2]
              , playerTokens = mkTokens [(GemToken Ruby, 1)]
              }
            ps' = replaceAt curIdx cur' ps
            board' = (gsBoard gs)
              { boardTier1 = TierRow [] [buyableCard]
              , boardNobles = [noble1, noble2]
              }
        in mg' { mgGameState = gs { gsPlayers = ps', gsBoard = board' } }
      _ <- processAction ss gid session (BuyCard (FromDisplay "buyMe") (singleGem (GemToken Ruby) 1))
      -- Verify pending nobles
      mg2 <- readTVarIO gameTVar
      case mgPendingNobles mg2 of
        Just nobles -> length nobles `shouldBe` 2
        Nothing -> expectationFailure "Expected pending nobles"
      -- Choose a noble
      result <- processNobleChoice ss gid session "noble-a"
      result `shouldSatisfy` isRight
      -- Verify turn advanced and pending cleared
      mg3 <- readTVarIO gameTVar
      mgPendingNobles mg3 `shouldBe` Nothing
      gsCurrentPlayer (mgGameState mg3) `shouldNotBe` curIdx

    it "buy→NeedNobleChoice→invalid noble→valid noble" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
      let diamondCard1 = Card "dc1" Tier1 emptyGems Diamond 0
          diamondCard2 = Card "dc2" Tier1 emptyGems Diamond 0
          buyableCard  = Card "buyMe" Tier1 (singleGem (GemToken Ruby) 1) Diamond 0
          noble1 = Noble "noble-a" (Map.fromList [(Diamond, 3)]) 3
          noble2 = Noble "noble-b" (Map.fromList [(Diamond, 3)]) 3
      atomically $ modifyTVar' gameTVar $ \mg' ->
        let gs = mgGameState mg'
            ps = gsPlayers gs
            cur = ps !! curIdx
            cur' = cur
              { playerPurchased = [diamondCard1, diamondCard2]
              , playerTokens = mkTokens [(GemToken Ruby, 1)]
              }
            ps' = replaceAt curIdx cur' ps
            board' = (gsBoard gs)
              { boardTier1 = TierRow [] [buyableCard]
              , boardNobles = [noble1, noble2]
              }
        in mg' { mgGameState = gs { gsPlayers = ps', gsBoard = board' } }
      _ <- processAction ss gid session (BuyCard (FromDisplay "buyMe") (singleGem (GemToken Ruby) 1))
      -- Invalid noble
      result1 <- processNobleChoice ss gid session "nonexistent-noble"
      result1 `shouldSatisfy` isLeft
      -- Valid noble
      result2 <- processNobleChoice ss gid session "noble-a"
      result2 `shouldSatisfy` isRight

    it "processAction rejected during MustReturnGems" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
      -- Force into MustReturnGems phase
      atomically $ modifyTVar' gameTVar $ \mg' ->
        let gs = mgGameState mg'
            ps = gsPlayers gs
            cur = ps !! curIdx
            cur' = cur { playerTokens = mkTokens [(GemToken Ruby, 6), (GemToken Diamond, 5)] }
            ps' = replaceAt curIdx cur' ps
        in mg' { mgGameState = gs { gsPlayers = ps', gsTurnPhase = MustReturnGems 1 } }
      result <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      result `shouldSatisfy` isLeft

    it "processGemReturn rejected during AwaitingAction" $ do
      (ss, gid, s1, s2) <- setupGame
      session <- currentSession ss gid s1 s2
      result <- processGemReturn ss gid session (singleGem (GemToken Ruby) 1)
      result `shouldSatisfy` isLeft

    it "two consecutive turns: player 1→player 2" $ do
      (ss, gid, s1, s2) <- setupGame
      -- Turn 1
      session1 <- currentSession ss gid s1 s2
      result1 <- processAction ss gid session1 (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      result1 `shouldSatisfy` isRight
      -- Turn 2: other player
      session2 <- currentSession ss gid s1 s2
      session2 `shouldNotBe` session1
      result2 <- processAction ss gid session2 (TakeGems (TakeDifferent [Sapphire, Onyx, Ruby]))
      result2 `shouldSatisfy` isRight

  -- ================================================================
  -- Section 3: Lobby → Game → Play lifecycle
  -- ================================================================
  describe "lobby → game → play lifecycle" $ do
    it "lobby create→join→start→play one turn" $ do
      ss <- newServerState
      -- Create and join
      createResp <- run $ createH ss (CreateLobbyRequest "Alice" "Test")
      let lid = clrLobbyId createResp
          s1  = clrSessionId createResp
      joinResp <- run $ joinH ss lid (JoinLobbyRequest "Bob")
      let s2 = jlrSessionId joinResp
      -- Start
      startResp <- run $ startH ss lid
      let gid = sgrGameId startResp
      -- Play one turn
      session <- currentSession ss gid s1 s2
      result <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      result `shouldSatisfy` isRight

    it "lobby create→join→start→play to completion" $ do
      ss <- newServerState
      createResp <- run $ createH ss (CreateLobbyRequest "Alice" "Test")
      let lid = clrLobbyId createResp
          s1  = clrSessionId createResp
      joinResp <- run $ joinH ss lid (JoinLobbyRequest "Bob")
      let s2 = jlrSessionId joinResp
      startResp <- run $ startH ss lid
      let gid = sgrGameId startResp
      gen <- newStdGen
      simulateServerGame ss gid [s1, s2] gen 2000
      mg <- lookupGameOrFail ss gid
      mgStatus mg `shouldBe` GameFinished

    it "lobby start creates valid game state" $ do
      ss <- newServerState
      createResp <- run $ createH ss (CreateLobbyRequest "Alice" "Test")
      let lid = clrLobbyId createResp
      _ <- run $ joinH ss lid (JoinLobbyRequest "Bob")
      startResp <- run $ startH ss lid
      let gid = sgrGameId startResp
      mg <- lookupGameOrFail ss gid
      let gs = mgGameState mg
      length (gsPlayers gs) `shouldBe` 2
      map playerName (gsPlayers gs) `shouldBe` ["Alice", "Bob"]
      gsPhase gs `shouldBe` InProgress
      gsTurnPhase gs `shouldBe` AwaitingAction

  -- ================================================================
  -- Section 4: Broadcast message ordering
  -- ================================================================
  describe "broadcast message ordering" $ do
    it "GameStateUpdate comes before ActionRequired per turn" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      chan1 <- atomically $ registerConnection gameTVar s1
      chan2 <- atomically $ registerConnection gameTVar s2
      -- Play 3 turns, checking order after each
      playTurnsCheckOrder ss gid s1 s2 chan1 chan2 3

    it "GemReturnNeeded sent only to current player, not opponent" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
          otherSession = if curIdx == 0 then s2 else s1
      -- Set up for NeedGemReturn
      atomically $ modifyTVar' gameTVar $ \mg' ->
        let gs = mgGameState mg'
            ps = gsPlayers gs
            cur = ps !! curIdx
            cur' = cur { playerTokens = mkTokens [(GemToken Sapphire, 4), (GemToken Onyx, 4)] }
            ps' = replaceAt curIdx cur' ps
            board' = (gsBoard gs) { boardBank = mkTokens
              [ (GemToken Ruby, 4), (GemToken Diamond, 4), (GemToken Emerald, 4)
              , (GemToken Sapphire, 0), (GemToken Onyx, 0), (GoldToken, 5) ] }
        in mg' { mgGameState = gs { gsPlayers = ps', gsBoard = board' } }
      chanCur <- atomically $ registerConnection gameTVar session
      chanOther <- atomically $ registerConnection gameTVar otherSession
      _ <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      msgsCur <- drainChan chanCur
      msgsOther <- drainChan chanOther
      let hasGemReturn msgs = any (\case GemReturnNeeded _ _ -> True; _ -> False) msgs
      hasGemReturn msgsCur `shouldBe` True
      hasGemReturn msgsOther `shouldBe` False

-- ============================================================
-- Simulation helper
-- ============================================================

-- | Simulate a full game through the server layer by picking random legal actions.
simulateServerGame :: ServerState -> GameId -> [SessionId] -> StdGen -> Int -> IO ()
simulateServerGame ss gid sessions gen0 maxTurns = go gen0 maxTurns
  where
    go _ 0 = expectationFailure "Game did not finish within turn limit"
    go g n = do
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      case mgStatus mg of
        GameFinished -> pure ()
        GameActive -> do
          let gs = mgGameState mg
              curIdx = gsCurrentPlayer gs
              session = sessions !! curIdx
          case gsTurnPhase gs of
            AwaitingAction -> do
              let actions = legalActions gs
              case actions of
                [] -> do
                  -- Game may have finished between our read and now (TOCTOU)
                  mg' <- readTVarIO gameTVar
                  case mgStatus mg' of
                    GameFinished -> pure ()
                    _ -> expectationFailure "No legal actions available"
                _ -> do
                  let (idx, g') = uniformR (0, length actions - 1) g
                      action = actions !! idx
                  result <- processAction ss gid session action
                  case result of
                    Left err -> expectationFailure $ "processAction failed: " ++ show err
                    Right () -> handlePostAction ss gid sessions g' (n - 1)
            MustReturnGems _ -> do
              let returns = legalGemReturns gs
              case returns of
                [] -> expectationFailure "No legal gem returns available"
                _ -> do
                  let (idx, g') = uniformR (0, length returns - 1) g
                      ret = returns !! idx
                  result <- processGemReturn ss gid session ret
                  case result of
                    Left err -> expectationFailure $ "processGemReturn failed: " ++ show err
                    Right () -> handlePostAction ss gid sessions g' (n - 1)

    handlePostAction :: ServerState -> GameId -> [SessionId] -> StdGen -> Int -> IO ()
    handlePostAction ss' gid' sessions' g' n' = do
      gameTVar <- lookupGameTVarOrFail ss' gid'
      mg <- readTVarIO gameTVar
      case mgPendingNobles mg of
        Just nobles -> do
          let curIdx = gsCurrentPlayer (mgGameState mg)
              session = sessions' !! curIdx
              (nIdx, g'') = uniformR (0, length nobles - 1) g'
              noble = nobles !! nIdx
          result <- processNobleChoice ss' gid' session (nobleId noble)
          case result of
            Left err -> expectationFailure $ "processNobleChoice failed: " ++ show err
            Right () -> go g'' n'
        Nothing -> go g' n'

-- ============================================================
-- Token conservation helper
-- ============================================================

-- | Compute total tokens in the game (bank + all player hands).
totalTokens :: GameState -> GemCollection
totalTokens gs =
  foldl addGems (boardBank (gsBoard gs)) (map playerTokens (gsPlayers gs))

-- ============================================================
-- Message helpers
-- ============================================================

isGameOverMsg :: ServerMessage -> Bool
isGameOverMsg (GameOverMsg _) = True
isGameOverMsg _               = False

-- ============================================================
-- Broadcast ordering helper
-- ============================================================

-- | Play N turns and verify GameStateUpdate comes before ActionRequired in each.
playTurnsCheckOrder :: ServerState -> GameId -> SessionId -> SessionId
                    -> TChan ServerMessage -> TChan ServerMessage -> Int -> IO ()
playTurnsCheckOrder _  _   _  _  _     _     0 = pure ()
playTurnsCheckOrder ss gid s1 s2 chan1 chan2 n = do
  session <- currentSession ss gid s1 s2
  mg <- lookupGameOrFail ss gid
  let actions = legalActions (mgGameState mg)
  case actions of
    [] -> expectationFailure "No legal actions available"
    (action:_) -> do
      result <- processAction ss gid session action
      result `shouldSatisfy` isRight
  -- Drain both channels and check ordering on the next player's channel
  -- (they receive both GameStateUpdate and ActionRequired)
  nextSession <- currentSession ss gid s1 s2
  let nextChan = if nextSession == s1 then chan1 else chan2
  msgs <- drainChan nextChan
  let tags = map msgTag msgs
  -- GameStateUpdate should appear before ActionRequired
  case (elemIndex "GameStateUpdate" tags, elemIndex "ActionRequired" tags) of
    (Just gsuIdx, Just arIdx) -> gsuIdx `shouldSatisfy` (< arIdx)
    _ -> pure ()  -- If not both present, no ordering to check
  -- Also drain the other channel to clear it
  let otherChan = if nextSession == s1 then chan2 else chan1
  _ <- drainChan otherChan
  playTurnsCheckOrder ss gid s1 s2 chan1 chan2 (n - 1)

