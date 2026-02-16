module Splendor.Server.RestoreSpec (spec) where

import Control.Concurrent.STM
import Data.Either (isRight)
import Data.Map.Strict qualified as Map
import Test.Hspec

import Splendor.Core.Types
import Splendor.Server.GameManager (createGame, processAction, processGemReturn, processNobleChoice)
import Splendor.Server.Persistence (saveGame)
import Splendor.Server.Restore (restoreGames)
import Splendor.Server.TestHelpers
import Splendor.Server.Types

spec :: Spec
spec = do
  describe "restoreGames basic" $ do
    it "empty database → empty ssGames" $ withTempDb $ \ph -> do
      ss <- newServerState ph
      restoreGames ss
      games <- readTVarIO (ssGames ss)
      Map.size games `shouldBe` 0

    it "single game restored into ssGames with correct state" $ withTempDb $ \ph -> do
      -- Create a game with persistence
      (ss, gid, _, _) <- setupGameWithPersistence ph
      mg <- lookupGameOrFail ss gid
      let originalGs = mgGameState mg
      -- "Restart": new ServerState reading from same DB
      ss2 <- newServerState ph
      restoreGames ss2
      games <- readTVarIO (ssGames ss2)
      Map.size games `shouldBe` 1
      case Map.lookup gid games of
        Nothing -> expectationFailure "Game not found after restore"
        Just tv -> do
          mg2 <- readTVarIO tv
          mgGameState mg2 `shouldBe` originalGs

    it "multiple games restored" $ withTempDb $ \ph -> do
      ss <- newServerState ph
      let slots1 = [ LobbySlot "s1" "Alice" False, LobbySlot "s2" "Bob" False ]
          slots2 = [ LobbySlot "s3" "Charlie" False, LobbySlot "s4" "Diana" False ]
      _gid1 <- createGame ss slots1
      _gid2 <- createGame ss slots2
      ss2 <- newServerState ph
      restoreGames ss2
      games <- readTVarIO (ssGames ss2)
      Map.size games `shouldBe` 2

    it "finished games not restored" $ withTempDb $ \ph -> do
      (ss, gid, _, _) <- setupGameWithPersistence ph
      mg <- lookupGameOrFail ss gid
      -- Mark as finished
      saveGame ph gid (mgGameState mg) GameFinished (mgSessions mg) Nothing
      ss2 <- newServerState ph
      restoreGames ss2
      games <- readTVarIO (ssGames ss2)
      Map.size games `shouldBe` 0

  describe "session recovery" $ do
    it "restored sessions registered in ssSessions" $ withTempDb $ \ph -> do
      (ss, gid, s1, s2) <- setupGameWithPersistence ph
      mg <- lookupGameOrFail ss gid
      let originalSessions = mgSessions mg
      ss2 <- newServerState ph
      restoreGames ss2
      sessions <- readTVarIO (ssSessions ss2)
      Map.lookup s1 sessions `shouldBe` Map.lookup s1 originalSessions
      Map.lookup s2 sessions `shouldBe` Map.lookup s2 originalSessions

    it "psIsAI flag preserved through save/restore" $ withTempDb $ \ph -> do
      ss <- newServerState ph
      let slots = [ LobbySlot "human" "Alice" False
                  , LobbySlot "ai-bot" "Bot" True
                  ]
      _gid <- createGame ss slots
      -- Restart
      ss2 <- newServerState ph
      restoreGames ss2
      sessions <- readTVarIO (ssSessions ss2)
      case Map.lookup "human" sessions of
        Just ps -> psIsAI ps `shouldBe` False
        Nothing -> expectationFailure "Human session not found"
      case Map.lookup "ai-bot" sessions of
        Just ps -> psIsAI ps `shouldBe` True
        Nothing -> expectationFailure "AI session not found"

    it "psPlayerId and psPlayerName preserved" $ withTempDb $ \ph -> do
      (ss, gid, s1, s2) <- setupGameWithPersistence ph
      mg <- lookupGameOrFail ss gid
      let origS1 = Map.lookup s1 (mgSessions mg)
          origS2 = Map.lookup s2 (mgSessions mg)
      ss2 <- newServerState ph
      restoreGames ss2
      sessions <- readTVarIO (ssSessions ss2)
      case (origS1, Map.lookup s1 sessions) of
        (Just o, Just r) -> do
          psPlayerId r `shouldBe` psPlayerId o
          psPlayerName r `shouldBe` psPlayerName o
        _ -> expectationFailure "Session not found"
      case (origS2, Map.lookup s2 sessions) of
        (Just o, Just r) -> do
          psPlayerId r `shouldBe` psPlayerId o
          psPlayerName r `shouldBe` psPlayerName o
        _ -> expectationFailure "Session not found"

  describe "connections" $ do
    it "restored games have empty connections (players reconnect via WS)" $ withTempDb $ \ph -> do
      (_ss, gid, _s1, _s2) <- setupGameWithPersistence ph
      ss2 <- newServerState ph
      restoreGames ss2
      games <- readTVarIO (ssGames ss2)
      case Map.lookup gid games of
        Nothing -> expectationFailure "Game not found"
        Just tv -> do
          mg <- readTVarIO tv
          Map.size (mgConnections mg) `shouldBe` 0

    it "restored games have empty spectators map" $ withTempDb $ \ph -> do
      (_ss, gid, _, _) <- setupGameWithPersistence ph
      ss2 <- newServerState ph
      restoreGames ss2
      games <- readTVarIO (ssGames ss2)
      case Map.lookup gid games of
        Nothing -> expectationFailure "Game not found"
        Just tv -> do
          mg <- readTVarIO tv
          Map.size (mgSpectators mg) `shouldBe` 0

  describe "full restart cycle" $ do
    it "create game → play turns → restart → state matches last mutation" $ withTempDb $ \ph -> do
      (ss, gid, s1, s2) <- setupGameWithPersistence ph
      -- Play a turn
      session <- currentSession ss gid s1 s2
      result <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      result `shouldSatisfy` isRight
      -- Read the mutated state
      mg <- lookupGameOrFail ss gid
      let gsAfterTurn = mgGameState mg
      -- "Restart"
      ss2 <- newServerState ph
      restoreGames ss2
      games <- readTVarIO (ssGames ss2)
      case Map.lookup gid games of
        Nothing -> expectationFailure "Game not found after restart"
        Just tv -> do
          mg2 <- readTVarIO tv
          mgGameState mg2 `shouldBe` gsAfterTurn

    it "game in MustReturnGems phase survives restart, processGemReturn works" $ withTempDb $ \ph -> do
      (ss, gid, s1, s2) <- setupGameWithPersistence ph
      gameTVar <- lookupGameTVarOrFail ss gid
      -- Set up: give current player 8 tokens so taking 3 → 11 → NeedGemReturn 1
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
      -- Take gems to trigger MustReturnGems
      result <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      result `shouldSatisfy` isRight
      mg2 <- lookupGameOrFail ss gid
      case gsTurnPhase (mgGameState mg2) of
        MustReturnGems _ -> pure ()
        other -> expectationFailure $ "Expected MustReturnGems, got " ++ show other
      -- "Restart"
      ss2 <- newServerState ph
      restoreGames ss2
      -- Verify restored game is in MustReturnGems
      games <- readTVarIO (ssGames ss2)
      case Map.lookup gid games of
        Nothing -> expectationFailure "Game not found after restart"
        Just tv -> do
          mg3 <- readTVarIO tv
          case gsTurnPhase (mgGameState mg3) of
            MustReturnGems n -> do
              -- processGemReturn should work on restored state
              result2 <- processGemReturn ss2 gid session (singleGem (GemToken Ruby) n)
              result2 `shouldSatisfy` isRight
            other -> expectationFailure $ "Expected MustReturnGems after restore, got " ++ show other

    it "game in NeedNobleChoice phase survives restart, processNobleChoice works" $ withTempDb $ \ph -> do
      (ss, gid, s1, s2) <- setupGameWithPersistence ph
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
      -- Set up: 2 diamond cards owned + buyable card + 2 nobles requiring 3 diamonds
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
      result <- processAction ss gid session (BuyCard (FromDisplay "buyMe") (singleGem (GemToken Ruby) 1))
      result `shouldSatisfy` isRight
      mg2 <- lookupGameOrFail ss gid
      case mgPendingNobles mg2 of
        Just nobles -> length nobles `shouldBe` 2
        Nothing -> expectationFailure "Expected mgPendingNobles to be set"
      -- "Restart"
      ss2 <- newServerState ph
      restoreGames ss2
      games <- readTVarIO (ssGames ss2)
      case Map.lookup gid games of
        Nothing -> expectationFailure "Game not found after restart"
        Just tv -> do
          mg3 <- readTVarIO tv
          case mgPendingNobles mg3 of
            Just nobles@(n:_) -> do
              length nobles `shouldBe` 2
              result2 <- processNobleChoice ss2 gid session (nobleId n)
              result2 `shouldSatisfy` isRight
            Just [] -> expectationFailure "Expected non-empty pendingNobles after restore"
            Nothing -> expectationFailure "Expected mgPendingNobles after restore"

    it "3-player game restored with all sessions" $ withTempDb $ \ph -> do
      (_ss, gid, s1, s2, s3) <- setupGame3WithPersistence ph
      ss2 <- newServerState ph
      restoreGames ss2
      games <- readTVarIO (ssGames ss2)
      case Map.lookup gid games of
        Nothing -> expectationFailure "Game not found after restore"
        Just tv -> do
          mg <- readTVarIO tv
          Map.size (mgSessions mg) `shouldBe` 3
      sessions <- readTVarIO (ssSessions ss2)
      Map.member s1 sessions `shouldBe` True
      Map.member s2 sessions `shouldBe` True
      Map.member s3 sessions `shouldBe` True

    it "AI sessions trigger thread respawn on restore" $ withTempDb $ \ph -> do
      ss <- newServerState ph
      let slots = [ LobbySlot "human" "Alice" False
                  , LobbySlot "ai-bot" "Bot" True
                  ]
      _gid <- createGame ss slots
      -- "Restart"
      ss2 <- newServerState ph
      restoreGames ss2
      games <- readTVarIO (ssGames ss2)
      case Map.toList games of
        [] -> expectationFailure "Game not found after restore"
        ((_, tv):_) -> do
          mg <- readTVarIO tv
          length (mgAIThreads mg) `shouldBe` 1
