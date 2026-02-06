module Splendor.Server.GameManagerSpec (spec) where

import Control.Concurrent.STM
import Data.Either (isRight, isLeft)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec
import Splendor.Core.Types
import Splendor.Server.Types
import Splendor.Server.GameManager

spec :: Spec
spec = do
  describe "createGame" $ do
    it "returns a non-empty gameId" $ do
      (_, gid, _, _) <- setupGame
      gid `shouldSatisfy` (not . T.null)

    it "game is registered in ssGames" $ do
      (ss, gid, _, _) <- setupGame
      mGame <- lookupGame ss gid
      case mGame of
        Just _  -> pure ()
        Nothing -> expectationFailure "Game not found in ssGames"

    it "sessions are registered in ssSessions" $ do
      (ss, _, s1, s2) <- setupGame
      ms1 <- lookupSession ss s1
      ms2 <- lookupSession ss s2
      case ms1 of
        Just _  -> pure ()
        Nothing -> expectationFailure "Session 1 not found"
      case ms2 of
        Just _  -> pure ()
        Nothing -> expectationFailure "Session 2 not found"

    it "initial game state is InProgress, AwaitingAction" $ do
      (ss, gid, _, _) <- setupGame
      mg <- lookupGameOrFail ss gid
      gsPhase (mgGameState mg) `shouldBe` InProgress
      gsTurnPhase (mgGameState mg) `shouldBe` AwaitingAction

    it "player count matches slot count" $ do
      (ss, gid, _, _) <- setupGame
      mg <- lookupGameOrFail ss gid
      length (gsPlayers (mgGameState mg)) `shouldBe` 2

    it "player names match slot names" $ do
      (ss, gid, _, _) <- setupGame
      mg <- lookupGameOrFail ss gid
      let names = map playerName (gsPlayers (mgGameState mg))
      names `shouldBe` ["Alice", "Bob"]

    it "mgPendingNobles is initially Nothing" $ do
      (ss, gid, _, _) <- setupGame
      mg <- lookupGameOrFail ss gid
      mgPendingNobles mg `shouldBe` Nothing

  describe "processAction" $ do
    it "valid gem take advances game state" $ do
      (ss, gid, s1, s2) <- setupGame
      session <- currentSession ss gid s1 s2
      result <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      result `shouldSatisfy` isRight

    it "invalid session returns error" $ do
      (ss, gid, _, _) <- setupGame
      result <- processAction ss gid "bad-session" (TakeGems (TakeDifferent [Ruby]))
      result `shouldBe` Left "Invalid session"

    it "game not found returns error" $ do
      ss <- newServerState
      result <- processAction ss "no-such-game" "any-session" (TakeGems (TakeDifferent [Ruby]))
      result `shouldBe` Left "Game not found"

    it "wrong player's turn returns error" $ do
      (ss, gid, s1, s2) <- setupGame
      wrong <- wrongSession ss gid s1 s2
      result <- processAction ss gid wrong (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      result `shouldSatisfy` isLeft

    it "broadcasts GameStateUpdate after valid action" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      chan1 <- atomically $ registerConnection gameTVar s1
      chan2 <- atomically $ registerConnection gameTVar s2
      session <- currentSession ss gid s1 s2
      _ <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      msg1 <- atomically $ tryReadTChan chan1
      msg2 <- atomically $ tryReadTChan chan2
      msg1 `shouldSatisfy` isJustGameStateUpdate
      msg2 `shouldSatisfy` isJustGameStateUpdate

    it "current player receives ActionRequired after valid action" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      chan1 <- atomically $ registerConnection gameTVar s1
      chan2 <- atomically $ registerConnection gameTVar s2
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
          nextChan = if curIdx == 0 then chan2 else chan1
      _ <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      _ <- atomically $ tryReadTChan nextChan  -- GameStateUpdate
      msg <- atomically $ tryReadTChan nextChan  -- ActionRequired
      msg `shouldSatisfy` isJustActionRequired

    it "privacy: viewer sees own reserved, not opponent's" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      -- Give player 0 a reserved card by modifying game state directly
      atomically $ modifyTVar' gameTVar $ \mg ->
        let gs = mgGameState mg
            ps = gsPlayers gs
            p0 = (ps !! 0) { playerReserved = [Card "r1" Tier1 emptyGems Ruby 0] }
            gs' = gs { gsPlayers = p0 : drop 1 ps }
        in mg { mgGameState = gs' }
      chan1 <- atomically $ registerConnection gameTVar s1
      chan2 <- atomically $ registerConnection gameTVar s2
      session <- currentSession ss gid s1 s2
      _ <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      msg1 <- atomically $ tryReadTChan chan1
      msg2 <- atomically $ tryReadTChan chan2
      case msg1 of
        Just (GameStateUpdate view) -> do
          let pp0 = pgvPlayers view !! 0
          ppReserved pp0 `shouldBe` Just [Card "r1" Tier1 emptyGems Ruby 0]
        _ -> expectationFailure "Expected GameStateUpdate for player 1"
      case msg2 of
        Just (GameStateUpdate view) -> do
          let pp0 = pgvPlayers view !! 0
          ppReserved pp0 `shouldBe` Nothing
        _ -> expectationFailure "Expected GameStateUpdate for player 2"

    it "action causing >10 tokens sends GemReturnNeeded to current player" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      -- Give current player 8 tokens so taking 3 goes to 11 → NeedGemReturn 1
      -- Also ensure bank has Ruby, Diamond, Emerald available for the take
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
      -- Register connection for current player to observe broadcasts
      chan <- atomically $ registerConnection gameTVar session
      result <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      result `shouldSatisfy` isRight
      -- Drain all messages and check that GemReturnNeeded is among them
      msgs <- drainChan chan
      let gemReturnMsgs = [n | GemReturnNeeded n _ <- msgs]
      case gemReturnMsgs of
        [n] -> n `shouldBe` 1
        _   -> expectationFailure $ "Expected exactly one GemReturnNeeded, got messages: " ++ show (map msgTag msgs)

  describe "processGemReturn" $ do
    it "invalid session returns error" $ do
      (ss, gid, _, _) <- setupGame
      result <- processGemReturn ss gid "bad-session" emptyGems
      result `shouldBe` Left "Invalid session"

    it "not in gem return phase returns error" $ do
      (ss, gid, s1, _) <- setupGame
      result <- processGemReturn ss gid s1 (singleGem (GemToken Ruby) 1)
      result `shouldSatisfy` isLeft

    it "valid gem return advances game state" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      -- Set up: give current player 11 tokens and set MustReturnGems 1
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
      atomically $ modifyTVar' gameTVar $ \mg' ->
        let gs = mgGameState mg'
            ps = gsPlayers gs
            cur = ps !! curIdx
            cur' = cur { playerTokens = mkTokens [(GemToken Ruby, 6), (GemToken Diamond, 5)] }
            ps' = replaceAt curIdx cur' ps
        in mg' { mgGameState = gs { gsPlayers = ps', gsTurnPhase = MustReturnGems 1 } }
      result <- processGemReturn ss gid session (singleGem (GemToken Ruby) 1)
      result `shouldSatisfy` isRight
      -- Verify game advanced past gem return
      mg' <- readTVarIO gameTVar
      gsTurnPhase (mgGameState mg') `shouldBe` AwaitingAction

    it "broadcasts GameStateUpdate after valid gem return" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
      atomically $ modifyTVar' gameTVar $ \mg' ->
        let gs = mgGameState mg'
            ps = gsPlayers gs
            cur = ps !! curIdx
            cur' = cur { playerTokens = mkTokens [(GemToken Ruby, 6), (GemToken Diamond, 5)] }
            ps' = replaceAt curIdx cur' ps
        in mg' { mgGameState = gs { gsPlayers = ps', gsTurnPhase = MustReturnGems 1 } }
      chan1 <- atomically $ registerConnection gameTVar s1
      chan2 <- atomically $ registerConnection gameTVar s2
      _ <- processGemReturn ss gid session (singleGem (GemToken Ruby) 1)
      msg1 <- atomically $ tryReadTChan chan1
      msg2 <- atomically $ tryReadTChan chan2
      msg1 `shouldSatisfy` isJustGameStateUpdate
      msg2 `shouldSatisfy` isJustGameStateUpdate

    it "wrong count returns error" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
      atomically $ modifyTVar' gameTVar $ \mg' ->
        let gs = mgGameState mg'
            ps = gsPlayers gs
            cur = ps !! curIdx
            cur' = cur { playerTokens = mkTokens [(GemToken Ruby, 6), (GemToken Diamond, 6)] }
            ps' = replaceAt curIdx cur' ps
        in mg' { mgGameState = gs { gsPlayers = ps', gsTurnPhase = MustReturnGems 2 } }
      -- Return only 1 gem when 2 required
      result <- processGemReturn ss gid session (singleGem (GemToken Ruby) 1)
      result `shouldSatisfy` isLeft

  describe "processNobleChoice" $ do
    it "invalid session returns error" $ do
      (ss, gid, _, _) <- setupGame
      result <- processNobleChoice ss gid "bad-session" "n1"
      result `shouldBe` Left "Invalid session"

    it "noble not in game returns error" $ do
      (ss, gid, s1, _) <- setupGame
      result <- processNobleChoice ss gid s1 "nonexistent-noble"
      result `shouldSatisfy` isLeft

    it "valid noble choice advances game state" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
      -- Place 2 nobles on the board that we can choose from
      let noble1 = Noble "test-n1" (Map.fromList [(Diamond, 1)]) 3
          noble2 = Noble "test-n2" (Map.fromList [(Ruby, 1)]) 3
      atomically $ modifyTVar' gameTVar $ \mg' ->
        let gs = mgGameState mg'
            board' = (gsBoard gs) { boardNobles = [noble1, noble2] }
        in mg' { mgGameState = gs { gsBoard = board' } }
      result <- processNobleChoice ss gid session "test-n1"
      result `shouldSatisfy` isRight
      -- Verify noble was removed from board and assigned to player
      mg' <- readTVarIO gameTVar
      let gs' = mgGameState mg'
          nobles' = boardNobles (gsBoard gs')
      any (\n -> nobleId n == "test-n1") nobles' `shouldBe` False

    it "sets mgPendingNobles on NeedNobleChoice via processAction" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
      -- Set up: player has 2 Diamond bonuses (purchased cards), board has 2 nobles
      -- requiring 3 Diamond, and a cheap card giving +1 Diamond is in the display.
      -- Buying that card → 3 Diamond bonuses → both nobles eligible → NeedNobleChoice
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
      let payment = singleGem (GemToken Ruby) 1
      result <- processAction ss gid session (BuyCard (FromDisplay "buyMe") payment)
      result `shouldSatisfy` isRight
      -- Verify mgPendingNobles is set
      mg' <- readTVarIO gameTVar
      case mgPendingNobles mg' of
        Just nobles -> length nobles `shouldBe` 2
        Nothing -> expectationFailure "Expected mgPendingNobles to be set"

    it "processNobleChoice clears mgPendingNobles" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
      -- Trigger NeedNobleChoice same as above
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
      -- Now choose a noble
      _ <- processNobleChoice ss gid session "noble-a"
      mg' <- readTVarIO gameTVar
      mgPendingNobles mg' `shouldBe` Nothing

  describe "session cleanup" $ do
    it "sessions are removed from ssSessions after GameOver" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      mg <- readTVarIO gameTVar
      let curIdx = gsCurrentPlayer (mgGameState mg)
          session = if curIdx == 0 then s1 else s2
      -- Set up a game state where FinalRound + next player is 0 → GameOver
      -- Player 0 has 15+ prestige, it's player 1's turn (so curIdx=1),
      -- phase is FinalRound. When player 1's action advances turn back to 0,
      -- the engine triggers GameOver.
      -- Simpler: just set it to FinalRound with curPlayer = 1 (last player before wrap)
      -- and give player 0 enough prestige.
      let prestigeCard = Card "pc1" Tier3 emptyGems Ruby 15
      atomically $ modifyTVar' gameTVar $ \mg' ->
        let gs = mgGameState mg'
            ps = gsPlayers gs
            -- Give player 0 a card worth 15 prestige
            p0 = (ps !! 0) { playerPurchased = [prestigeCard] }
            ps' = replaceAt 0 p0 ps
            -- Set current player to 1, FinalRound phase
            gs' = gs { gsPlayers = ps', gsCurrentPlayer = 1, gsPhase = FinalRound }
        in mg' { mgGameState = gs' }
      -- Player 1 takes gems → turn advances → wraps to player 0 → GameOver
      _ <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      -- Wait, need to make sure session is for player 1 (curIdx=1)
      -- Actually we forced curIdx=1, so we need s2's session
      -- Re-check: we set gsCurrentPlayer to 1, so we need player 1's session
      -- But we don't know which session maps to which player index.
      -- Let's look up the correct session.
      mg2 <- readTVarIO gameTVar
      -- If it reached GameOver, sessions should be cleaned
      case mgStatus mg2 of
        GameFinished -> do
          -- Verify sessions are removed from global map
          ms1 <- lookupSession ss s1
          ms2 <- lookupSession ss s2
          ms1 `shouldBe` Nothing
          ms2 `shouldBe` Nothing
        GameActive ->
          -- The action might have failed because we picked the wrong session.
          -- Try the other session.
          pendingAfterRetry ss gid s1 s2 gameTVar

  describe "registerConnection / unregisterConnection" $ do
    it "register adds a channel that can receive messages" $ do
      (ss, gid, s1, _) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      chan <- atomically $ registerConnection gameTVar s1
      atomically $ writeTChan chan Pong
      msg <- atomically $ readTChan chan
      case msg of
        Pong -> pure ()
        _    -> expectationFailure "Expected Pong"

    it "unregister removes channel from connections map" $ do
      (ss, gid, s1, _) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      _ <- atomically $ registerConnection gameTVar s1
      atomically $ unregisterConnection gameTVar s1
      mg <- readTVarIO gameTVar
      Map.member s1 (mgConnections mg) `shouldBe` False

  describe "lookupGame" $ do
    it "found after create" $ do
      (ss, gid, _, _) <- setupGame
      result <- lookupGame ss gid
      case result of
        Just _  -> pure ()
        Nothing -> expectationFailure "Game not found after create"

    it "not found for random id" $ do
      ss <- newServerState
      result <- lookupGame ss "random-id"
      case result of
        Nothing -> pure ()
        Just _  -> expectationFailure "Expected Nothing for random game id"

  describe "lookupSession" $ do
    it "found after create" $ do
      (ss, _, s1, _) <- setupGame
      result <- lookupSession ss s1
      case result of
        Just _  -> pure ()
        Nothing -> expectationFailure "Session not found after create"

    it "not found for random id" $ do
      ss <- newServerState
      result <- lookupSession ss "random-id"
      result `shouldBe` Nothing

  describe "resolveSession" $ do
    it "finds a session in the ManagedGame" $ do
      (ss, gid, s1, _) <- setupGame
      mg <- lookupGameOrFail ss gid
      resolveSession s1 mg `shouldSatisfy` \case
        Just ps -> psSessionId ps == s1
        Nothing -> False

    it "returns Nothing for unknown session" $ do
      (ss, gid, _, _) <- setupGame
      mg <- lookupGameOrFail ss gid
      resolveSession "unknown" mg `shouldBe` Nothing

-- ============================================================
-- Setup helpers
-- ============================================================

-- | Create a ServerState with a 2-player game, returning identifiers
setupGame :: IO (ServerState, GameId, SessionId, SessionId)
setupGame = do
  ss <- newServerState
  let s1 = "session-1"
      s2 = "session-2"
      slots = [ LobbySlot s1 "Alice" False
              , LobbySlot s2 "Bob" False
              ]
  gid <- createGame ss slots
  pure (ss, gid, s1, s2)

-- | Look up a game TVar, failing the test if not found.
lookupGameTVarOrFail :: ServerState -> GameId -> IO (TVar ManagedGame)
lookupGameTVarOrFail ss gid = do
  mGame <- lookupGame ss gid
  case mGame of
    Just tv -> pure tv
    Nothing -> error "Game not found (test setup failure)"

-- | Read ManagedGame, failing the test if game not found.
lookupGameOrFail :: ServerState -> GameId -> IO ManagedGame
lookupGameOrFail ss gid = lookupGameTVarOrFail ss gid >>= readTVarIO

-- | Get the session ID of the current player.
currentSession :: ServerState -> GameId -> SessionId -> SessionId -> IO SessionId
currentSession ss gid s1 s2 = do
  mg <- lookupGameOrFail ss gid
  let curIdx = gsCurrentPlayer (mgGameState mg)
  pure $ if curIdx == 0 then s1 else s2

-- | Get the session ID of the non-current player.
wrongSession :: ServerState -> GameId -> SessionId -> SessionId -> IO SessionId
wrongSession ss gid s1 s2 = do
  mg <- lookupGameOrFail ss gid
  let curIdx = gsCurrentPlayer (mgGameState mg)
  pure $ if curIdx == 0 then s2 else s1

-- | Replace the element at a given index in a list.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx x xs = take idx xs ++ [x] ++ drop (idx + 1) xs

-- | Build a GemCollection from a list of (TokenType, Int) pairs.
mkTokens :: [(TokenType, Int)] -> GemCollection
mkTokens = GemCollection . Map.fromList

-- | Helper for session cleanup test retry with the other session.
pendingAfterRetry :: ServerState -> GameId -> SessionId -> SessionId -> TVar ManagedGame -> IO ()
pendingAfterRetry ss gid s1 s2 gameTVar = do
  -- Reset game state for retry with the other player
  mg <- readTVarIO gameTVar
  let curIdx = gsCurrentPlayer (mgGameState mg)
      otherSession = if curIdx == 0 then s1 else s2
  result <- processAction ss gid otherSession (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
  result `shouldSatisfy` isRight
  mg' <- readTVarIO gameTVar
  mgStatus mg' `shouldBe` GameFinished
  ms1 <- lookupSession ss s1
  ms2 <- lookupSession ss s2
  ms1 `shouldBe` Nothing
  ms2 `shouldBe` Nothing

-- ============================================================
-- Message matchers
-- ============================================================

isJustGameStateUpdate :: Maybe ServerMessage -> Bool
isJustGameStateUpdate (Just (GameStateUpdate _)) = True
isJustGameStateUpdate _                          = False

isJustActionRequired :: Maybe ServerMessage -> Bool
isJustActionRequired (Just (ActionRequired _)) = True
isJustActionRequired _                         = False

-- | Drain all available messages from a TChan.
drainChan :: TChan ServerMessage -> IO [ServerMessage]
drainChan chan = do
  mMsg <- atomically $ tryReadTChan chan
  case mMsg of
    Nothing  -> pure []
    Just msg -> (msg :) <$> drainChan chan

-- | Short tag for a ServerMessage (for error reporting).
msgTag :: ServerMessage -> String
msgTag (GameStateUpdate _)    = "GameStateUpdate"
msgTag (ActionRequired _)     = "ActionRequired"
msgTag (GemReturnNeeded n _)  = "GemReturnNeeded(" ++ show n ++ ")"
msgTag (NobleChoiceRequired _)= "NobleChoiceRequired"
msgTag (GameOverMsg _)        = "GameOverMsg"
msgTag (ErrorMsg t)           = "ErrorMsg(" ++ show t ++ ")"
msgTag Pong                   = "Pong"
