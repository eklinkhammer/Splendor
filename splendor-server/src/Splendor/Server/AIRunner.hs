module Splendor.Server.AIRunner
  ( spawnAIPlayers
  , aiLoopWith
  , checkTurn
  , AICheck(..)
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (AsyncException, SomeException, fromException, try)
import Data.Map.Strict qualified as Map
import System.IO (hPutStrLn, stderr)

import Splendor.Core.Rules.ActionValidation (legalActions, legalGemReturns)
import Splendor.Core.Types

import Splendor.AI.Agent (Agent(..))
import Splendor.AI.MCTS (MCTSAgent(..), MCTSConfig(..), defaultMCTSConfig)

import Splendor.Server.Types
import Splendor.Server.GameManager (processAction, processGemReturn, processNobleChoice)

-- | Spawn an AI loop thread for each AI player session. Returns thread IDs.
spawnAIPlayers :: ServerState -> GameId -> [(SessionId, PlayerSession)] -> IO [ThreadId]
spawnAIPlayers ss gid aiSessions =
  mapM (\(sid, _ps) -> forkIO (aiLoop ss gid sid)) aiSessions

-- | Main AI loop: polls the game state and makes moves when it's the AI's turn.
--   Wraps the inner loop in a retry mechanism so transient exceptions don't kill the thread.
aiLoop :: ServerState -> GameId -> SessionId -> IO ()
aiLoop ss gid sid =
  aiLoopWith ss gid sid agent
  where
    agent :: MCTSAgent
    agent = MCTSAgent { mctsConfig = defaultMCTSConfig { mctsIterations = 200, mctsTimeoutMs = 2000 } }

-- | Signal returned by 'goOnce' to tell the retry loop what to do next.
data LoopSignal = Continue | Done

-- | Parameterized AI loop for testing: accepts any 'Agent' instance.
aiLoopWith :: forall a. Agent a => ServerState -> GameId -> SessionId -> a -> IO ()
aiLoopWith ss gid sid agent = do
  threadDelay aiMoveDelayUs
  retryLoop (0 :: Int)
  where
    maxRetries :: Int
    maxRetries = 5

    retryLoop :: Int -> IO ()
    retryLoop retries = do
      result <- try @SomeException goOnce
      case result of
        Right Done -> pure ()  -- normal exit (game finished)
        Right Continue -> retryLoop 0  -- reset retries on success
        Left ex
          | Just (_ :: AsyncException) <- fromException ex ->
              -- Thread killed (e.g. game cleanup) — let it die
              pure ()
          | retries >= maxRetries -> do
              logAI $ "AI thread dying after " ++ show maxRetries
                ++ " retries. Last error: " ++ show ex
          | otherwise -> do
              logAI $ "AI loop exception (retry " ++ show (retries + 1)
                ++ "/" ++ show maxRetries ++ "): " ++ show ex
              threadDelay 1000000  -- 1s backoff
              retryLoop (retries + 1)

    goOnce :: IO LoopSignal
    goOnce = do
      mAction <- atomically $ checkTurn ss gid sid
      case mAction of
        AIFinished -> pure Done
        AIWait -> do
          threadDelay aiPollIntervalUs
          pure Continue
        AINeedAction gs _pid -> do
          let actions = legalActions gs
          case actions of
            [] -> pure Continue  -- no legal actions; wait
            (fallback:_) -> do
              result <- try @SomeException (chooseAction agent gs actions)
              action <- case result of
                Right a -> pure a
                Left ex -> do
                  logAI $ "chooseAction exception, using fallback: " ++ show ex
                  pure fallback
              processResult <- processAction ss gid sid action
              case processResult of
                Left err -> logAI $ "processAction rejected: " ++ show err
                Right () -> pure ()
              threadDelay aiMoveDelayUs
              pure Continue
        AINeedGemReturn gs _pid -> do
          let options = legalGemReturns gs
          case options of
            [] -> pure Continue  -- no options; wait
            (fallback:_) -> do
              result <- try @SomeException (chooseGemReturn agent gs options)
              ret <- case result of
                Right r -> pure r
                Left ex -> do
                  logAI $ "chooseGemReturn exception, using fallback: " ++ show ex
                  pure fallback
              processResult <- processGemReturn ss gid sid ret
              case processResult of
                Left err -> logAI $ "processGemReturn rejected: " ++ show err
                Right () -> pure ()
              threadDelay aiMoveDelayUs
              pure Continue
        AINeedNoble gs _pid nobles -> do
          result <- try @SomeException (chooseNoble agent gs nobles)
          noble <- case result of
            Right n -> pure n
            Left ex -> do
              logAI $ "chooseNoble exception, using fallback: " ++ show ex
              case nobles of
                (n:_) -> pure n
                []    -> pure (Noble "" mempty 0)  -- shouldn't happen
          processResult <- processNobleChoice ss gid sid (nobleId noble)
          case processResult of
            Left err -> logAI $ "processNobleChoice rejected: " ++ show err
            Right () -> pure ()
          threadDelay aiMoveDelayUs
          pure Continue

    logAI :: String -> IO ()
    logAI msg = hPutStrLn stderr $ "[AI:" ++ show gid ++ ":" ++ show sid ++ "] " ++ msg

-- | Result of checking whether the AI needs to act.
data AICheck
  = AIFinished
  | AIWait
  | AINeedAction GameState PlayerId
  | AINeedGemReturn GameState PlayerId
  | AINeedNoble GameState PlayerId [Noble]

-- | Atomically check if it's the AI player's turn and what action is needed.
--   Legal move computation is deferred to IO (outside the STM transaction)
--   to minimize transaction hold time.
checkTurn :: ServerState -> GameId -> SessionId -> STM AICheck
checkTurn ss gid sid = do
  games <- readTVar (ssGames ss)
  case Map.lookup gid games of
    Nothing -> pure AIFinished
    Just gameTVar -> do
      mg <- readTVar gameTVar
      case mgStatus mg of
        GameFinished -> pure AIFinished
        GameActive ->
          case Map.lookup sid (mgSessions mg) of
            Nothing -> pure AIFinished
            Just ps ->
              let gs = mgGameState mg
              in case currentPlayer gs of
                Nothing -> pure AIWait
                Just cp
                  | playerId cp /= psPlayerId ps -> pure AIWait
                  | otherwise ->
                      case mgPendingNobles mg of
                        Just nobles -> pure (AINeedNoble gs (playerId cp) nobles)
                        Nothing ->
                          case gsTurnPhase gs of
                            MustReturnGems _ ->
                              pure (AINeedGemReturn gs (playerId cp))
                            AwaitingAction ->
                              pure (AINeedAction gs (playerId cp))

-- | Delay before each AI move for UX (500ms).
aiMoveDelayUs :: Int
aiMoveDelayUs = 500000

-- | Polling interval when it's not the AI's turn (200ms).
aiPollIntervalUs :: Int
aiPollIntervalUs = 200000
