module Splendor.Server.AIRunner
  ( spawnAIPlayers
  , checkTurn
  , AICheck(..)
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Data.Map.Strict qualified as Map

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
aiLoop :: ServerState -> GameId -> SessionId -> IO ()
aiLoop ss gid sid = do
  threadDelay aiMoveDelayUs
  go
  where
    go :: IO ()
    go = do
      mAction <- atomically $ checkTurn ss gid sid
      case mAction of
        AIFinished -> pure ()
        AIWait -> do
          threadDelay aiPollIntervalUs
          go
        AINeedAction gs _pid -> do
          let actions = legalActions gs
          case actions of
            [] -> go  -- no legal actions; wait
            (fallback:_) -> do
              result <- try @SomeException (chooseAction agent gs actions)
              action <- case result of
                Right a -> pure a
                Left _  -> pure fallback
              _ <- processAction ss gid sid action
              threadDelay aiMoveDelayUs
              go
        AINeedGemReturn gs _pid -> do
          let options = legalGemReturns gs
          case options of
            [] -> go  -- no options; wait
            (fallback:_) -> do
              result <- try @SomeException (chooseGemReturn agent gs options)
              ret <- case result of
                Right r -> pure r
                Left _  -> pure fallback
              _ <- processGemReturn ss gid sid ret
              threadDelay aiMoveDelayUs
              go
        AINeedNoble gs _pid nobles -> do
          result <- try @SomeException (chooseNoble agent gs nobles)
          noble <- case result of
            Right n -> pure n
            Left _  -> case nobles of
              (n:_) -> pure n
              []    -> pure (Noble "" mempty 0)  -- shouldn't happen
          _ <- processNobleChoice ss gid sid (nobleId noble)
          threadDelay aiMoveDelayUs
          go

    agent :: MCTSAgent
    agent = MCTSAgent { mctsConfig = defaultMCTSConfig { mctsIterations = 200, mctsTimeoutMs = 2000 } }

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
