module Splendor.Server.AIRunner
  ( spawnAIPlayers
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM
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
        AINeedAction gs _pid actions -> do
          action <- chooseAction agent gs actions
          _ <- processAction ss gid sid action
          -- Small delay between checking again
          threadDelay aiMoveDelayUs
          go
        AINeedGemReturn gs _pid options -> do
          ret <- chooseGemReturn agent gs options
          _ <- processGemReturn ss gid sid ret
          threadDelay aiMoveDelayUs
          go
        AINeedNoble gs _pid nobles -> do
          noble <- chooseNoble agent gs nobles
          _ <- processNobleChoice ss gid sid (nobleId noble)
          threadDelay aiMoveDelayUs
          go

    agent :: MCTSAgent
    agent = MCTSAgent { mctsConfig = defaultMCTSConfig { mctsIterations = 200, mctsTimeoutMs = 2000 } }

-- | Result of checking whether the AI needs to act.
data AICheck
  = AIFinished
  | AIWait
  | AINeedAction GameState PlayerId [Action]
  | AINeedGemReturn GameState PlayerId [GemCollection]
  | AINeedNoble GameState PlayerId [Noble]

-- | Atomically check if it's the AI player's turn and what action is needed.
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
                              let returns = legalGemReturns gs
                              in pure (AINeedGemReturn gs (playerId cp) returns)
                            AwaitingAction ->
                              let actions = legalActions gs
                              in pure (AINeedAction gs (playerId cp) actions)

-- | Delay before each AI move for UX (500ms).
aiMoveDelayUs :: Int
aiMoveDelayUs = 500000

-- | Polling interval when it's not the AI's turn (200ms).
aiPollIntervalUs :: Int
aiPollIntervalUs = 200000
