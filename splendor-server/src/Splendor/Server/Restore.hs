module Splendor.Server.Restore
  ( restoreGames
  ) where

import Control.Concurrent.STM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Splendor.Core.Types (GameState, Noble)

import Splendor.Server.AIRunner (spawnAIPlayers)
import Splendor.Server.GameManager (storeAIThreads)
import Splendor.Server.Persistence (loadAllActiveGames)
import Splendor.Server.Types

-- | Restore all active games from the database on server startup.
restoreGames :: ServerState -> IO ()
restoreGames ss = do
  games <- loadAllActiveGames (ssPersistence ss)
  mapM_ (restoreOne ss) games
  TIO.putStrLn $ "Restored " <> T.pack (show (length games)) <> " active game(s) from database"

restoreOne :: ServerState
           -> (GameId, GameState, Map.Map SessionId PlayerSession, Maybe [Noble])
           -> IO ()
restoreOne ss (gid, gs, sessions, pendingNobles) = do
  -- Build a ManagedGame with empty connections (players will reconnect via WS)
  chans <- mapM (\_ -> newTChanIO) (Map.elems sessions)
  let chanMap = Map.fromList $ zip (Map.keys sessions) chans
      mg = ManagedGame
        { mgGameState     = gs
        , mgSessions      = sessions
        , mgConnections   = chanMap
        , mgSpectators    = Map.empty
        , mgStatus        = GameActive
        , mgPendingNobles = pendingNobles
        , mgAIThreads     = []
        }
  gameTVar <- newTVarIO mg
  atomically $ do
    modifyTVar' (ssGames ss) (Map.insert gid gameTVar)
    modifyTVar' (ssSessions ss) (\m -> Map.union m sessions)
  -- Respawn AI threads for AI sessions
  let aiSessions = [ (psSessionId ps, ps)
                   | ps <- Map.elems sessions
                   , psIsAI ps
                   ]
  case aiSessions of
    [] -> pure ()
    _  -> do
      tids <- spawnAIPlayers ss gid aiSessions
      storeAIThreads ss gid tids
