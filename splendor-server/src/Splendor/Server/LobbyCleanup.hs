module Splendor.Server.LobbyCleanup
  ( lobbyCleanupThread
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar')
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)

import Splendor.Server.Types (Lobby(..), LobbyStatus(..), ServerState(..))

-- | Background thread that removes stale lobbies every 5 minutes.
-- Removes any lobby older than 60 minutes regardless of status,
-- since Waiting lobbies are abandoned and Started/Closed lobbies
-- have already transitioned to games.
lobbyCleanupThread :: ServerState -> IO ()
lobbyCleanupThread ss = go
  where
    go :: IO ()
    go = do
      threadDelay (5 * 60 * 1_000_000)  -- sleep 5 minutes
      now <- getCurrentTime
      atomically $ modifyTVar' (ssLobbies ss) (Map.filter (notStale now))
      go

    notStale :: UTCTime -> Lobby -> Bool
    notStale now lobby = case lobbyStatus lobby of
      Starting -> True  -- never remove mid-transition lobbies
      _        -> diffUTCTime now (lobbyCreatedAt lobby) <= 3600
