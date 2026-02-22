module Splendor.Server.LobbyCleanup
  ( lobbyCleanupThread
  , removeStaleLobbies
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Exception (AsyncException, SomeException, fromException, try)
import Control.Monad (when)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import System.IO (hPutStrLn, stderr)

import Splendor.Server.Types (Lobby(..), LobbyId, LobbyStatus(..), ServerState(..))

-- | Cleanup interval: 5 minutes in microseconds.
cleanupIntervalUs :: Int
cleanupIntervalUs = 5 * 60 * 1_000_000

-- | Maximum age for Waiting lobbies: 60 minutes in seconds.
lobbyMaxAgeSecs :: Double
lobbyMaxAgeSecs = 3600

-- | Remove stale lobbies. Started/Closed lobbies are removed immediately
-- (they have already transitioned to games). Waiting lobbies are removed
-- after 'lobbyMaxAgeSecs'. Starting lobbies are never removed.
removeStaleLobbies :: UTCTime -> Map.Map LobbyId Lobby -> Map.Map LobbyId Lobby
removeStaleLobbies now = Map.filter notStale
  where
    notStale :: Lobby -> Bool
    notStale lobby = case lobbyStatus lobby of
      Starting -> True  -- never remove mid-transition lobbies
      Started{} -> False -- already transitioned to a game
      Closed   -> False -- already transitioned to a game
      Waiting  -> realToFrac (diffUTCTime now (lobbyCreatedAt lobby)) <= lobbyMaxAgeSecs

-- | Background thread that removes stale lobbies every 5 minutes.
lobbyCleanupThread :: ServerState -> IO ()
lobbyCleanupThread ss = go
  where
    go :: IO ()
    go = do
      threadDelay cleanupIntervalUs
      result <- try @SomeException $ do
        now <- getCurrentTime
        removed <- atomically $ do
          before <- readTVar (ssLobbies ss)
          let after = removeStaleLobbies now before
          writeTVar (ssLobbies ss) after
          pure (Map.size before - Map.size after)
        when (removed > 0) $
          logCleanup $ "Removed " ++ show removed ++ " stale lobbies"
      case result of
        Right () -> go
        Left ex
          | Just (_ :: AsyncException) <- fromException ex -> pure ()
          | otherwise -> do
              logCleanup $ "Exception: " ++ show ex
              go

    logCleanup :: String -> IO ()
    logCleanup msg = hPutStrLn stderr $ "[LobbyCleanup] " ++ msg
