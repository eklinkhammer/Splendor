module Splendor.Server.Persistence
  ( PersistenceHandle
  , initPersistence
  , saveGame
  , loadGame
  ) where

import Data.Text (Text)
import Splendor.Core.Types (GameState)

-- | Opaque handle to the persistence layer (stubbed).
data PersistenceHandle = PersistenceHandle

-- | Initialize persistence (stubbed no-op).
initPersistence :: Maybe FilePath -> IO PersistenceHandle
initPersistence _ = pure PersistenceHandle

-- | Save a game state (stubbed no-op).
saveGame :: PersistenceHandle -> Text -> GameState -> IO ()
saveGame _ _ _ = pure ()

-- | Load a game state (stubbed, always returns Nothing).
loadGame :: PersistenceHandle -> Text -> IO (Maybe GameState)
loadGame _ _ = pure Nothing
