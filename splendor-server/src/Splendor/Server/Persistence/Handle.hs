module Splendor.Server.Persistence.Handle
  ( PersistenceHandle(..)
  ) where

import Database.SQLite.Simple (Connection)

-- | Handle to the persistence layer. 'NoPersistence' disables all DB operations.
data PersistenceHandle
  = PersistenceHandle Connection
  | NoPersistence
