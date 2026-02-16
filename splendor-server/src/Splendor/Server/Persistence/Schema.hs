module Splendor.Server.Persistence.Schema
  ( initSchema
  ) where

import Control.Exception (SomeException, try)
import Database.SQLite.Simple (execute_)

import Splendor.Server.Persistence.Handle (PersistenceHandle(..))

-- | Create tables if they don't exist, then run migrations.
initSchema :: PersistenceHandle -> IO ()
initSchema NoPersistence = pure ()
initSchema (PersistenceHandle conn) = do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS games (\
    \  game_id        TEXT PRIMARY KEY, \
    \  status         TEXT NOT NULL, \
    \  game_state     TEXT NOT NULL, \
    \  sessions       TEXT NOT NULL, \
    \  pending_nobles TEXT, \
    \  created_at     TEXT NOT NULL DEFAULT (datetime('now')), \
    \  updated_at     TEXT NOT NULL DEFAULT (datetime('now')) \
    \)"
  -- Migrations (no-op if already applied)
  _ <- try @SomeException $
    execute_ conn "ALTER TABLE games ADD COLUMN created_at TEXT NOT NULL DEFAULT (datetime('now'))"
  _ <- try @SomeException $
    execute_ conn "DROP TABLE IF EXISTS sessions"
  pure ()
