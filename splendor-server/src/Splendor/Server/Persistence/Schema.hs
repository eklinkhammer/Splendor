module Splendor.Server.Persistence.Schema
  ( initSchema
  ) where

import Database.SQLite.Simple (execute_)

import Splendor.Server.Persistence.Handle (PersistenceHandle(..))

-- | Create tables if they don't exist.
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
    \  updated_at     TEXT NOT NULL DEFAULT (datetime('now')) \
    \)"
  execute_ conn
    "CREATE TABLE IF NOT EXISTS sessions (\
    \  session_id  TEXT PRIMARY KEY, \
    \  game_id     TEXT NOT NULL REFERENCES games(game_id), \
    \  player_name TEXT NOT NULL, \
    \  is_ai       INTEGER NOT NULL DEFAULT 0 \
    \)"
