module Splendor.Server.Persistence
  ( PersistenceHandle(..)
  , initPersistence
  , saveGame
  , loadAllActiveGames
  ) where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Database.SQLite.Simple
import System.IO (hPutStrLn, stderr)

import Splendor.Core.Types (GameState, Noble)

import Splendor.Server.Persistence.Handle (PersistenceHandle(..))
import Splendor.Server.Types (GameId, SessionId, PlayerSession(..), GameStatus(..))

-- | Initialize the persistence layer. Opens a SQLite connection and enables
--   WAL mode + foreign keys. Returns 'NoPersistence' if no path is given.
initPersistence :: Maybe FilePath -> IO PersistenceHandle
initPersistence Nothing   = pure NoPersistence
initPersistence (Just fp) = do
  conn <- open fp
  execute_ conn "PRAGMA journal_mode=WAL"
  execute_ conn "PRAGMA foreign_keys=ON"
  pure (PersistenceHandle conn)

-- | Persist the current state of a game (UPSERT).
saveGame :: PersistenceHandle -> GameId -> GameState -> GameStatus -> Map SessionId PlayerSession -> Maybe [Noble] -> IO ()
saveGame NoPersistence _ _ _ _ _ = pure ()
saveGame (PersistenceHandle conn) gid gs status sessions pendingNobles =
  withTransaction conn $ do
    let statusText :: Text
        statusText = case status of
          GameActive   -> "active"
          GameFinished -> "finished"
        gsJson       = TE.decodeUtf8 (LBS.toStrict (encode gs))
        sessionsJson = TE.decodeUtf8 (LBS.toStrict (encode sessions))
        noblesJson   = TE.decodeUtf8 . LBS.toStrict . encode <$> pendingNobles
    -- UPSERT game row
    execute conn
      "INSERT INTO games (game_id, status, game_state, sessions, pending_nobles, updated_at) \
      \VALUES (?, ?, ?, ?, ?, datetime('now')) \
      \ON CONFLICT(game_id) DO UPDATE SET \
      \  status = excluded.status, \
      \  game_state = excluded.game_state, \
      \  sessions = excluded.sessions, \
      \  pending_nobles = excluded.pending_nobles, \
      \  updated_at = excluded.updated_at"
      (gid, statusText, gsJson, sessionsJson, noblesJson)

-- | Load all active games from the database.
loadAllActiveGames :: PersistenceHandle -> IO [(GameId, GameState, Map SessionId PlayerSession, Maybe [Noble])]
loadAllActiveGames NoPersistence = pure []
loadAllActiveGames (PersistenceHandle conn) = do
  rows <- query_ conn
    "SELECT game_id, game_state, sessions, pending_nobles \
    \FROM games WHERE status = 'active'"
    :: IO [(Text, Text, Text, Maybe Text)]
  catMaybes <$> mapM decodeRow rows
  where
    decodeRow (gid, gsText, sessText, noblesText) =
      case decode (LBS.fromStrict (TE.encodeUtf8 gsText)) of
        Nothing -> do
          hPutStrLn stderr $ "loadAllActiveGames: failed to decode game_state for game " ++ show gid
          pure Nothing
        Just gs ->
          case decode (LBS.fromStrict (TE.encodeUtf8 sessText)) of
            Nothing -> do
              hPutStrLn stderr $ "loadAllActiveGames: failed to decode sessions for game " ++ show gid
              pure Nothing
            Just sess -> do
              let nobles = noblesText >>= decode . LBS.fromStrict . TE.encodeUtf8
              pure (Just (gid, gs, sess, nobles))
