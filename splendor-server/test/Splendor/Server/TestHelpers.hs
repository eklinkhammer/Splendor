module Splendor.Server.TestHelpers
  ( -- * Game setup
    setupGame
  , setupGame3
    -- * Lookups
  , lookupGameTVarOrFail
  , lookupGameOrFail
    -- * Session helpers
  , currentSession
  , wrongSession
    -- * Data helpers
  , replaceAt
  , mkTokens
    -- * Message helpers
  , drainChan
  , isJustGameStateUpdate
  , isJustActionRequired
  , msgTag
    -- * Handler helpers
  , run
  , createH
  , listH
  , getH
  , joinH
  , startH
  , getGameH
  ) where

import Control.Concurrent.STM
import Data.Map.Strict qualified as Map
import Servant (Handler, (:<|>)(..))
import Servant qualified
import Splendor.Core.Types
import Splendor.Server.API.Game (gameServer)
import Splendor.Server.API.Lobby (lobbyServer)
import Splendor.Server.Types
import Splendor.Server.GameManager

-- ============================================================
-- Game setup
-- ============================================================

-- | Create a ServerState with a 2-player game, returning identifiers.
setupGame :: IO (ServerState, GameId, SessionId, SessionId)
setupGame = do
  ss <- newServerState
  let s1 = "session-1"
      s2 = "session-2"
      slots = [ LobbySlot s1 "Alice" False
              , LobbySlot s2 "Bob" False
              ]
  gid <- createGame ss slots
  pure (ss, gid, s1, s2)

-- | Create a ServerState with a 3-player game, returning identifiers.
setupGame3 :: IO (ServerState, GameId, SessionId, SessionId, SessionId)
setupGame3 = do
  ss <- newServerState
  let s1 = "session-1"
      s2 = "session-2"
      s3 = "session-3"
      slots = [ LobbySlot s1 "Alice" False
              , LobbySlot s2 "Bob" False
              , LobbySlot s3 "Charlie" False
              ]
  gid <- createGame ss slots
  pure (ss, gid, s1, s2, s3)

-- ============================================================
-- Lookups
-- ============================================================

-- | Look up a game TVar, failing the test if not found.
lookupGameTVarOrFail :: ServerState -> GameId -> IO (TVar ManagedGame)
lookupGameTVarOrFail ss gid = do
  mGame <- lookupGame ss gid
  case mGame of
    Just tv -> pure tv
    Nothing -> error "Game not found (test setup failure)"

-- | Read ManagedGame, failing the test if game not found.
lookupGameOrFail :: ServerState -> GameId -> IO ManagedGame
lookupGameOrFail ss gid = lookupGameTVarOrFail ss gid >>= readTVarIO

-- ============================================================
-- Session helpers
-- ============================================================

-- | Get the session ID of the current player.
currentSession :: ServerState -> GameId -> SessionId -> SessionId -> IO SessionId
currentSession ss gid s1 s2 = do
  mg <- lookupGameOrFail ss gid
  let curIdx = gsCurrentPlayer (mgGameState mg)
  pure $ if curIdx == 0 then s1 else s2

-- | Get the session ID of the non-current player.
wrongSession :: ServerState -> GameId -> SessionId -> SessionId -> IO SessionId
wrongSession ss gid s1 s2 = do
  mg <- lookupGameOrFail ss gid
  let curIdx = gsCurrentPlayer (mgGameState mg)
  pure $ if curIdx == 0 then s2 else s1

-- ============================================================
-- Data helpers
-- ============================================================

-- | Replace the element at a given index in a list.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx x xs = take idx xs ++ [x] ++ drop (idx + 1) xs

-- | Build a GemCollection from a list of (TokenType, Int) pairs.
mkTokens :: [(TokenType, Int)] -> GemCollection
mkTokens = GemCollection . Map.fromList

-- ============================================================
-- Message helpers
-- ============================================================

-- | Drain all available messages from a TChan.
drainChan :: TChan ServerMessage -> IO [ServerMessage]
drainChan chan = do
  mMsg <- atomically $ tryReadTChan chan
  case mMsg of
    Nothing  -> pure []
    Just msg -> (msg :) <$> drainChan chan

isJustGameStateUpdate :: Maybe ServerMessage -> Bool
isJustGameStateUpdate (Just (GameStateUpdate _)) = True
isJustGameStateUpdate _                          = False

isJustActionRequired :: Maybe ServerMessage -> Bool
isJustActionRequired (Just (ActionRequired _)) = True
isJustActionRequired _                         = False

-- | Short tag for a ServerMessage (for error reporting).
msgTag :: ServerMessage -> String
msgTag (GameStateUpdate _)    = "GameStateUpdate"
msgTag (ActionRequired _)     = "ActionRequired"
msgTag (GemReturnNeeded n _)  = "GemReturnNeeded(" ++ show n ++ ")"
msgTag (NobleChoiceRequired _)= "NobleChoiceRequired"
msgTag (GameOverMsg _)        = "GameOverMsg"
msgTag (ErrorMsg t)           = "ErrorMsg(" ++ show t ++ ")"
msgTag Pong                   = "Pong"

-- ============================================================
-- Handler helpers
-- ============================================================

-- | Run a Handler, failing the test on ServerError.
run :: Handler a -> IO a
run h = do
  result <- Servant.runHandler h
  case result of
    Right a  -> pure a
    Left err -> error $ "Handler failed: " ++ show err

createH :: ServerState -> CreateLobbyRequest -> Handler CreateLobbyResponse
createH ss = let (h :<|> _) = lobbyServer ss in h

listH :: ServerState -> Handler [Lobby]
listH ss = let (_ :<|> h :<|> _) = lobbyServer ss in h

getH :: ServerState -> LobbyId -> Handler Lobby
getH ss = let (_ :<|> _ :<|> h :<|> _) = lobbyServer ss in h

joinH :: ServerState -> LobbyId -> JoinLobbyRequest -> Handler JoinLobbyResponse
joinH ss = let (_ :<|> _ :<|> _ :<|> h :<|> _) = lobbyServer ss in h

startH :: ServerState -> LobbyId -> Handler StartGameResponse
startH ss = let (_ :<|> _ :<|> _ :<|> _ :<|> h) = lobbyServer ss in h

getGameH :: ServerState -> GameId -> SessionId -> Handler PublicGameView
getGameH ss = let (h :<|> _) = gameServer ss in h
