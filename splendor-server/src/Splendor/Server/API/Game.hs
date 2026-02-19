module Splendor.Server.API.Game
  ( GameAPI
  , gameServer
  ) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict qualified as Map
import Network.WebSockets qualified as WS
import Servant
import Servant.API.WebSocket (WebSocketPending)

import Splendor.Server.GameManager (lookupGame)
import Splendor.Server.Types
import Splendor.Server.WebSocket (handleWebSocket, handleSpectatorWebSocket)

type GameAPI =
       "games" :> Capture "id" GameId :> QueryParam' '[Required] "session" SessionId :> Get '[JSON] PublicGameView
  :<|> "games" :> Capture "id" GameId :> "ws" :> QueryParam' '[Required] "session" SessionId :> WebSocketPending
  :<|> "games" :> Capture "id" GameId :> "spectate" :> WebSocketPending

gameServer :: ServerState -> Server GameAPI
gameServer ss =
       getGameHandler ss
  :<|> wsHandler ss
  :<|> spectateHandler ss

getGameHandler :: ServerState -> GameId -> SessionId -> Handler PublicGameView
getGameHandler ss gid sid = do
  mGameTVar <- liftIO $ lookupGame ss gid
  case mGameTVar of
    Nothing -> throwError err404 { errBody = "Game not found" }
    Just gameTVar -> do
      mg <- liftIO $ readTVarIO gameTVar
      case Map.lookup sid (mgSessions mg) of
        Nothing -> throwError err403 { errBody = "Invalid session" }
        Just ps ->
          let view = toPublicGameView (psPlayerId ps) (aiPlayerIds mg) (mgGameState mg)
          in pure view

wsHandler :: ServerState -> GameId -> SessionId -> WS.PendingConnection -> Handler ()
wsHandler ss gid sid pending = do
  -- Validate game and session before accepting
  mGameTVar <- liftIO $ lookupGame ss gid
  case mGameTVar of
    Nothing -> liftIO $ WS.rejectRequest pending "Game not found"
    Just gameTVar -> do
      mg <- liftIO $ readTVarIO gameTVar
      case Map.lookup sid (mgSessions mg) of
        Nothing -> liftIO $ WS.rejectRequest pending "Invalid session"
        Just _ps -> liftIO $ do
          conn <- WS.acceptRequest pending
          WS.withPingThread conn 30 (pure ()) $
            handleWebSocket ss gid sid conn

spectateHandler :: ServerState -> GameId -> WS.PendingConnection -> Handler ()
spectateHandler ss gid pending = do
  mGameTVar <- liftIO $ lookupGame ss gid
  case mGameTVar of
    Nothing -> liftIO $ WS.rejectRequest pending "Game not found"
    Just gameTVar -> liftIO $ do
      specId <- newUUID
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 30 (pure ()) $
        handleSpectatorWebSocket gameTVar specId conn
