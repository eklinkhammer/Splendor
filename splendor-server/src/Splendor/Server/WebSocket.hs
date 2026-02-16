module Splendor.Server.WebSocket
  ( handleWebSocket
  , handleSpectatorWebSocket
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Exception (finally)
import Data.Aeson (decode, encode)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Network.WebSockets qualified as WS

import Splendor.Core.Rules.ActionValidation (legalActions, legalGemReturns)
import Splendor.Core.Types (currentPlayer, playerId, gsTurnPhase, TurnPhase(..))

import Splendor.Server.GameManager (lookupGame, processAction, processGemReturn,
                                     processNobleChoice, registerConnection,
                                     unregisterConnection, registerSpectator,
                                     unregisterSpectator, resolveSession)
import Splendor.Server.Types

-- | Handle a WebSocket connection for a game.
--   Called after the pending connection has been accepted.
handleWebSocket :: ServerState -> GameId -> SessionId -> WS.Connection -> IO ()
handleWebSocket ss gameId sessionId conn = do
  mGameTVar <- lookupGame ss gameId
  case mGameTVar of
    Nothing -> WS.sendClose conn ("Game not found" :: Text)
    Just gameTVar -> do
      -- Validate session belongs to this game
      mg <- atomically $ readTVar gameTVar
      case resolveSession sessionId mg of
        Nothing -> WS.sendClose conn ("Invalid session for this game" :: Text)
        Just ps -> do
          -- Register connection and get channel
          chan <- atomically $ registerConnection gameTVar sessionId
          -- Send initial state
          sendInitialState conn gameTVar ps
          -- Fork sender thread and wait for it to start
          started <- newEmptyMVar
          senderId <- forkIO $ do
            putMVar started ()
            senderLoop conn chan
          takeMVar started
          -- Run receiver loop, clean up on exit
          finally
            (receiverLoop ss conn gameId sessionId)
            (do killThread senderId
                atomically $ unregisterConnection gameTVar sessionId)

-- | Send the initial game state to a newly connected player.
sendInitialState :: WS.Connection -> TVar ManagedGame -> PlayerSession -> IO ()
sendInitialState conn gameTVar ps = do
  mg <- atomically $ readTVar gameTVar
  let gs = mgGameState mg
      view = toPublicGameView (psPlayerId ps) gs
  WS.sendTextData conn (encode (GameStateUpdate view))
  -- If it's this player's turn, send the appropriate prompt
  case (gsTurnPhase gs, currentPlayer gs) of
    (AwaitingAction, Just cp)
      | playerId cp == psPlayerId ps ->
          case mgPendingNobles mg of
            Just nobles ->
              WS.sendTextData conn (encode (NobleChoiceRequired nobles))
            Nothing -> do
              let actions = legalActions gs
              WS.sendTextData conn (encode (ActionRequired actions))
    (MustReturnGems n, Just cp)
      | playerId cp == psPlayerId ps -> do
          let options = legalGemReturns gs
          WS.sendTextData conn (encode (GemReturnNeeded n options))
    _ -> pure ()

-- | Sender loop: reads from TChan and sends to WebSocket.
senderLoop :: WS.Connection -> TChan ServerMessage -> IO ()
senderLoop conn chan = do
  msg <- atomically $ readTChan chan
  WS.sendTextData conn (encode msg)
  senderLoop conn chan

-- | Receiver loop: reads from WebSocket, parses ClientMessage, dispatches.
receiverLoop :: ServerState -> WS.Connection -> GameId -> SessionId -> IO ()
receiverLoop ss conn gameId sessionId = do
  raw <- WS.receiveData conn
  case decode raw of
    Nothing -> do
      WS.sendTextData conn (encode (ErrorMsg "Invalid message format"))
      receiverLoop ss conn gameId sessionId
    Just clientMsg -> do
      handleClientMessage ss conn gameId sessionId clientMsg
      receiverLoop ss conn gameId sessionId

-- | Dispatch a parsed client message.
handleClientMessage :: ServerState -> WS.Connection -> GameId -> SessionId -> ClientMessage -> IO ()
handleClientMessage ss conn gameId sessionId = \case
  Ping -> WS.sendTextData conn (encode Pong)
  SubmitAction action -> do
    result <- processAction ss gameId sessionId action
    case result of
      Left err -> WS.sendTextData conn (encode (ErrorMsg err))
      Right () -> pure ()
  ReturnGems gems -> do
    result <- processGemReturn ss gameId sessionId gems
    case result of
      Left err -> WS.sendTextData conn (encode (ErrorMsg err))
      Right () -> pure ()
  ChooseNoble nid -> do
    result <- processNobleChoice ss gameId sessionId nid
    case result of
      Left err -> WS.sendTextData conn (encode (ErrorMsg err))
      Right () -> pure ()
  SendChat text -> do
    mGameTVar <- lookupGame ss gameId
    case mGameTVar of
      Nothing -> pure ()
      Just gameTVar -> do
        let truncated = T.take 200 text
        atomically $ do
          mg <- readTVar gameTVar
          case resolveSession sessionId mg of
            Nothing -> pure ()
            Just ps -> do
              let chatMsg = ChatMessage (psPlayerName ps) truncated
              mapM_ (\chan -> writeTChan chan chatMsg) (Map.elems (mgConnections mg))
              mapM_ (\chan -> writeTChan chan chatMsg) (Map.elems (mgSpectators mg))

-- | Handle a spectator WebSocket connection (read-only).
handleSpectatorWebSocket :: TVar ManagedGame -> SpectatorId -> WS.Connection -> IO ()
handleSpectatorWebSocket gameTVar specId conn = do
  chan <- atomically $ registerSpectator gameTVar specId
  -- Send initial spectator view
  mg <- atomically $ readTVar gameTVar
  let view = toSpectatorGameView (mgGameState mg)
  WS.sendTextData conn (encode (GameStateUpdate view))
  -- Fork sender and wait for it to start
  started <- newEmptyMVar
  senderId <- forkIO $ do
    putMVar started ()
    senderLoop conn chan
  takeMVar started
  -- Receiver: only accept Ping, reject everything else
  finally
    (spectatorReceiverLoop conn)
    (do killThread senderId
        atomically $ unregisterSpectator gameTVar specId)

spectatorReceiverLoop :: WS.Connection -> IO ()
spectatorReceiverLoop conn = do
  raw <- WS.receiveData conn
  case decode raw of
    Just Ping -> WS.sendTextData conn (encode Pong)
    _ -> WS.sendTextData conn (encode (ErrorMsg "Spectators cannot perform actions" :: ServerMessage))
  spectatorReceiverLoop conn
