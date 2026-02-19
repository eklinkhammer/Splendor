module Splendor.Server.GameManager
  ( createGame
  , processAction
  , processGemReturn
  , processNobleChoice
  , registerConnection
  , unregisterConnection
  , registerSpectator
  , unregisterSpectator
  , lookupGame
  , lookupSession
  , resolveSession
  , storeAIThreads
  ) where

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import System.IO (hPutStrLn, stderr)
import System.Random (newStdGen)

import Splendor.Core.Engine
import Splendor.Core.Rules.ActionValidation (legalActions, legalGemReturns)
import Splendor.Core.Setup (initGameState)
import Splendor.Core.Types

import Splendor.Server.Persistence (saveGame)
import Splendor.Server.Types

-- -----------------------------------------------------------------
-- Game creation
-- -----------------------------------------------------------------

createGame :: ServerState -> [LobbySlot] -> IO GameId
createGame ss slots = do
  gameId <- newUUID
  gen    <- newStdGen
  let playerNames = map lsPlayerName slots
      playerCount = length slots
      gs = initGameState gen gameId playerCount playerNames
      sessions = buildSessions slots gs
  chans <- mapM (\_ -> newTChanIO) slots
  let chanMap = Map.fromList $ zip (map lsSessionId slots) chans
      mg = ManagedGame
        { mgGameState     = gs
        , mgSessions      = sessions
        , mgConnections   = chanMap
        , mgSpectators    = Map.empty
        , mgStatus        = GameActive
        , mgPendingNobles = Nothing
        , mgAIThreads     = []
        }
  gameTVar <- newTVarIO mg
  atomically $ do
    modifyTVar' (ssGames ss) (Map.insert gameId gameTVar)
    -- Register all sessions globally
    modifyTVar' (ssSessions ss) (\m -> Map.union m sessions)
  persistGame ss gameId mg
  pure gameId

buildSessions :: [LobbySlot] -> GameState -> Map.Map SessionId PlayerSession
buildSessions slots gs =
  let players = gsPlayers gs
      pairs = zip slots players
  in Map.fromList
       [ (lsSessionId slot, PlayerSession
           { psSessionId  = lsSessionId slot
           , psPlayerId   = playerId p
           , psPlayerName = lsPlayerName slot
           , psIsAI       = lsIsAI slot
           })
       | (slot, p) <- pairs
       ]

-- -----------------------------------------------------------------
-- Action processing
-- -----------------------------------------------------------------

processAction :: ServerState -> GameId -> SessionId -> Action -> IO (Either Text ())
processAction ss gid sid action = do
  result <- atomically $ do
    mGameTVar <- lookupGameSTM ss gid
    case mGameTVar of
      Nothing -> pure (Left "Game not found")
      Just gameTVar -> do
        mg <- readTVar gameTVar
        case resolveSession sid mg of
          Nothing -> pure (Left "Invalid session")
          Just ps ->
            case applyAction (mgGameState mg) (psPlayerId ps) action of
              StepError err -> pure (Left (T.pack (show err)))
              Advanced gs' -> do
                let mg' = mg { mgGameState = gs', mgPendingNobles = Nothing }
                writeTVar gameTVar mg'
                broadcastGameState mg'
                pure (Right ([], mg'))
              NeedGemReturn gs' n -> do
                let gs'' = gs' { gsTurnPhase = MustReturnGems n }
                    mg' = mg { mgGameState = gs'', mgPendingNobles = Nothing }
                writeTVar gameTVar mg'
                broadcastGameState mg'
                sendGemReturnPrompt mg' n
                pure (Right ([], mg'))
              NeedNobleChoice gs' nobles -> do
                let mg' = mg { mgGameState = gs', mgPendingNobles = Just nobles }
                writeTVar gameTVar mg'
                broadcastGameState mg'
                sendNobleChoicePrompt mg' nobles
                pure (Right ([], mg'))
              GameOver gs' result' -> do
                let mg' = mg { mgGameState = gs', mgStatus = GameFinished, mgPendingNobles = Nothing }
                writeTVar gameTVar mg'
                broadcastGameState mg'
                broadcastMessage mg' (GameOverMsg result')
                tids <- cleanupSessions ss mg'
                pure (Right (tids, mg'))
  case result of
    Left err -> pure (Left err)
    Right (tids, mg') -> do
      persistGame ss gid mg'
      killAIThreads tids
      pure (Right ())

processGemReturn :: ServerState -> GameId -> SessionId -> GemCollection -> IO (Either Text ())
processGemReturn ss gid sid gems = do
  result <- atomically $ do
    mGameTVar <- lookupGameSTM ss gid
    case mGameTVar of
      Nothing -> pure (Left "Game not found")
      Just gameTVar -> do
        mg <- readTVar gameTVar
        case resolveSession sid mg of
          Nothing -> pure (Left "Invalid session")
          Just ps ->
            case applyGemReturn (mgGameState mg) (psPlayerId ps) gems of
              StepError err -> pure (Left (T.pack (show err)))
              Advanced gs' -> do
                let mg' = mg { mgGameState = gs', mgPendingNobles = Nothing }
                writeTVar gameTVar mg'
                broadcastGameState mg'
                pure (Right ([], mg'))
              NeedNobleChoice gs' nobles -> do
                let mg' = mg { mgGameState = gs', mgPendingNobles = Just nobles }
                writeTVar gameTVar mg'
                broadcastGameState mg'
                sendNobleChoicePrompt mg' nobles
                pure (Right ([], mg'))
              GameOver gs' result' -> do
                let mg' = mg { mgGameState = gs', mgStatus = GameFinished, mgPendingNobles = Nothing }
                writeTVar gameTVar mg'
                broadcastGameState mg'
                broadcastMessage mg' (GameOverMsg result')
                tids <- cleanupSessions ss mg'
                pure (Right (tids, mg'))
              NeedGemReturn _ _ -> pure (Left "Unexpected NeedGemReturn after gem return")
  case result of
    Left err -> pure (Left err)
    Right (tids, mg') -> do
      persistGame ss gid mg'
      killAIThreads tids
      pure (Right ())

processNobleChoice :: ServerState -> GameId -> SessionId -> NobleId -> IO (Either Text ())
processNobleChoice ss gid sid nid = do
  result <- atomically $ do
    mGameTVar <- lookupGameSTM ss gid
    case mGameTVar of
      Nothing -> pure (Left "Game not found")
      Just gameTVar -> do
        mg <- readTVar gameTVar
        case resolveSession sid mg of
          Nothing -> pure (Left "Invalid session")
          Just ps ->
            case applyNobleChoice (mgGameState mg) (psPlayerId ps) nid of
              StepError err -> pure (Left (T.pack (show err)))
              Advanced gs' -> do
                let mg' = mg { mgGameState = gs', mgPendingNobles = Nothing }
                writeTVar gameTVar mg'
                broadcastGameState mg'
                pure (Right ([], mg'))
              GameOver gs' result' -> do
                let mg' = mg { mgGameState = gs', mgStatus = GameFinished, mgPendingNobles = Nothing }
                writeTVar gameTVar mg'
                broadcastGameState mg'
                broadcastMessage mg' (GameOverMsg result')
                tids <- cleanupSessions ss mg'
                pure (Right (tids, mg'))
              NeedGemReturn _ _ -> pure (Left "Unexpected NeedGemReturn after noble choice")
              NeedNobleChoice _ _ -> pure (Left "Unexpected NeedNobleChoice after noble choice")
  case result of
    Left err -> pure (Left err)
    Right (tids, mg') -> do
      persistGame ss gid mg'
      killAIThreads tids
      pure (Right ())

-- -----------------------------------------------------------------
-- Connection management
-- -----------------------------------------------------------------

registerConnection :: TVar ManagedGame -> SessionId -> STM (TChan ServerMessage)
registerConnection gameTVar sid = do
  chan <- newTChan
  modifyTVar' gameTVar $ \mg ->
    mg { mgConnections = Map.insert sid chan (mgConnections mg) }
  pure chan

unregisterConnection :: TVar ManagedGame -> SessionId -> STM ()
unregisterConnection gameTVar sid =
  modifyTVar' gameTVar $ \mg ->
    mg { mgConnections = Map.delete sid (mgConnections mg) }

registerSpectator :: TVar ManagedGame -> SpectatorId -> STM (TChan ServerMessage)
registerSpectator gameTVar specId = do
  chan <- newTChan
  modifyTVar' gameTVar $ \mg ->
    mg { mgSpectators = Map.insert specId chan (mgSpectators mg) }
  pure chan

unregisterSpectator :: TVar ManagedGame -> SpectatorId -> STM ()
unregisterSpectator gameTVar specId =
  modifyTVar' gameTVar $ \mg ->
    mg { mgSpectators = Map.delete specId (mgSpectators mg) }

-- -----------------------------------------------------------------
-- Lookup helpers
-- -----------------------------------------------------------------

lookupGame :: ServerState -> GameId -> IO (Maybe (TVar ManagedGame))
lookupGame ss gid = atomically $ lookupGameSTM ss gid

lookupGameSTM :: ServerState -> GameId -> STM (Maybe (TVar ManagedGame))
lookupGameSTM ss gid = do
  games <- readTVar (ssGames ss)
  pure (Map.lookup gid games)

lookupSession :: ServerState -> SessionId -> IO (Maybe PlayerSession)
lookupSession ss sid = atomically $ do
  sessions <- readTVar (ssSessions ss)
  pure (Map.lookup sid sessions)

-- -----------------------------------------------------------------
-- Internal helpers
-- -----------------------------------------------------------------

resolveSession :: SessionId -> ManagedGame -> Maybe PlayerSession
resolveSession sid mg = Map.lookup sid (mgSessions mg)

-- | Broadcast personalized game state to each connected player.
broadcastGameState :: ManagedGame -> STM ()
broadcastGameState mg = do
  let gs = mgGameState mg
      aiIds = aiPlayerIds mg
  mapM_ (\(sid, chan) ->
    case resolveSession sid mg of
      Nothing -> pure ()
      Just ps -> do
        let view = toPublicGameView (psPlayerId ps) aiIds gs
        writeTChan chan (GameStateUpdate view)
        -- If it's this player's turn in AwaitingAction, send legal moves
        case (gsTurnPhase gs, currentPlayer gs) of
          (AwaitingAction, Just cp)
            | playerId cp == psPlayerId ps ->
                let actions = legalActions gs
                in writeTChan chan (ActionRequired actions)
          _ -> pure ()
    ) (Map.toList (mgConnections mg))
  -- Send spectator view (no reserved cards visible)
  let spectatorView = toSpectatorGameView aiIds gs
  mapM_ (\chan -> writeTChan chan (GameStateUpdate spectatorView))
    (Map.elems (mgSpectators mg))

-- | Send gem return prompt to the current player.
sendGemReturnPrompt :: ManagedGame -> Int -> STM ()
sendGemReturnPrompt mg n = do
  let gs = mgGameState mg
      options = legalGemReturns gs
  case currentPlayer gs of
    Nothing -> pure ()
    Just cp ->
      mapM_ (\(sid, chan) ->
        case resolveSession sid mg of
          Just ps | psPlayerId ps == playerId cp ->
            writeTChan chan (GemReturnNeeded n options)
          _ -> pure ()
        ) (Map.toList (mgConnections mg))

-- | Send noble choice prompt to the current player.
sendNobleChoicePrompt :: ManagedGame -> [Noble] -> STM ()
sendNobleChoicePrompt mg nobles = do
  let gs = mgGameState mg
  case currentPlayer gs of
    Nothing -> pure ()
    Just cp ->
      mapM_ (\(sid, chan) ->
        case resolveSession sid mg of
          Just ps | psPlayerId ps == playerId cp ->
            writeTChan chan (NobleChoiceRequired nobles)
          _ -> pure ()
        ) (Map.toList (mgConnections mg))

-- | Broadcast a message to all connected players and spectators.
broadcastMessage :: ManagedGame -> ServerMessage -> STM ()
broadcastMessage mg msg = do
  mapM_ (\chan -> writeTChan chan msg) (Map.elems (mgConnections mg))
  mapM_ (\chan -> writeTChan chan msg) (Map.elems (mgSpectators mg))

-- | Remove a game's sessions from the global session map.
--   Returns AI thread IDs that should be killed in IO after the transaction.
cleanupSessions :: ServerState -> ManagedGame -> STM [ThreadId]
cleanupSessions ss mg = do
  modifyTVar' (ssSessions ss) $ \sessions ->
    foldr Map.delete sessions (Map.keys (mgSessions mg))
  pure (mgAIThreads mg)

-- | Kill AI threads safely (ignores exceptions from already-dead threads).
killAIThreads :: [ThreadId] -> IO ()
killAIThreads = mapM_ (\tid -> try @SomeException (killThread tid) >> pure ())

-- | Store AI thread IDs in the managed game.
storeAIThreads :: ServerState -> GameId -> [ThreadId] -> IO ()
storeAIThreads ss gid tids = atomically $ do
  mGameTVar <- lookupGameSTM ss gid
  case mGameTVar of
    Nothing -> pure ()
    Just gameTVar ->
      modifyTVar' gameTVar $ \mg -> mg { mgAIThreads = tids }

-- | Persist a managed game to the database (fire-and-forget; logs errors to stderr).
persistGame :: ServerState -> GameId -> ManagedGame -> IO ()
persistGame ss gid mg = do
  result <- try @SomeException $
    saveGame (ssPersistence ss) gid (mgGameState mg) (mgStatus mg) (mgSessions mg) (mgPendingNobles mg)
  case result of
    Right () -> pure ()
    Left ex  -> hPutStrLn stderr $ "persistGame: failed for game " ++ show gid ++ ": " ++ show ex

