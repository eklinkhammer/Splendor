module Splendor.Server.API.Lobby
  ( LobbyAPI
  , lobbyServer
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Servant

import Splendor.Server.AIRunner (spawnAIPlayers)
import Splendor.Server.GameManager (createGame, storeAIThreads)
import Splendor.Server.Types

type LobbyAPI =
       "lobbies" :> ReqBody '[JSON] CreateLobbyRequest :> Post '[JSON] CreateLobbyResponse
  :<|> "lobbies" :> Get '[JSON] [Lobby]
  :<|> "lobbies" :> Capture "id" LobbyId :> Get '[JSON] Lobby
  :<|> "lobbies" :> Capture "id" LobbyId :> "join" :> ReqBody '[JSON] JoinLobbyRequest :> Post '[JSON] JoinLobbyResponse
  :<|> "lobbies" :> Capture "id" LobbyId :> "start" :> Post '[JSON] StartGameResponse
  :<|> "lobbies" :> Capture "id" LobbyId :> "add-ai" :> Post '[JSON] LobbySlot
  :<|> "lobbies" :> Capture "id" LobbyId :> "leave" :> QueryParam' '[Required] "session" SessionId :> Post '[JSON] NoContent

lobbyServer :: ServerState -> Server LobbyAPI
lobbyServer ss =
       createLobbyHandler ss
  :<|> listLobbiesHandler ss
  :<|> getLobbyHandler ss
  :<|> joinLobbyHandler ss
  :<|> startGameHandler ss
  :<|> addAIHandler ss
  :<|> leaveLobbyHandler ss

createLobbyHandler :: ServerState -> CreateLobbyRequest -> Handler CreateLobbyResponse
createLobbyHandler ss req = do
  lid       <- liftIO newUUID
  sid       <- liftIO newUUID
  now       <- liftIO getCurrentTime
  let slot = LobbySlot
        { lsSessionId  = sid
        , lsPlayerName = clrPlayerName req
        , lsIsAI       = False
        }
      lobby = Lobby
        { lobbyId         = lid
        , lobbyName       = clrLobbyName req
        , lobbySlots      = [slot]
        , lobbyMaxPlayers = 4
        , lobbyMinPlayers = 2
        , lobbyStatus     = Waiting
        , lobbyCreatedAt  = now
        }
  liftIO $ atomically $
    modifyTVar' (ssLobbies ss) (Map.insert lid lobby)
  pure CreateLobbyResponse
    { clrLobbyId   = lid
    , clrSessionId = sid
    }

listLobbiesHandler :: ServerState -> Handler [Lobby]
listLobbiesHandler ss =
  liftIO $ Map.elems <$> readTVarIO (ssLobbies ss)

getLobbyHandler :: ServerState -> LobbyId -> Handler Lobby
getLobbyHandler ss lid = do
  lobbies <- liftIO $ readTVarIO (ssLobbies ss)
  case Map.lookup lid lobbies of
    Nothing    -> throwError err404 { errBody = "Lobby not found" }
    Just lobby -> pure lobby

joinLobbyHandler :: ServerState -> LobbyId -> JoinLobbyRequest -> Handler JoinLobbyResponse
joinLobbyHandler ss lid req = do
  sid <- liftIO newUUID
  result <- liftIO $ atomically $ do
    lobbies <- readTVar (ssLobbies ss)
    case Map.lookup lid lobbies of
      Nothing -> pure (Left "Lobby not found")
      Just lobby ->
        case lobbyStatus lobby of
          Waiting
            | length (lobbySlots lobby) >= lobbyMaxPlayers lobby ->
                pure (Left "Lobby is full")
            | otherwise -> do
                let slot = LobbySlot
                      { lsSessionId  = sid
                      , lsPlayerName = jlrPlayerName req
                      , lsIsAI       = False
                      }
                    lobby' = lobby { lobbySlots = lobbySlots lobby ++ [slot] }
                modifyTVar' (ssLobbies ss) (Map.insert lid lobby')
                pure (Right sid)
          _ -> pure (Left "Lobby is not accepting players")
  case result of
    Left err -> throwError err400 { errBody = encodeUtf8 err }
    Right s  -> pure JoinLobbyResponse { jlrSessionId = s }

startGameHandler :: ServerState -> LobbyId -> Handler StartGameResponse
startGameHandler ss lid = do
  -- Atomically validate and set transitional Starting status to prevent races
  mLobby <- liftIO $ atomically $ do
    lobbies <- readTVar (ssLobbies ss)
    case Map.lookup lid lobbies of
      Nothing -> pure (Left "Lobby not found")
      Just lobby ->
        case lobbyStatus lobby of
          Waiting
            | length (lobbySlots lobby) < lobbyMinPlayers lobby ->
                pure (Left "Not enough players")
            | otherwise -> do
                modifyTVar' (ssLobbies ss) $
                  Map.adjust (\l -> l { lobbyStatus = Starting }) lid
                pure (Right lobby)
          _ -> pure (Left "Game already started or lobby closed")
  case mLobby of
    Left err -> throwError err400 { errBody = encodeUtf8 err }
    Right lobby -> do
      gameId <- liftIO $ createGame ss (lobbySlots lobby)
      -- Spawn AI player threads for any AI slots
      let aiSlots = filter lsIsAI (lobbySlots lobby)
      liftIO $ do
        -- Look up the AI sessions from the managed game
        mGameTVar <- atomically $ do
          games <- readTVar (ssGames ss)
          pure (Map.lookup gameId games)
        case mGameTVar of
          Just gameTVar -> do
            mg <- readTVarIO gameTVar
            let aiSessions = [ (lsSessionId slot, ps)
                             | slot <- aiSlots
                             , Just ps <- [Map.lookup (lsSessionId slot) (mgSessions mg)]
                             ]
            tids <- spawnAIPlayers ss gameId aiSessions
            storeAIThreads ss gameId tids
          Nothing -> pure ()  -- shouldn't happen
      liftIO $ atomically $ modifyTVar' (ssLobbies ss) $
        Map.adjust (\l -> l { lobbyStatus = Started gameId }) lid
      pure StartGameResponse { sgrGameId = gameId }

addAIHandler :: ServerState -> LobbyId -> Handler LobbySlot
addAIHandler ss lid = do
  sid <- liftIO newUUID
  result <- liftIO $ atomically $ do
    lobbies <- readTVar (ssLobbies ss)
    case Map.lookup lid lobbies of
      Nothing -> pure (Left "Lobby not found")
      Just lobby ->
        case lobbyStatus lobby of
          Waiting
            | length (lobbySlots lobby) >= lobbyMaxPlayers lobby ->
                pure (Left "Lobby is full")
            | otherwise -> do
                let aiCount = length (filter lsIsAI (lobbySlots lobby))
                    aiName = "AI Player " <> T.pack (show (aiCount + 1))
                    slot = LobbySlot
                      { lsSessionId  = sid
                      , lsPlayerName = aiName
                      , lsIsAI       = True
                      }
                    lobby' = lobby { lobbySlots = lobbySlots lobby ++ [slot] }
                modifyTVar' (ssLobbies ss) (Map.insert lid lobby')
                pure (Right slot)
          _ -> pure (Left "Lobby is not accepting players")
  case result of
    Left err -> throwError err400 { errBody = encodeUtf8 err }
    Right slot -> pure slot

leaveLobbyHandler :: ServerState -> LobbyId -> SessionId -> Handler NoContent
leaveLobbyHandler ss lid sid = do
  result <- liftIO $ atomically $ do
    lobbies <- readTVar (ssLobbies ss)
    case Map.lookup lid lobbies of
      Nothing -> pure (Left "Lobby not found")
      Just lobby ->
        case lobbyStatus lobby of
          Waiting -> do
            let isCreator = case lobbySlots lobby of
                              (s:_) -> lsSessionId s == sid
                              []    -> False
                slots' = filter (\s -> lsSessionId s /= sid) (lobbySlots lobby)
            if length slots' == length (lobbySlots lobby)
              then pure (Left "Session not in lobby")
              else if isCreator
                then do
                  modifyTVar' (ssLobbies ss) $ Map.delete lid
                  pure (Right ())
                else do
                  modifyTVar' (ssLobbies ss) $
                    Map.adjust (\l -> l { lobbySlots = slots' }) lid
                  pure (Right ())
          _ -> pure (Left "Cannot leave: game already started")
  case result of
    Left err -> throwError err400 { errBody = encodeUtf8 err }
    Right () -> pure NoContent

encodeUtf8 :: Text -> LBS.ByteString
encodeUtf8 = LBS.fromStrict . TE.encodeUtf8
