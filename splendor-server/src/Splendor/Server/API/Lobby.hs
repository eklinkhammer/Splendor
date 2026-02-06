module Splendor.Server.API.Lobby
  ( LobbyAPI
  , lobbyServer
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Servant

import Splendor.Server.GameManager (createGame)
import Splendor.Server.Types

type LobbyAPI =
       "lobbies" :> ReqBody '[JSON] CreateLobbyRequest :> Post '[JSON] CreateLobbyResponse
  :<|> "lobbies" :> Get '[JSON] [Lobby]
  :<|> "lobbies" :> Capture "id" LobbyId :> Get '[JSON] Lobby
  :<|> "lobbies" :> Capture "id" LobbyId :> "join" :> ReqBody '[JSON] JoinLobbyRequest :> Post '[JSON] JoinLobbyResponse
  :<|> "lobbies" :> Capture "id" LobbyId :> "start" :> Post '[JSON] StartGameResponse

lobbyServer :: ServerState -> Server LobbyAPI
lobbyServer ss =
       createLobbyHandler ss
  :<|> listLobbiesHandler ss
  :<|> getLobbyHandler ss
  :<|> joinLobbyHandler ss
  :<|> startGameHandler ss

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
      liftIO $ atomically $ modifyTVar' (ssLobbies ss) $
        Map.adjust (\l -> l { lobbyStatus = Started gameId }) lid
      pure StartGameResponse { sgrGameId = gameId }

encodeUtf8 :: Text -> LBS.ByteString
encodeUtf8 = LBS.fromStrict . TE.encodeUtf8
