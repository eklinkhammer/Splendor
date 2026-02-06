module Splendor.Server.Types
  ( -- * Identifiers
    GameId
  , SessionId
  , LobbyId
    -- * Lobby types
  , LobbySlot(..)
  , LobbyStatus(..)
  , Lobby(..)
  , CreateLobbyRequest(..)
  , CreateLobbyResponse(..)
  , JoinLobbyRequest(..)
  , JoinLobbyResponse(..)
  , StartGameResponse(..)
    -- * Session
  , PlayerSession(..)
    -- * Game status
  , GameStatus(..)
    -- * Public view
  , PublicPlayer(..)
  , PublicGameView(..)
  , toPublicGameView
    -- * WebSocket messages
  , ClientMessage(..)
  , ServerMessage(..)
    -- * Server state
  , ManagedGame(..)
  , ServerState(..)
  , newServerState
  ) where

import Control.Concurrent.STM (TChan, TVar, newTVarIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Splendor.Core.Types

-- -----------------------------------------------------------------
-- Identifiers
-- -----------------------------------------------------------------

type GameId    = Text
type SessionId = Text
type LobbyId   = Text

-- -----------------------------------------------------------------
-- Lobby types
-- -----------------------------------------------------------------

data LobbySlot = LobbySlot
  { lsSessionId  :: SessionId
  , lsPlayerName :: Text
  , lsIsAI       :: Bool
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data LobbyStatus
  = Waiting
  | Started GameId
  | Closed
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Lobby = Lobby
  { lobbyId         :: LobbyId
  , lobbyName       :: Text
  , lobbySlots      :: [LobbySlot]
  , lobbyMaxPlayers :: Int
  , lobbyMinPlayers :: Int
  , lobbyStatus     :: LobbyStatus
  , lobbyCreatedAt  :: UTCTime
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data CreateLobbyRequest = CreateLobbyRequest
  { clrPlayerName :: Text
  , clrLobbyName  :: Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data CreateLobbyResponse = CreateLobbyResponse
  { clrLobbyId   :: LobbyId
  , clrSessionId :: SessionId
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data JoinLobbyRequest = JoinLobbyRequest
  { jlrPlayerName :: Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data JoinLobbyResponse = JoinLobbyResponse
  { jlrSessionId :: SessionId
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data StartGameResponse = StartGameResponse
  { sgrGameId :: GameId
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- -----------------------------------------------------------------
-- Session
-- -----------------------------------------------------------------

data PlayerSession = PlayerSession
  { psSessionId  :: SessionId
  , psPlayerId   :: PlayerId
  , psPlayerName :: Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- -----------------------------------------------------------------
-- Game status
-- -----------------------------------------------------------------

data GameStatus
  = GameActive
  | GameFinished
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- -----------------------------------------------------------------
-- Public view types
-- -----------------------------------------------------------------

data PublicPlayer = PublicPlayer
  { ppPlayerId      :: PlayerId
  , ppPlayerName    :: Text
  , ppTokens        :: GemCollection
  , ppPurchased     :: [Card]
  , ppReservedCount :: Int
  , ppReserved      :: Maybe [Card]  -- Just for self, Nothing for opponents
  , ppNobles        :: [Noble]
  , ppPrestige      :: Int
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PublicGameView = PublicGameView
  { pgvGameId        :: GameId
  , pgvBoard         :: PublicBoard
  , pgvPlayers       :: [PublicPlayer]
  , pgvCurrentPlayer :: Int
  , pgvTurnNumber    :: Int
  , pgvPhase         :: GamePhase
  , pgvTurnPhase     :: TurnPhase
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Build a personalized public view for a specific player.
--   The requesting player sees their own reserved cards; opponents see count only.
toPublicGameView :: PlayerId -> GameState -> PublicGameView
toPublicGameView viewerId gs = PublicGameView
  { pgvGameId        = gsGameId gs
  , pgvBoard         = toPublicBoard (gsBoard gs)
  , pgvPlayers       = map (toPublicPlayer viewerId) (gsPlayers gs)
  , pgvCurrentPlayer = gsCurrentPlayer gs
  , pgvTurnNumber    = gsTurnNumber gs
  , pgvPhase         = gsPhase gs
  , pgvTurnPhase     = gsTurnPhase gs
  }

toPublicPlayer :: PlayerId -> Player -> PublicPlayer
toPublicPlayer viewerId p = PublicPlayer
  { ppPlayerId      = playerId p
  , ppPlayerName    = playerName p
  , ppTokens        = playerTokens p
  , ppPurchased     = playerPurchased p
  , ppReservedCount = length (playerReserved p)
  , ppReserved      = if playerId p == viewerId
                      then Just (playerReserved p)
                      else Nothing
  , ppNobles        = playerNobles p
  , ppPrestige      = playerPrestige p
  }

-- -----------------------------------------------------------------
-- WebSocket messages
-- -----------------------------------------------------------------

data ClientMessage
  = SubmitAction Action
  | ReturnGems GemCollection
  | ChooseNoble NobleId
  | Ping
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ServerMessage
  = GameStateUpdate PublicGameView
  | ActionRequired [Action]
  | GemReturnNeeded Int [GemCollection]
  | NobleChoiceRequired [Noble]
  | GameOverMsg GameResult
  | ErrorMsg Text
  | Pong
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- -----------------------------------------------------------------
-- Server state (STM)
-- -----------------------------------------------------------------

data ManagedGame = ManagedGame
  { mgGameState   :: GameState
  , mgSessions    :: Map SessionId PlayerSession
  , mgConnections :: Map SessionId (TChan ServerMessage)
  , mgStatus      :: GameStatus
  }

data ServerState = ServerState
  { ssLobbies  :: TVar (Map LobbyId Lobby)
  , ssGames    :: TVar (Map GameId (TVar ManagedGame))
  , ssSessions :: TVar (Map SessionId PlayerSession)
  }

newServerState :: IO ServerState
newServerState = ServerState
  <$> newTVarIO Map.empty
  <*> newTVarIO Map.empty
  <*> newTVarIO Map.empty
