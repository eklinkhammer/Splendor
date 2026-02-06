module Splendor.Server.API
  ( SplendorAPI
  , splendorAPI
  , splendorServer
  ) where

import Servant

import Splendor.Server.API.Game (GameAPI, gameServer)
import Splendor.Server.API.Lobby (LobbyAPI, lobbyServer)
import Splendor.Server.Types (ServerState)

type SplendorAPI = "api" :> "v1" :> (LobbyAPI :<|> GameAPI)

splendorAPI :: Proxy SplendorAPI
splendorAPI = Proxy

splendorServer :: ServerState -> Server SplendorAPI
splendorServer ss = lobbyServer ss :<|> gameServer ss
