module Splendor.Server.App
  ( runServer
  , mkApp
  ) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors
import Servant (serve)

import Splendor.Server.API (splendorAPI, splendorServer)
import Splendor.Server.Config (Config(..), loadConfig)
import Splendor.Server.Persistence (initPersistence)
import Splendor.Server.Persistence.Schema (initSchema)
import Splendor.Server.Restore (restoreGames)
import Splendor.Server.Types (ServerState, newServerState)

mkApp :: ServerState -> Application
mkApp ss = serve splendorAPI (splendorServer ss)

runServer :: IO ()
runServer = do
  config <- loadConfig
  persistence <- initPersistence (configDbPath config)
  initSchema persistence
  ss <- newServerState persistence
  restoreGames ss
  let port = configPort config
  TIO.putStrLn $ "Splendor server starting on port " <> T.pack (show port)
  Warp.run port
    $ corsMiddleware
    $ mkApp ss

corsMiddleware :: Middleware
corsMiddleware = cors $ const $ Just CorsResourcePolicy
  { corsOrigins        = Nothing  -- allow all origins
  , corsMethods        = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , corsRequestHeaders = ["Content-Type", "Authorization"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Just 86400
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
  }
