module Splendor.Server.App
  ( runServer
  ) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors
import Servant (serve)

import Splendor.Server.API (splendorAPI, splendorServer)
import Splendor.Server.Config (Config(..), loadConfig)
import Splendor.Server.Persistence (initPersistence)
import Splendor.Server.Persistence.Schema (initSchema)
import Splendor.Server.Types (newServerState)

runServer :: IO ()
runServer = do
  config <- loadConfig
  _persistence <- initPersistence (configDbPath config)
  initSchema
  ss <- newServerState
  let port = configPort config
  TIO.putStrLn $ "Splendor server starting on port " <> T.pack (show port)
  Warp.run port
    $ corsMiddleware
    $ serve splendorAPI (splendorServer ss)

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
