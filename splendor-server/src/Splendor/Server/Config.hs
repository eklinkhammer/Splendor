module Splendor.Server.Config
  ( Config(..)
  , loadConfig
  ) where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Config = Config
  { configPort   :: Int
  , configDbPath :: Maybe FilePath
  } deriving stock (Show)

loadConfig :: IO Config
loadConfig = do
  portStr <- lookupEnv "SPLENDOR_PORT"
  dbPath  <- lookupEnv "SPLENDOR_DB_PATH"
  let port = fromMaybe 8080 (portStr >>= readMaybe)
  pure Config
    { configPort   = port
    , configDbPath = dbPath
    }
