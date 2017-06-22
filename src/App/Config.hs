
module App.Config where

import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql
       (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool)
import Network.Wai.Handler.Warp (Port)
import System.ReadEnvVar (lookupEnvDef, readEnvDef)

import App.Environment (Environment(..))

data Config = Config
  { configEnvironment :: Environment
  , configPool :: ConnectionPool
  , configPort :: Port
  } deriving Show

configFromEnv :: IO Config
configFromEnv = do
  env <- readEnvDef "APP_ENV" Development
  port <- readEnvDef "PORT" 8105
  connNum <- readEnvDef "DB_CONN_NUM" 10
  connectionString <-
    lookupEnvDef
      "DB_URL"
      "postgres://appuser:ahah8eh9ahf23hr9aaw3r@localhost:5432/appdb"
  createConfig env port connNum connectionString

createConfig :: Environment -> Port -> Int -> ConnectionString -> IO Config
createConfig env port connNum connectionString = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connectionString connNum
  pure $ Config env pool port
