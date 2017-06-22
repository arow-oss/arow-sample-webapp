
module App.Config where

import Network.Wai.Handler.Warp (Port)
import System.ReadEnvVar (readEnvDef)

import App.Environment (Environment(..))

data Config = Config
  { configEnvironment :: Environment
  , configPort :: Port
  } deriving (Eq, Read, Show)

configFromEnv :: IO Config
configFromEnv = do
  env <- readEnvDef "ENV" Development
  port <- readEnvDef "PORT" 8105
  pure $ createConfig env port

createConfig :: Environment -> Port -> Config
createConfig env port = Config env port
