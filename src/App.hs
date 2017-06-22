module App where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Natural ((:~>)(NT))
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import Network.Wai (Application, Middleware, Request)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
       (logStdout, logStdoutDev)
import Database.Persist.Sql (runSqlPool)
import Servant
       ((:>), (:<|>)(..), Context(..), Get, Handler, JSON, Post,
        ServantErr, Server, ServerT, enter, serve)
import Servant.Server.Experimental.Auth (AuthHandler)

import App.Config
       (Config(configEnvironment, configPool, configPort), configFromEnv)
import App.Db (doMigrations)
import App.Environment
       (Environment(Development, Production, Testing))
import App.Monad (AppM)

requestLoggerMiddleware :: Environment -> Middleware
requestLoggerMiddleware Testing = id
requestLoggerMiddleware Development = logStdoutDev
requestLoggerMiddleware Production = logStdout

setup :: IO (Config, Middleware)
setup = do
  config <- configFromEnv
  let loggerMiddleware = requestLoggerMiddleware (configEnvironment config)
  return (config, loggerMiddleware)

defaultMainApi :: IO ()
defaultMainApi = do
  (config, loggerMiddleware) <- setup
  runSqlPool doMigrations $ configPool config
  let port = configPort config
  putStrLn $ "app running on port " <> show (configPort config)
  run port . loggerMiddleware $ app config

type Api = "v0" :> (ApiSearch :<|> ApiStatus)

type ApiSearch = "search" :> Post '[JSON] String

type ApiStatus = "status" :> Get '[JSON] Int

serverRoot :: ServerT Api AppM
serverRoot = search :<|> status

search :: AppM String
search = do
  eitherStatuses <- undefined
  case eitherStatuses of
    Left twitterErr -> undefined
    Right statuses -> undefined

status :: AppM Int
status = pure 1

-- | Given a 'Config', this returns a Wai 'Application'.
app :: Config -> Application
app config = serve (Proxy :: Proxy Api) $ apiServer config

-- | Given a 'Config', this returns a servant 'Server' for 'Api'
apiServer :: Config -> Server Api
apiServer config = enter natTrans serverRoot
  where
    natTrans :: AppM :~> Handler
    natTrans = NT trans

    trans :: forall a. AppM a -> Handler a
    trans appM = runReaderT appM config
