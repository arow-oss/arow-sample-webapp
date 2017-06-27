module App where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Natural ((:~>)(NT))
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import GHC.Generics (Generic)
import Network.Wai (Application, Middleware, Request)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
       (logStdout, logStdoutDev)
import Database.Persist.Sql (runSqlPool)
import Servant
       ((:>), (:<|>)(..), Context(..), Get, Handler, Header, Headers,
        JSON, NoContent(NoContent), Post, PostNoContent, ReqBody,
        ServantErr, Server, ServerT, enter, err401, serve,
        serveWithContext)
import Servant.Auth.Server
       (Auth, AuthResult(..), Cookie, CookieSettings(cookieIsSecure),
        FromJWT, JWT, JWTSettings, IsSecure(NotSecure), SetCookie, ToJWT,
        acceptLogin, defaultCookieSettings, defaultJWTSettings,
        generateKey, throwAll)
import Servant.Auth.Server.SetCookieOrphan ()
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
  putStrLn $ "app running on port " <> show (configPort config)
  run (configPort config) . loggerMiddleware $ app config

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

----------------
-- Auth stuff --
----------------

data User = User { name :: String, email :: String }
   deriving (Eq, Generic, Read, Show)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

data Login = Login { username :: String, password :: String }
   deriving (Eq, Generic, Read, Show)

instance ToJSON Login
instance FromJSON Login


type Protected
     = "name" :> Get '[JSON] String
  :<|> "email" :> Get '[JSON] String

type ApiLogin =
  "login"
    :> ReqBody '[JSON] Login
    :> PostNoContent
        '[JSON]
        (Headers
          '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
          NoContent
        )

-- | 'Protected' will be protected by 'auths', which we still have to specify.
protected :: AuthResult User -> Server Protected
protected (Authenticated user) = return (name user) :<|> return (email user)
protected BadPassword = undefined :<|> (liftIO (print "badpassword") *> throwError err401)
protected NoSuchUser = undefined :<|> (liftIO (print "no such user") *> throwError err401)
protected Indefinite = undefined :<|> (liftIO (print "indefinite") *> throwError err401)

login
  :: CookieSettings
  -> JWTSettings
  -> Login
  -> Handler
      (Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        NoContent
      )
login cookieSettings jwtSettings (Login "Ali Baba" "Open Sesame") = do
  let usr = User "Ali Baba" "ali@email.com"
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
  case mApplyCookies of
    Nothing           -> do
      liftIO $ print "error with mApplyCookies"
      throwError err401
    Just applyCookies -> return $ applyCookies NoContent
login cookieSettings jwtSettings login' = do
  liftIO $ print login'
  throwError err401

type TestApi auths = (Auth auths User :> Protected) :<|> ApiLogin

server :: CookieSettings -> JWTSettings -> Server (TestApi auths)
server cs jwts = protected :<|> login cs jwts

-- In main, we fork the server, and allow new tokens to be created in the
-- command line for the specified user name and email.
mainWithJWT :: IO ()
mainWithJWT = do
  -- We generate the key for signing tokens. This would generally be persisted,
  -- and kept safely
  myKey <- generateKey
  -- Adding some configurations. All authentications require CookieSettings to
  -- be in the context.
  let jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings
      cfg = cookieCfg :. jwtCfg :. EmptyContext
      --- Here we actually make concrete
      api = Proxy :: Proxy (TestApi '[JWT])
  undefined
  -- _ <-
  --   forkIO $
  --     run 7249 $
  --       serveWithContext api cfg (server cookieCfg jwtCfg)

  -- putStrLn "Started server on localhost:7249"
  -- putStrLn "Enter name and email separated by a space for a new token"

  -- forever $ do
  --    xs <- words <$> getLine
  --    case xs of
  --      [name', email'] -> do
  --        etoken <- makeJWT (User name' email') jwtCfg Nothing
  --        case etoken of
  --          Left e -> putStrLn $ "Error generating token:t" ++ show e
  --          Right v -> putStrLn $ "New token:\t" ++ show v
  --      _ -> putStrLn "Expecting a name and email separated by spaces"


mainWithCookies :: IO ()
mainWithCookies = do
  -- We *also* need a key to sign the cookies
  myKey <- generateKey
  -- Adding some configurations. 'Cookie' requires, in addition to
  -- CookieSettings, JWTSettings (for signing), so everything is just as before
  let jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings
      cfg = cookieCfg :. jwtCfg :. EmptyContext
      --- Here is the actual change
      api = Proxy :: Proxy (TestApi '[Cookie])
  run 7250 $ serveWithContext api cfg (server cookieCfg jwtCfg)
