module App.Server where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, reader, runReaderT)
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

import App.Config (Config(..), configFromEnv)
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

defaultMainServer :: IO ()
defaultMainServer = do
  (config, loggerMiddleware) <- setup
  runSqlPool doMigrations $ configPool config
  putStrLn $ "app running on port " <> show (configPort config)
  -- run 7250 $ serveWithContext api context (server cookieCfg jwtCfg)
  run (configPort config) . loggerMiddleware $ app config
  -- mainWithCookies

type Api (auths :: [*]) = "v0" :> (ApiLogin :<|> ApiSearch :<|> ApiStatus)

type ApiSearch = "search" :> Post '[JSON] String

type ApiStatus = "status" :> Get '[JSON] Int

type ApiLogin =
  "login"
    :> ReqBody '[JSON] Login
    :> PostNoContent
        '[JSON]
        (Headers
          '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
          NoContent
        )

serverRoot :: ServerT (Api auths) AppM
serverRoot = login :<|> search :<|> status

search :: AppM String
search = do
  eitherStatuses <- undefined
  case eitherStatuses of
    Left twitterErr -> undefined
    Right statuses -> undefined

status :: AppM Int
status = pure 1

login
  :: Login
  -> AppM
      (Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        NoContent
      )
login (Login "Ali Baba" "Open Sesame") = do
  -- TODO: Rewrite this to actually try logging in the real user.
  cookieSettings <- reader configCookieSettings
  jwtSettings <- reader configJWTSettings
  let usr = User "Ali Baba" "ali@email.com"
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
  case mApplyCookies of
    Nothing           -> do
      liftIO $ print "error with mApplyCookies"
      throwError err401
    Just applyCookies -> return $ applyCookies NoContent
login _ = throwError err401

-- | Given a 'Config', this returns a Wai 'Application'.
app :: Config -> Application
app config =
  let context =
        configJWTSettings config :. configCookieSettings config :. EmptyContext
      api = apiServer config
  in serveWithContext (Proxy :: Proxy (Api '[Cookie])) context api

-- | Given a 'Config', this returns a servant 'Server' for 'Api'
apiServer :: Config -> Server (Api auths)
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

-- | 'Protected' will be protected by 'auths', which we still have to specify.
protected :: AuthResult User -> Server Protected
protected (Authenticated user) = return (name user) :<|> return (email user)
protected BadPassword = undefined :<|> (liftIO (print "badpassword") *> throwError err401)
protected NoSuchUser = undefined :<|> (liftIO (print "no such user") *> throwError err401)
protected Indefinite = undefined :<|> (liftIO (print "indefinite") *> throwError err401)

login'
  :: CookieSettings
  -> JWTSettings
  -> Login
  -> Handler
      (Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        NoContent
      )
login' cookieSettings jwtSettings (Login "Ali Baba" "Open Sesame") = do
  let usr = User "Ali Baba" "ali@email.com"
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
  case mApplyCookies of
    Nothing           -> do
      liftIO $ print "error with mApplyCookies"
      throwError err401
    Just applyCookies -> return $ applyCookies NoContent
login' cookieSettings jwtSettings login' = do
  liftIO $ print login'
  throwError err401

type TestApi auths = (Auth auths User :> Protected) :<|> ApiLogin

server :: CookieSettings -> JWTSettings -> Server (TestApi auths)
server cs jwts = protected :<|> login' cs jwts

mainWithCookies :: IO ()
mainWithCookies = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings { cookieIsSecure = NotSecure }
      cfg = cookieCfg :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (TestApi '[Cookie])
  run 7250 $ serveWithContext api cfg (server cookieCfg jwtCfg)

-- Examples of running it:
--
-- $ curl -v \
--     -X POST \
--     -H "Content-Type: application/json" \
--     -d '{"username": "hello", "password": "hello"}' \
--     http://localhost:7250/login
--
-- $ curl -v \
--     -X POST \
--     -H "Content-Type: application/json" \
--     -d '{"username": "Ali Baba", "password": "Open Sesame"}' \
--     http://localhost:7250/login
--
-- $ curl -v \
--     -H "Content-Type: application/json"
--     -X GET \
--     http://localhost:7250/name
--
-- $ curl -v \
--     -X GET \
--     -H "Content-Type: application/json" \
--     -H 'X-XSRF-TOKEN: Oj79TaT2vRCKuVLPYZcnXi2iwEXUbxpH5m1OtkMfecA=' \
--     --cookie 'JWT-Cookie=eyJhbGciOiJIUzI1NiJ9.eyJkYXQiOnsiZW1haWwiOiJhbGlAZW1haWwuY29tIiwibmFtZSI6IkFsaSBCYWJhIn19.R-jyWgSpyGdSLJ9KhHmx6O9xQ9CLsHgEp05edIE3fpQ; XSRF-TOKEN=Oj79TaT2vRCKuVLPYZcnXi2iwEXUbxpH5m1OtkMfecA=' \
--     http://localhost:7250/email
