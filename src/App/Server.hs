module App.Server where

import Control.Monad.Except (throwError)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, reader, runReaderT)
import Control.Natural ((:~>)(NT))
import Crypto.Random (drgNew)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Char8 (pack)
import Data.Default (def)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import Data.Serialize (Serialize)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Network.Wai (Application, Middleware, Request)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
       (logStdout, logStdoutDev)
import Database.Persist.Sql (runSqlPool)
import Servant
       ((:>), (:<|>)(..), Context(..), AuthProtect, Get, Handler, Header,
        Headers, JSON, NoContent(NoContent), Post, PostNoContent, ReqBody,
        ServantErr, Server, ServerT, addHeader, enter, err401, err403,
        errBody, serve, serveWithContext)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Servant.Server.Experimental.Auth.Cookie
       (AuthCookieData, AuthCookieException, AuthCookieSettings, Cookied,
        PersistentServerKey, RandomSource, WithMetadata(WithMetadata),
        acsCookieFlags, addSession, emptyEncryptedSession, getSession,
        mkPersistentServerKey, mkRandomSource)

import App.Config (Config(..), configFromEnv)
import App.Db (dbCheckUserPassword, doMigrations, runDb)
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
  randomSource <- mkRandomSource drgNew 1000
  runSqlPool doMigrations $ configPool config
  putStrLn $ "app running on port " <> show (configPort config)
  run (configPort config) . loggerMiddleware $ app config randomSource

type Api = "v0" :> (ApiLogin :<|> ApiSearch :<|> ApiStatus)

type ApiSearch = "search" :> Post '[JSON] String

type ApiStatus = AuthProtect "cookie-auth" :> "status" :> Get '[JSON] String

type ApiLogin =
  "login" :> ReqBody '[JSON] Login :> PostNoContent '[JSON] (Cookied NoContent)

serverRoot
  :: AuthCookieSettings
  -> RandomSource
  -> PersistentServerKey
  -> ServerT Api AppM
serverRoot authSettings randomSource serverKey =
  login authSettings randomSource serverKey :<|> search :<|> status

search :: AppM String
search = do
  eitherStatuses <- undefined
  case eitherStatuses of
    Left twitterErr -> undefined
    Right statuses -> undefined

status :: WithMetadata User -> AppM String
status (WithMetadata (User email _) _) = pure email

data Login = Login
  { loginEmail :: Text
  , loginPassword :: Text
  } deriving (Eq, Generic, Read, Show)

instance ToJSON Login
instance FromJSON Login

login
  :: AuthCookieSettings
  -> RandomSource
  -> PersistentServerKey
  -> Login
  -> AppM (Cookied NoContent)
login authSettings randomSource serverKey (Login email pass) = do
  maybeUserId <- runDb $ dbCheckUserPassword email pass
  case maybeUserId of
    Nothing   -> pure $ addHeader emptyEncryptedSession NoContent
    Just _uid  ->
      addSession
        authSettings
        randomSource
        serverKey
        (User (unpack email) (unpack pass))
        NoContent

-- How to use curl to access login and status:
--
-- $ curl --verbose --request POST --header 'Content-Type: application/json' -d '{"loginEmail": "user@google.com", "loginPassword": "somepassword"}' 'http://localhost:8105/v0/login'
--
-- $ curl --verbose --request GET --cookie 'Session=Z0wv2YJSTMch0teESKV2vzIwMTcxMDAzMjM0OTU4iY+J6rt+rMT2BGWjQZAepGgBVb4uGyh6yqHxI6CN/YoQIGQuvx+frmcn/5dPbmGt1BY6gDi41yq8Cth8Ork3FWro7yKSEnbSao9EmhwORDg=' --header 'Content-Type: application/json' 'http://localhost:8105/v0/status'

data User = User
  { name :: String
  , email :: String
  } deriving (Eq, Generic, Read, Show)

instance Serialize User

-- TODO: See if anything breaks if we remove this line.
type instance AuthCookieData = User

-- | Custom handler that bluntly reports any occurred errors.
authHandler
  :: AuthCookieSettings
  -> PersistentServerKey
  -> AuthHandler Request (WithMetadata User)
authHandler authSettings serverKey = mkAuthHandler $ \request ->
  getSession authSettings serverKey request `catch` handleEx >>= maybe
    (throwError err403 {errBody = "No cookies"})
    return
  where
    handleEx :: AuthCookieException -> Handler (Maybe (WithMetadata User))
    handleEx ex = throwError err403 {errBody = fromStrict . pack $ show ex}

-- | Given a 'Config', this returns a Wai 'Application'.
app :: Config -> RandomSource -> Application
app config randomSource = do
  let authSettings =
        -- Note that we do not use "Secure" flag here. Cookies with this flag will be
        -- accepted only if they were transfered over https. This is a must for
        -- production server, but is an obstacle if you want to check it without
        -- setting up TLS.
        def {acsCookieFlags = ["HttpOnly"]}
      serverKey = mkPersistentServerKey "0123456789abcdef"
      context = authHandler authSettings serverKey :. EmptyContext
      api = apiServer config authSettings randomSource serverKey
  serveWithContext (Proxy :: Proxy Api) context api

-- | Given a 'Config', this returns a servant 'Server' for 'Api'
apiServer
  :: Config
  -> AuthCookieSettings
  -> RandomSource
  -> PersistentServerKey
  -> Server Api
apiServer config authSettings randomSource serverKey =
  enter natTrans (serverRoot authSettings randomSource serverKey)
  where
    natTrans :: AppM :~> Handler
    natTrans = NT trans
    trans
      :: forall a.
         AppM a -> Handler a
    trans appM = runReaderT appM config
