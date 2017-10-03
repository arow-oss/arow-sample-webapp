module App.Server where

import Control.Monad.Except (throwError)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, reader, runReaderT)
import Control.Natural ((:~>)(NT))
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Char8 (pack)
import Data.Default (def)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai (Application, Middleware, Request)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
       (logStdout, logStdoutDev)
import Database.Persist.Sql (runSqlPool)
import Servant
       ((:>), (:<|>)(..), Context(..), Get, Handler, Header, Headers,
        JSON, NoContent(NoContent), Post, PostNoContent, ReqBody,
        ServantErr, Server, ServerT, enter, err401, err403, errBody, serve,
        serveWithContext)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Servant.Server.Experimental.Auth.Cookie
       (AuthCookieData, AuthCookieException, AuthCookieSettings, Cookied,
        PersistentServerKey, RandomSource, WithMetadata, acsCookieFlags,
        getSession, mkPersistentServerKey)

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
  runSqlPool doMigrations $ configPool config
  putStrLn $ "app running on port " <> show (configPort config)
  run (configPort config) . loggerMiddleware $ app config

type Api = "v0" :> (ApiLogin :<|> ApiSearch :<|> ApiStatus)

type ApiSearch = "search" :> Post '[JSON] String

type ApiStatus = "status" :> Get '[JSON] Int

type ApiLogin =
  "login"
    :> ReqBody '[JSON] Login
    :> PostNoContent
        '[JSON]
        -- (Headers
        --   '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        --   NoContent
        -- )
        (Cookied NoContent)

serverRoot :: ServerT Api AppM
serverRoot = login :<|> search :<|> status

search :: AppM String
search = do
  eitherStatuses <- undefined
  case eitherStatuses of
    Left twitterErr -> undefined
    Right statuses -> undefined

status :: AppM Int
status = pure 1

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
login cookieSettings randomSource serverKey (Login email pass) = do
  maybeUserId <- runDb $ dbCheckUserPassword email pass
  case maybeUserId of
    Nothing   -> return $ addHeader emptyEncryptedSession (loginPage False)
    Just uid  ->
      addSession
        cookieSettings
        randomSource
        serverKey
        (User lfUsername lfPassword)
        (pure ())
      -- (redirectPage "/private" "Session has been started")

  -- cookieSettings <- reader configCookieSettings
  -- jwtSettings <- reader configJWTSettings
  -- maybeUserId <- runDb $ dbCheckUserPassword email pass
  -- case maybeUserId of
  --   Nothing -> throwError err401
  --   Just userId -> do
  --     mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings userId
  --     case mApplyCookies of
  --       Nothing -> throwError err401
  --       Just applyCookies -> pure $ applyCookies NoContent

data User = User
  { name :: Text
  , email :: Text
  } deriving (Eq, Generic, Read, Show)

instance Serialize User

-- TODO: See if anything breaks if we remove this line.
type instance AuthCookieData = User

-- | Custom handler that bluntly reports any occurred errors.
authHandler
  :: AuthCookieSettings
  -> PersistentServerKey
  -> AuthHandler Request (WithMetadata User)
authHandler cookieSettings serverKey = mkAuthHandler $ \request ->
  getSession cookieSettings serverKey request `catch` handleEx >>= maybe
    (throwError err403 {errBody = "No cookies"})
    return
  where
    handleEx :: AuthCookieException -> Handler (Maybe (WithMetadata User))
    handleEx ex = throwError err403 {errBody = fromStrict . pack $ show ex}

-- | Implementation
-- server ::
--   :: AuthCookieSettings
--   -> (IO ())
--   -> RandomSource
--   -> PersistentServerKey
--   -> Server ExampleAPI
-- server settings generateKey rs sks =
--        serveHome
--   :<|> serveLogin
--   :<|> serveLoginPost
--   :<|> serveLogout
--   :<|> servePrivate
--   :<|> serveWhoami
--   :<|> serveKeys where


--   serveHome = return homePage
--   serveLogin = return (loginPage True)

--   serveLoginPost LoginForm {..} =
--     case userLookup lfUsername lfPassword usersDB of
--       Nothing   -> return $ addHeader emptyEncryptedSession (loginPage False)
--       Just uid  -> addSession'
--         (Account uid lfUsername lfPassword)
--         (redirectPage "/private" "Session has been started")

--   serveLogout = removeSession settings (redirectPage "/" "Session has been terminated")

--   serveWhoami Nothing = return $ whoamiPage Nothing
--   serveWhoami (Just h) = do
--     mwm <- getHeaderSession settings sks h `catch` handleEx
--     return $ whoamiPage $ wmData <$> mwm
--     where
--       handleEx :: AuthCookieException -> Handler (Maybe (WithMetadata Account))
--       handleEx _ex = return Nothing

--   servePrivate = cookied settings rs sks (Proxy :: Proxy Account) servePrivate'

--   servePrivate' :: Account -> Handler Markup
--   servePrivate' (Account uid u p) = return $ privatePage uid u p

--   serveKeys = keysPage False <$> getKeys sks

-- | Given a 'Config', this returns a Wai 'Application'.
app :: Config -> Application
app config =
  let authSettings =
        -- Note that we do not use "Secure" flag here. Cookies with this flag will be
        -- accepted only if they were transfered over https. This is a must for
        -- production server, but is an obstacle if you want to check it without
        -- setting up TLS.
        def {acsCookieFlags = ["HttpOnly"]}
      serverKey = mkPersistentServerKey "0123456789abcdef"
      context = authHandler authSettings serverKey :. EmptyContext
      api = undefined -- apiServer config
  in serveWithContext (Proxy :: Proxy Api) context api

-- | Given a 'Config', this returns a servant 'Server' for 'Api'
apiServer :: Config -> Server Api
apiServer config = enter natTrans serverRoot
  where
    natTrans :: AppM :~> Handler
    natTrans = NT trans

    trans :: forall a. AppM a -> Handler a
    trans appM = runReaderT appM config

-- -- | Application
-- app :: (ServerKeySet s)
--   => AuthCookieSettings
--   -> IO () -- ^ An action to create a new key
--   -> RandomSource
--   -> s
--   -> Application
-- app settings generateKey rs sks = serveWithContext
--   (Proxy :: Proxy ExampleAPI)
--   ((authHandler settings sks) :. EmptyContext)
--   (server settings generateKey rs sks)

----------------
-- Auth stuff --
----------------

-- data User = User { name :: String, email :: String }
--    deriving (Eq, Generic, Read, Show)

-- instance ToJSON User
-- instance ToJWT User
-- instance FromJSON User
-- instance FromJWT User

-- data Login = Login { loginEmail :: Text, loginPassword :: Text }
--    deriving (Eq, Generic, Read, Show)

-- instance ToJSON Login
-- instance FromJSON Login


-- type Protected
--      = "name" :> Get '[JSON] String
--   :<|> "email" :> Get '[JSON] String

-- -- | 'Protected' will be protected by 'auths', which we still have to specify.
-- protected :: AuthResult User -> Server Protected
-- protected (Authenticated user) = return (name user) :<|> return (email user)
-- protected BadPassword = undefined :<|> (liftIO (print "badpassword") *> throwError err401)
-- protected NoSuchUser = undefined :<|> (liftIO (print "no such user") *> throwError err401)
-- protected Indefinite = undefined :<|> (liftIO (print "indefinite") *> throwError err401)

-- login'
--   :: CookieSettings
--   -> JWTSettings
--   -> Login
--   -> Handler
--       (Headers
--         '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
--         NoContent
--       )
-- login' cookieSettings jwtSettings (Login email pass) = do
--   maybeUserId <- runDb $ dbCheckUserPassword email pass
--   case maybeUserId of
--     Nothing -> throwError err401
--     Just userId -> do
--       mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
--       case mApplyCookies of
--         Nothing -> throwError err401
--         Just applyCookies -> pure $ applyCookies NoContent

-- type TestApi auths = (Auth auths User :> Protected) :<|> ApiLogin

-- server :: CookieSettings -> JWTSettings -> Server (TestApi auths)
-- server cs jwts = protected :<|> login' cs jwts

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
