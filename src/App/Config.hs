
module App.Config where

import Control.Monad.Logger (runStdoutLoggingT)
import Crypto.JOSE (JWK)
import Data.Aeson (eitherDecodeStrict')
import Data.ByteString (ByteString)
import Database.Persist.Postgresql
       (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool)
import Data.Semigroup ((<>))
import Network.Wai.Handler.Warp (Port)
import Servant.Auth.Server
       (CookieSettings(cookieIsSecure), IsSecure(NotSecure), JWTSettings,
        defaultCookieSettings, defaultJWTSettings)
import System.ReadEnvVar (lookupEnvDef, readEnvDef)

import App.Environment (Environment(..))

data Config = Config
  { configCookieSettings :: CookieSettings
  , configEnvironment :: Environment
  , configJWTSettings :: JWTSettings
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
  jwkByteString <- lookupEnvDef "JWK" devJWKByteString
  let eitherJwk = eitherDecodeStrict' jwkByteString
  jwk <-
    case eitherJwk of
      Left err ->
        error $
          "got following error when trying to decode json web key: " <> show err
      Right jwk -> pure jwk
  createConfig env jwk port connNum connectionString

createConfig :: Environment -> JWK -> Port -> Int -> ConnectionString -> IO Config
createConfig env jwk port connNum connectionString = do
  let jwtConfig = defaultJWTSettings jwk
      cookieConfig = defaultCookieSettings { cookieIsSecure = NotSecure }
  pool <- runStdoutLoggingT $ createPostgresqlPool connectionString connNum
  pure $ Config cookieConfig env jwtConfig pool port

devJWKByteString :: ByteString
devJWKByteString = "{\"k\":\"XEP7OZKVglaRnGsN_sM-BI1gP6Sz3NOOpq3UcjGSf2HtOP5vomvzITpz0d0-7dCY9Rk51Pmw9ttxE0_1JOxoHKrA_8J_kiqMjESGDKav3CqdLClOnxGUmptru-muNg4_Wej5KoiQDQk1_C1z5mfLyGWwR_KS86j6lqJXrRlwXAbgK_a0hv5lG5DCsmBIah4-UDlT_loKXhEeD1APPnRqcpMW4DQ3yeV3wA84pIqdpU59f4-u5Euv47R60Ki--g87Udky9VK8MhlMc4MW6qE6jlbH9lRcIlhvpVNqr4rR9dycG6dvvmuMSB8oz3bm63WlWlp2--y95nZUhcYFQU5Z0w\",\"kty\":\"oct\"}"
