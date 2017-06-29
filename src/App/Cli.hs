module App.Cli where

import App.Config (Config(..), configFromEnv)
import App.Monad (AppM)

defaultMainCli :: IO ()
defaultMainCli = do
  config <- configFromEnv
  -- -- runSqlPool doMigrations $ configPool config
  -- putStrLn $ "app running on port " <> show (configPort config)
  -- -- run 7250 $ serveWithContext api context (server cookieCfg jwtCfg)
  -- run (configPort config) . loggerMiddleware $ app config
  -- -- mainWithCookies
  undefined
