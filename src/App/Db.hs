{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Db where

import Control.Monad.Reader (ReaderT, reader)
import Data.Text (Text)
import Database.Persist
       (Entity(..), EntityField(..), Key(..), Unique)
import Database.Persist.Postgresql (SqlBackend(..), runMigration, runSqlPool )
import Database.Persist.TH
       (mkPersist, persistLowerCase, share, sqlSettings, mkMigrate)

import App.Config (Config(configPool))
import App.Monad (AppM)

$(share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Admin
      email    Text
      password Text
      name     Text

      UniqueAdminEmail email

      deriving Eq
      deriving Read
      deriving Show

    CompanyUser
      email    Text
      name     Text
      password Text

      UniqueCompanyUserEmail email

      deriving Eq
      deriving Read
      deriving Show

    User
      email    Text
      name     Text
      password Text

      UniqueUserEmail

      deriving Eq
      deriving Show
  |])

-- | Run all sql migration.
doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

-- | Run a Persistent query.
runDb :: ReaderT SqlBackend AppM a -> AppM a
runDb query = reader configPool >>= runSqlPool query
