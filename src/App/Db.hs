{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Db
  ( module App.Db
  , module App.Db.Types
  , module Database.Persist
  , SqlBackend
  , toSqlKey
  ) where

import Control.Monad.Reader (ReaderT, reader)
import Data.Text (Text)
import Database.Persist
       (Entity(..), EntityField(..), Key(..), ToBackendKey, Unique)
import Database.Persist.Postgresql
       (SqlBackend(..), runMigration, runSqlPool, toSqlKey)
import Database.Persist.TH
       (mkPersist, persistLowerCase, share, sqlSettings, mkMigrate)

import App.Config (Config(configPool))
import App.Db.Types (PasswordHash(..))
import App.Monad (AppM)

$(share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Admin
      email        Text
      passwordHash PasswordHash
      name         Text

      UniqueAdminEmail email

      deriving     Eq
      deriving     Read
      deriving     Show

    CompanyUser
      email        Text
      passwordHash PasswordHash
      name         Text

      UniqueCompanyUserEmail email

      deriving     Eq
      deriving     Read
      deriving     Show

    User
      email        Text
      passwordHash PasswordHash
      name         Text

      UniqueUserEmail

      deriving     Eq
      deriving     Show
  |])

-- | Run all sql migration.
doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

-- | Run a Persistent query.
runDb :: ReaderT SqlBackend AppM a -> AppM a
runDb query = reader configPool >>= runSqlPool query
