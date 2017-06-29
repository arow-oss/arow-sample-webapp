{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Db
  ( module App.Db
  , module App.Db.Types
  , module Database.Persist.Postgresql
  ) where

import Control.Monad.Reader (ReaderT, reader)
import Data.Text (Text)
import Database.Persist.Postgresql
       (Entity(..), EntityField(..), Key(..), SqlBackend(..),
        ToBackendKey, Unique, fromSqlKey, insertUnique, runMigration,
        runSqlPool, toSqlKey)
import Database.Persist.TH
       (mkPersist, persistLowerCase, share, sqlSettings, mkMigrate)

import App.Config (Config(configPool))
import App.Db.Types (PasswordHash(..))
import App.Monad (AppM)
import App.Password (hashPassword)

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

dbAddAdmin
  :: Text -> Text -> Text -> ReaderT SqlBackend AppM (Maybe (Key Admin))
dbAddAdmin email password name = do
  hash <- hashPassword password
  insertUnique $ Admin email hash name

dbAddCompanyUser
  :: Text -> Text -> Text -> ReaderT SqlBackend AppM (Maybe (Key CompanyUser))
dbAddCompanyUser email password name = do
  hash <- hashPassword password
  insertUnique $ CompanyUser email hash name

dbAddUser
  :: Text -> Text -> Text -> ReaderT SqlBackend AppM (Maybe (Key User))
dbAddUser email password name = do
  hash <- hashPassword password
  insertUnique $ User email hash name
