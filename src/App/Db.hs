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
        ToBackendKey, Unique, fromSqlKey, getBy, insertUnique,
        runMigration, runSqlPool, toSqlKey)
import Database.Persist.TH
       (mkPersist, persistLowerCase, share, sqlSettings, mkMigrate)

import App.Config (Config(configPool))
import App.Db.Types (PasswordHash(..))
import App.Monad (AppM)
import App.Password
       (PasswordCheck(PasswordCorrect, PasswordIncorrect), checkPassword,
        hashPassword)

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

      UniqueUserEmail email

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
dbAddAdmin email pass name = do
  hash <- hashPassword pass
  insertUnique $ Admin email hash name

dbAddCompanyUser
  :: Text -> Text -> Text -> ReaderT SqlBackend AppM (Maybe (Key CompanyUser))
dbAddCompanyUser email pass name = do
  hash <- hashPassword pass
  insertUnique $ CompanyUser email hash name

dbAddUser
  :: Text -> Text -> Text -> ReaderT SqlBackend AppM (Maybe (Key User))
dbAddUser email pass name = do
  hash <- hashPassword pass
  insertUnique $ User email hash name

dbCheckUserPassword :: Text -> Text -> ReaderT SqlBackend AppM (Maybe (Key User))
dbCheckUserPassword email pass = do
  maybeUser <- getBy $ UniqueUserEmail email
  case maybeUser of
    Nothing -> pure Nothing
    Just (Entity userId (User _ userPasswordHash _)) ->
      case checkPassword pass userPasswordHash of
        PasswordCorrect -> pure (Just userId)
        PasswordIncorrect -> pure Nothing
