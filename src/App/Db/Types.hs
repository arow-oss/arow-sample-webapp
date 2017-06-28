

module App.Db.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Database.Persist (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..))

newtype PasswordHash = PasswordHash
  { unPasswordHash :: Text
  } deriving (Eq, FromJSON, PersistField, PersistFieldSql, Read, Show, ToJSON)
