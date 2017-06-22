
module App.Environment where

data Environment
  = Development
  | Testing
  | Production
  deriving (Eq, Ord, Read, Show)
