
module App.Monad where

import Control.Monad.Reader (ReaderT)
import Servant (Handler)

import App.Config (Config)

type AppM = ReaderT Config Handler
