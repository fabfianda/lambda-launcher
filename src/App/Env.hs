module App.Env where

import           Option.Types

newtype Env = Env {envOptions :: GlobalOpts} deriving (Show)
