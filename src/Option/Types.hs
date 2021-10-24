module Option.Types where

import           Data.Text

newtype GlobalOpts = GlobalOpts {cmdVerb :: CmdVerb} deriving (Show)

data Action = AllActions | SetSinkVolume deriving (Show, Eq,  Ord)

newtype CmdVerb = RofiVerb {optAction :: Action} deriving (Show)
