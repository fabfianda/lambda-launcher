module Option.Types where

import           App.Types
import           Data.Text

newtype GlobalOpts = GlobalOpts {cmdVerb :: CmdVerb} deriving (Show)

newtype CmdVerb = RofiVerb {optAction :: Action} deriving (Show)
