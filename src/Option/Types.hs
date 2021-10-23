module Option.Types where

import           Data.Text

newtype GlobalOpts = GlobalOpts {cmdVerb :: CmdVerb} deriving (Show)

newtype CmdVerb = RofiVerb {optAction :: Text} deriving (Show)
