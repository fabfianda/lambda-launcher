{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Controller where

import           Action
import           App
import           App.Env
import           App.Types
import           Control.Monad
import           Option
import           Option.Command

runController :: App ()
runController = do
        Env{..} <- ask
        case envOptions of
          (GlobalOpts (RofiVerb SetSinkVolume) ) -> inApp setSinkVolume
          (GlobalOpts (RofiVerb AllActions) )    -> inApp pickAction
