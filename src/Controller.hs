{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Controller where

import           Action
import           App
import           App.Env
import           Control.Monad
import           Option
import           Option.Command

runController :: App ()
runController = do
        Env{..} <- ask
        case envOptions of
          (GlobalOpts (RofiVerb SetSinkVolume) ) -> inApp setSinkVolume 
          (GlobalOpts (RofiVerb AllActions) ) -> inApp pickAction 
