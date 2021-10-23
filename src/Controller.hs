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
          (GlobalOpts (RofiVerb "set-sink-volume") ) -> inApp setSinkVolume 
          _ -> inApp $ print "OOF"
