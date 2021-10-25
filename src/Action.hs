{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Action
     where

import           Control.Monad
import           Data.Either
import           Data.Functor
import           Data.Maybe
import qualified Lib.PulseAudio.Ctl as Ctl
import           Lib.Rofi
import           Option
import           Option.Types
import           Shh


setSinkVolume :: IO ()
setSinkVolume = do
   initInteractive

   eSinks <- Ctl.getSinksList
   unless (isRight eSinks) (error "Sinks unavailable")

   mSink <- prettyPickItem "Sinks" (fromRight [] eSinks)
   guard (isJust mSink)

   vol <- Ctl.getSinkVolume (fromJust mSink)
   unless (isRight vol) (error "Could not determine current volume")

   newVol <- prettyPickItem ("Volume [" ++ show (fromRight 0 vol) ++ "]") (Ctl.ramp 5 0 100)
   guard (isJust newVol)

   Ctl.fadeSinkVolume 5 (fromRight 0 vol) (fromJust newVol) (fromJust mSink)

pickAction :: IO ()
pickAction = do
   initInteractive
   let actions = [SetSinkVolume]
   action <- rawPickItem "Choose an action" actions
   case action of
     Just SetSinkVolume -> setSinkVolume
     Just AllActions    -> error "something odd"
     Nothing            -> print "nothing to see here"
