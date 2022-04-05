{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Action
     where

import           App.Types
import           Control.Monad
import           Data.Either
import           Data.Functor
import           Data.Maybe
import qualified Lib.PulseAudio.Ctl as Ctl
import           Lib.Rofi
import qualified Lib.X11.Backlight  as XBacklight
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

setBacklightBrightness :: IO ()
setBacklightBrightness = do
   initInteractive

   newVal <- prettyPickItem "Brightness"  (Ctl.ramp 5 20 100)
   guard (isJust newVal)

   XBacklight.setPercentage (fromJust newVal)

pickAction :: IO ()
pickAction = do
   initInteractive
   let actions = [SetSinkVolume, SetBacklightBrightness]
   action <- rawPickItem "Choose an action" actions
   case action of
     Just SetSinkVolume          -> setSinkVolume
     Just SetBacklightBrightness -> setBacklightBrightness
     Just AllActions             -> error "something odd"
     Nothing                     -> print "nothing to see here"
