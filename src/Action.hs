{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Action
     where

import           Data.Either
import           Data.Functor
import qualified Lib.PulseAudio.Ctl as Ctl
import           Lib.Rofi
import           Option
import           Option.Types
import           Shh


setSinkVolume :: IO ()
setSinkVolume = do
   initInteractive
   eSinks <- Ctl.getSinksList
   case eSinks of
     Right sinks -> do
        sink <- prettyPickItem "Sinks" sinks
        case sink of
          Just sink -> do
                       vol <- Ctl.getSinkVolume sink
                       case vol of
                         Right vol -> do
                                 newVol <- prettyPickItem ("Volume [" ++ show vol ++ "]") (Ctl.ramp 5 0 100)
                                 case newVol of
                                   Just newVol -> Ctl.fadeSinkVolume 5 vol newVol sink
                                   Nothing     -> print "nothing to do here"

                         Left e -> print "Volume fetch error"

          _         -> print "nothing to see here"
     Left e -> print e

pickAction :: IO ()
pickAction = do
   initInteractive
   let actions = [SetSinkVolume]
   action <- rawPickItem "Sinks" actions
   case action of
     Just SetSinkVolume -> setSinkVolume
     Just AllActions    -> error "something odd"
     Nothing            -> print "nothing to see here"
