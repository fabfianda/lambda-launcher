{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Lib
    ( main
    ) where

import           Lib.PulseAudio.Ctl
import Data.Functor
import           Lib.Rofi
import           Shh
import Data.Either


main :: IO ()
main = do
   initInteractive
   eSinks <- getSinksList
   case eSinks of
     Right sinks -> do
        sink <- prettyPickItem "Sinks" sinks
        case sink of
          Just sink -> do
                       vol <- getSinkVolume sink
                       case vol of
                         Right vol -> do
                                 newVol <- prettyPickItem ("Volume [" ++ show vol ++ "]") (ramp 5 0 100)
                                 case newVol of
                                   Just newVol -> fadeSinkVolume 5 vol newVol sink
                                   Nothing     -> print "nothing to do here"

                         Left e -> print "Volume fetch error"

          _         -> print "nothing to see here"


     Left e -> print e

-- alt :: IO ()
-- alt = do
--    initInteractive
--    sink <- getSinksList >>= (\eSinks -> do 
--                                          sink <- prettyPickItem "Sinks" eSinks
--                                          pure sink
--                             )
