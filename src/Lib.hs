{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Lib
    ( main
    ) where

import           Lib.PulseAudio.Ctl
import           Lib.Rofi
import           Shh


main :: IO ()
main = do
   initInteractive
   eSinks <- getSinksList
   case eSinks of
     Right sinks -> do
        sink <- prettyPickItem sinks
        print sink

     Left e -> print e
