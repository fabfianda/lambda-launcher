{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Lib
    ( main
    ) where

import           Shh
import           System.Environment
import Data.Functor
import Lib.PulseAudio.Ctl

$(load SearchPath["rofi", "ls", "echo"])

main :: IO ()

main = do
   initInteractive
   sinks <- sinksList

   sink <- echo sinks |> rofi "-dmenu" |> captureLines
   echo sink
