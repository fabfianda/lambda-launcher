{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Lib
    ( main
    ) where

import           Shh
import           System.Environment

$(load SearchPath["rofi", "ls", "echo"])

main :: IO ()

main = do
   initInteractive
   dirs <- ls "-al" |> rofi "-dmenu" |> captureLines
   echo dirs
