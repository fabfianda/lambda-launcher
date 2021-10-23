{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Exe
    ( main
    ) where

import           App
import           App.Env
import           Controller
import           Option
import           Shh

main :: IO ()
main = do
   env <- Env
       <$> parseOptions
   runApp env runController

