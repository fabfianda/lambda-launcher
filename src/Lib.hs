{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Lib
    ( main
    ) where

import           Data.ByteString.Lazy.UTF8
import           Data.Functor
import           Lib.PulseAudio.Ctl
import           Prettyprinter
import           Shh
import qualified Shh.Internal              as Shh (unlines)
import           System.Environment

$(load SearchPath["rofi", "ls", "echo"])

toLBS = fromString . show . pretty

main :: IO ()
main = do
   initInteractive
   eSinks <- getSinksList
   case eSinks of
     Right sinks -> do
        sink <- echo (Shh.unlines $ sinks <&> toLBS) |> rofi "-dmenu" |> captureLines
        echo sink

     Left e -> print e
