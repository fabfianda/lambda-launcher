{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Lib.PulseAudio.Ctl where

import           Shh
import qualified Data.ByteString.Lazy as BSL(ByteString())
import Data.ByteString.Lazy.Utils
import Data.Functor

$(load SearchPath["pactl"])

-- API

getShortSinksList :: Command t => t
getShortSinksList = pactl "list" "short" "sinks"

-- Conversions

toSinkType = splitTabs

-- 
sinksList :: (Shell f, Monad f) => f [[BSL.ByteString]]
sinksList = do 
        sinks <- getShortSinksList |> captureLines
        pure $ sinks <&> toSinkType
