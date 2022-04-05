{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}

module Lib.X11.XRandR where


import           Shh

$(load SearchPath["xrandr"])

type Output = String

-- Shell

newBrightness :: (Command t) => Output -> Int -> t
newBrightness output = xrandr "--output" output "--brightness"

-- API
setBrightness :: (Command t) => Output -> Int -> t
setBrightness = newBrightness
