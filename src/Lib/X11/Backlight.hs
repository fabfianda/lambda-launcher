{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}

module Lib.X11.Backlight where


import           Shh

$(load SearchPath["xbacklight"])

-- Shell

newPercentage :: (Command t) => Int -> t
newPercentage = xbacklight "-set"

-- API
setPercentage :: (Command t) => Int -> t
setPercentage = newPercentage
