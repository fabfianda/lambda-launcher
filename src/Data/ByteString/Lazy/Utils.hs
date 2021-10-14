module Data.ByteString.Lazy.Utils
        where

import Data.ByteString.Lazy(split,intercalate)
import Data.ByteString.Lazy.Char8(pack)

splitTabs = split 9

unlines = intercalate (pack "\n")
