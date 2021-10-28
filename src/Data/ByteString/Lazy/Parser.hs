{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Lazy.Parser (module Text.Megaparsec, module Data.ByteString.Lazy.Parser, module Text.Megaparsec.Byte ) where

import           Data.ByteString.Lazy (ByteString)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Byte

type Parser = Parsec Void ByteString
