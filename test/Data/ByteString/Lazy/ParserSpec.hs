{-# LANGUAGE OverloadedStrings #-}
module Data.ByteString.Lazy.ParserSpec (spec) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Parser
import Test.Hspec
import Text.Megaparsec.Debug

--Tests

spec = do
  describe "oo" $ do
    it "is ok" $ do
        let input = "ok"
        let parser = dbg "oker" $ string "ok" :: Parser ByteString

        let parsed = runParser parser "" input

        parsed `shouldBe` Right "ok"

