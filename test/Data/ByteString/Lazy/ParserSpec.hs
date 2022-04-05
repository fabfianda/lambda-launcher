{-# LANGUAGE OverloadedStrings #-}
module Data.ByteString.Lazy.ParserSpec (spec) where

import           Data.ByteString.Lazy        (ByteString)
import           Data.ByteString.Lazy.Parser
import           Data.Either
import           Test.Hspec
import           Text.Megaparsec.Debug

--Tests

parser :: Parser ByteString
parser = dbg "oker" $ string "ok"

spec = do
  describe "parsing strings" $ do

    it "should succeed" $ do
        let input = "ok"
        let parsed = runParser parser "" input
        parsed `shouldBe` Right "ok"

    it "should fail" $ do
        let input = "olk"
        let parsed = runParser parser "" input
        isLeft parsed `shouldBe` True
