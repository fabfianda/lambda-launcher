{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Lazy.Parsec (module Data.ByteString.Lazy.Parsec, module Text.Parsec.ByteString.Lazy, module Text.Parsec) where

import           Control.Applicative         ((<*))
import           Control.Monad               (void)
import           Data.ByteString.Lazy        (ByteString (..))
import           Data.ByteString.Lazy.UTF8   (fromString)
import           Data.Char
import           Data.Text                   (Text (..), pack)
import           Text.Parsec                 hiding (parse)
import qualified Text.Parsec
import           Text.Parsec.ByteString.Lazy

-- helpers

parse :: Parser a -> ByteString -> Either ParseError a
parse p = Text.Parsec.parse p ""

parseWithEof :: Parser a -> ByteString -> Either ParseError a
parseWithEof p = Text.Parsec.parse (p <* eof) ""

parseWithLeftOver :: Parser a -> ByteString -> Either ParseError (a, String)
parseWithLeftOver p = Text.Parsec.parse ((,) <$> p <*> leftOver) ""
                       where leftOver = manyTill anyToken eof

doubleLineBreak :: Parser String
doubleLineBreak = string ['\n','\n']


parseWithDoubleLineBreak :: Parser a -> ByteString -> Either ParseError a
parseWithDoubleLineBreak p = Text.Parsec.parse (p <* notFollowedBy doubleLineBreak) ""

tabChar :: Parser Char
tabChar = satisfy (== '\t')

newLine :: Parser Char
newLine = satisfy (== '\n')

notTabChar :: Parser Char
notTabChar = satisfy (/= '\t')

isNotTabChar :: Char -> Bool
isNotTabChar = (/= '\t')

isNotNewLine :: Char -> Bool
isNotNewLine = (/= '\n')

allButTabChar :: Parser Text
allButTabChar = do
                 chars <- many1 $ satisfy isNotTabChar
                 pure $ Data.Text.pack chars

allButNewLine :: Parser Text
allButNewLine = do
                 chars <- many1 $ satisfy isNotNewLine
                 pure $ Data.Text.pack chars


digitField :: Parser Int
digitField = do
               num <- many1 $ satisfy (\x -> isDigit x && isNotTabChar x)
               pure $ read num

boolField :: Parser Bool
boolField = (== "yes") <$> (string "yes" <|> string "no")

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

number :: Parser Int
number = do
               num <- many1 $ satisfy isDigit
               pure $ read num

trimWSstart :: Parser a -> Parser a
trimWSstart parser = whitespace *> parser

trimWStartAndEnd :: Parser a -> Parser a
trimWStartAndEnd parser = whitespace *> parser <* whitespace

-- note how this and tabbedField below are identical!
tabbedFieldParser :: Parser a -> Parser a
tabbedFieldParser fieldTypeParser = do
                   field <- trimWStartAndEnd fieldTypeParser
                   void $ optional tabChar
                   pure field

tabbedField :: Parser a -> Parser a
tabbedField p = trimWStartAndEnd p <* optional tabChar

tabbedTextField :: Parser Text
tabbedTextField = tabbedField allButTabChar

tabbedDigitField :: Parser Int
tabbedDigitField = tabbedField digitField

-------

-- personParser :: Parser P
-- personParser = P
--                <$> csvField (exactMatchField <|> alphaNumField)
--                <*> csvField (exactMatchField <|> alphaNumField)
--                <*> csvField digitField
--                <*> csvField boolField
--                <*> csvField monthField

-- input = " fab , fiandanese ,42, yes, 11-2020"
-- examples = do
--     parseWithEof personParser input
