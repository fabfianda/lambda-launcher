{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
module Parsec where

import           Control.Applicative ((<*))
import           Control.Monad       (void)
import           Data.Char
import           Text.Parsec         hiding (parse)
import qualified Text.Parsec
import           Text.Parsec.String

-- helpers

parse :: Parser a -> String -> Either ParseError a
parse p = Text.Parsec.parse p ""

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = Text.Parsec.parse (p <* eof) ""

parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = Text.Parsec.parse ((,) <$> p <*> leftOver) ""
                       where leftOver = manyTill anyToken eof

--
-- data P = P { name :: String, surname :: String, age :: Int, male :: Bool, month :: Month } deriving (Show,Eq,Ord)


tabChar :: Parser Char
tabChar = satisfy (== '\t')

notTabChar :: Parser Char
notTabChar = satisfy (/= '\t')

isNotTabChar :: Char -> Bool
isNotTabChar = (/= '\t')

alphaNumField :: Parser String
alphaNumField = many1 $ satisfy (\x -> isAlphaNum x && isNotTabChar x)

allButTabChar :: Parser String
allButTabChar = many1 $ satisfy isNotTabChar 

digitField :: Parser Int
digitField = do
               num <- many1 $ satisfy (\x -> isDigit x && isNotTabChar x)
               pure $ read num

boolField :: Parser Bool
boolField = (== "yes") <$> (string "yes" <|> string "no")

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

trimWSstart :: Parser a -> Parser a
trimWSstart parser = whitespace *> parser

trimWStartAndEnd :: Parser a -> Parser a
trimWStartAndEnd parser = whitespace *> parser <* whitespace

-- note how this and v2 below are identical!
tabbedFieldParser :: Parser a -> Parser a
tabbedFieldParser fieldTypeParser = do
                   field <- trimWStartAndEnd fieldTypeParser
                   void $ optional tabChar
                   pure field

tabbedField p = trimWStartAndEnd p <* optional tabChar

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


someFunc :: IO ()
someFunc = do
    putStrLn "-----------"
    putStrLn "-----------"
