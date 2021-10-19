{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Lib.PulseAudio.Ctl where

import qualified Data.ByteString.Lazy        as BSL (ByteString)
import           Data.ByteString.Lazy.Parsec
import           Data.ByteString.Lazy.Utils
import           Data.Functor
import           Lib.PulseAudio.Types
import           Shh

$(load SearchPath["pactl"])

-- Shell

listShortSinks :: Command t => t
listShortSinks = pactl "list" "short" "sinks"

-- API
getSinksList :: (Shell f, Monad f) => f (Either ParseError [Sink])
getSinksList = do
        sinks <- listShortSinks |> captureLines
        let eSinks = sinks <&> toSinkType
        pure $ sequence eSinks

-- Parser
sinkParser :: Parser Sink
sinkParser = Sink <$> tabbedDigitField
                  <*> tabbedTextField
                  <*> tabbedTextField
                  <*> tabbedTextField
                  <*> tabbedField sinkStatusParser

sinkStatusParser :: Parser SinkStatus
sinkStatusParser = do
        match <- string "SUSPENDED" 
             <|> string "IDLE"
             <|> string "RUNNING"
        pure $ case match of
          "SUSPENDED" -> SinkSuspended
          "IDLE"      -> SinkIdle
          "RUNNING"   -> SinkRunning
          _           -> SinkStatusUnknown

-- boolField :: Parser Bool
-- boolField = do
--              output <- string "yes" <|> string "no"
--              pure $ output == "yes"
--

-- Conversions

toSinkType = parse sinkParser
