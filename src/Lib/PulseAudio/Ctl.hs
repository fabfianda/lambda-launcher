{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}

module Lib.PulseAudio.Ctl where

import qualified Data.ByteString.Lazy        as BSL (ByteString)
import           Data.ByteString.Lazy.Parsec
import           Data.ByteString.Lazy.Utils
import           Data.Functor
import           Lib.PulseAudio.Types
import           Prelude                     hiding (head, tail)
import           Shh

$(load SearchPath["pactl", "head", "grep", "tail", "sed"])

-- Shell

listShortSinks :: Command t => t
listShortSinks = pactl "list" "short" "sinks"

sinkVolume :: (Shell f) => Int -> f ()
sinkVolume num = pactl "list" "sinks" |> grep "^[[:space:]]Volume:" |> head ("-n " ++ show num) |> tail "-n 1" |> percentageMatch

percentageMatch :: Command t => t
percentageMatch = sed "-e" "s,.* \\([0-9][0-9]*\\)%.*,\\1,"


-- API
getSinksList :: (Shell f, Monad f) => f (Either ParseError [Sink])
getSinksList = do
        sinks <- listShortSinks |> captureLines
        let eSinks = sinks <&> toSinkType
        pure $ sequence eSinks

getSinkVolume :: (Shell f, Monad f) => Sink -> f ()
getSinkVolume Sink{..} = do
        sinkVolume sinkId

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

-- Conversions

toSinkType :: BSL.ByteString -> Either ParseError Sink
toSinkType = parse sinkParser
