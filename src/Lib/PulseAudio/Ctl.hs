{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}

module Lib.PulseAudio.Ctl where

import           Control.Monad
import           Data.ByteString.Lazy        (ByteString (..))
import           Data.ByteString.Lazy.Parsec
import           Data.ByteString.Lazy.Utils
import           Data.Functor
import           Lib.PulseAudio.Types
import           Prelude                     hiding (head, tail)
import           Shh

$(load SearchPath["pactl", "head", "grep", "tail", "sed", "sleep"])

-- Shell

listShortSinks :: Command t => t
listShortSinks = pactl "list" "short" "sinks"

sinkVolume :: (Shell f) => Int -> f ()
sinkVolume num = pactl "list" "sinks" |> grep "^[[:space:]]Volume:" |> head ("-n " ++ show num) |> tail "-n 1" |> percentageMatch

percentageMatch :: Command t => t
percentageMatch = sed "-e" "s,.* \\([0-9][0-9]*\\)%.*,\\1,"

newSinkVolume :: (Command t) => Int -> Int -> t
newSinkVolume sinkId vol = pactl "set-sink-volume" sinkId (show vol <> "%")

-- API
getSinksList :: (Shell f, Monad f) => f (Either ParseError [Sink])
getSinksList = do
        sinks <- listShortSinks |> captureLines
        let eSinks = sinks <&> toSinkType
        pure $ sequence eSinks

getSinkVolume :: (Shell f, Monad f) => Sink -> f (Either ParseError Int)
getSinkVolume Sink{..} = do
        vol <- sinkVolume sinkId |> captureTrim
        pure $ parse number vol

setSinkVolume :: (Command t) => Sink -> Int -> t
setSinkVolume Sink{..} = newSinkVolume sinkId

-- fadeSinkVolume :: p -> p1 -> Sink -> m ()
fadeSinkVolume from to sink = forM_ [from..to] (\v -> setSinkVolume sink v >> sleep "0.2" )

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

toSinkType :: ByteString -> Either ParseError Sink
toSinkType = parse sinkParser
