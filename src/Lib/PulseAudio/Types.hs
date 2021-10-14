module Lib.PulseAudio.Types where

import Data.Text

data SinkStatus = SinkRunning | SinkSuspended deriving (Show,Eq,Ord)

data Sink = Sink {
                    sinkId:: !Int,
                    sinkName :: !Text,
                    sinkLib :: !Text,
                    sinkBits :: !Text,
                    sinkFreq :: !Text,
                    sinkStatus :: !SinkStatus 
                 } deriving (Show,Eq,Ord)
