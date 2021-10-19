{-# LANGUAGE RecordWildCards #-}
module Lib.PulseAudio.Types where

import           Data.Text
import           Prettyprinter

data SinkStatus = SinkRunning | SinkSuspended | SinkIdle | SinkStatusUnknown deriving (Show,Eq,Ord)

data Sink = Sink {
                    sinkId         :: !Int,
                    sinkName       :: !Text,
                    sinkDriver     :: !Text,
                    sinkSampleSpec :: !Text,
                    sinkStatus     :: !SinkStatus
                 } deriving (Show,Eq,Ord)

--- PrettyPrinter

instance Pretty SinkStatus where
        pretty status =
                case status of
                  SinkIdle      -> pretty "Idle"
                  SinkRunning   -> pretty "Running"
                  SinkSuspended -> pretty "Suspended"
                  _             -> pretty "n/a"

instance Pretty Sink where
        pretty Sink{..} = brackets (pretty sinkId) <+> pretty sinkName <+> parens (pretty sinkStatus)
