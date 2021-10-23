module Option (module Option, module Option.Parser, module Option.Types) where

import Option.Parser
import Option.Types
import Options.Applicative (execParser)

parseOptions :: IO GlobalOpts
parseOptions = execParser parseWithInfoGlobalOpts
