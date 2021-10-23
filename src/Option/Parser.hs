module Option.Parser where

import           Option.Types
import           Options.Applicative

-- Rofi

cmdRofiVerb :: Mod CommandFields CmdVerb
cmdRofiVerb = command
                "rofi"
                   (info
                      (RofiVerb <$> strArgument (metavar "ACTION" <> help "Action to perform" ))
                      (header "Rofi powered actions")
                   )

parseAllCmdVerbs :: Parser CmdVerb
parseAllCmdVerbs = hsubparser cmdRofiVerb

parseGlobalOpts :: Parser GlobalOpts
parseGlobalOpts = GlobalOpts <$> parseAllCmdVerbs

parseWithInfoGlobalOpts :: ParserInfo GlobalOpts
parseWithInfoGlobalOpts = info
                            (helper <*> parseGlobalOpts)
                            (fullDesc <> progDesc "Lambda Launcher" <> header "lamda-launcher" )
