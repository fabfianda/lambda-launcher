module Option.Parser where

import           App.Types
import           Option.Types
import           Options.Applicative

-- Rofi

cmdRofiVerb :: Mod CommandFields CmdVerb
cmdRofiVerb = command
                "rofi"
                   (info
                      (RofiVerb <$> parseAction)
                      (header "Rofi powered actions")
                   )


parseAction :: Parser Action
parseAction = subparser
       ( command "set-sink-volume"
         (info (pure SetSinkVolume) (progDesc "Set sink volume"))
      <> command "set-backlight-brightness"
         (info (pure SetBacklightBrightness) (progDesc "Set backlight brightness"))
      <> command "all"
         (info (pure AllActions) (progDesc "Pick from a list"))
       )

parseAllCmdVerbs :: Parser CmdVerb
parseAllCmdVerbs = hsubparser cmdRofiVerb

parseGlobalOpts :: Parser GlobalOpts
parseGlobalOpts = GlobalOpts <$> parseAllCmdVerbs

parseWithInfoGlobalOpts :: ParserInfo GlobalOpts
parseWithInfoGlobalOpts = info
                            (helper <*> parseGlobalOpts)
                            (fullDesc <> progDesc "Lambda Launcher" <> header "lamda-launcher" )
