{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Lib.Rofi where

import           Control.Exception         (SomeException, catch)
import           Data.ByteString.Lazy.UTF8 (ByteString, fromString)
import           Data.Functor              ((<&>))
import           Data.List                 (elemIndex, (!!))
import           Data.Text
import           Prettyprinter             (Pretty (pretty))
import           Shh                       (ExecReference (SearchPath),
                                            captureTrim, load, (|>))
import qualified Shh.Internal              as Shh (unlines)

$(load SearchPath["rofi", "ls", "echo"])

-- Shell

rofiDmenu title = rofi "-dmenu" "-p" (fromString title)

-- Types
type Renderer a = (a -> ByteString)

-- Helpers
prettyRenderer :: Pretty a => a -> ByteString
prettyRenderer = fromString . show . pretty

basicRenderer :: Show a => a -> ByteString
basicRenderer = fromString . show

-- API
pickItem :: String -> Renderer a -> [a] -> IO ( Maybe a )
pickItem title render items = do
        let renderedItems = items <&> render
        item <- catch( echo (Shh.unlines renderedItems ) |> rofiDmenu title |> captureTrim )
                     ( \(e :: SomeException) -> pure "" )

        return $ case elemIndex item renderedItems of
          Just index -> Just $ items !! index
          Nothing    -> Nothing

prettyPickItem :: (Pretty a) => String -> [a] -> IO (Maybe a)
prettyPickItem title = pickItem title prettyRenderer

rawPickItem :: (Show a) => String -> [a] -> IO (Maybe a)
rawPickItem title = pickItem title basicRenderer
