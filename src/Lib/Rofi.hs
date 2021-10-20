{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Lib.Rofi where

import           Control.Exception         (SomeException, catch)
import           Data.ByteString.Lazy.UTF8 (ByteString, fromString)
import           Data.Functor              ((<&>))
import           Data.List                 (elemIndex, (!!))
import           Prettyprinter             (Pretty (pretty))
import           Shh                       (ExecReference (SearchPath),
                                            captureTrim, load, (|>))
import qualified Shh.Internal              as Shh (unlines)
import           System.Environment        ()

$(load SearchPath["rofi", "ls", "echo"])

-- Shell

rofiDmenu = rofi "-dmenu"

-- Types
type Renderer a = (a -> ByteString)

-- Helpers
prettyRenderer :: Pretty a => a -> ByteString
prettyRenderer = fromString . show . pretty

basicRenderer :: Show a => a -> ByteString
basicRenderer = fromString . show

-- API
pickItem :: Renderer a -> [a] -> IO ( Maybe a )
pickItem render items = do
        let renderedItems = items <&> render
        item <- catch( echo (Shh.unlines renderedItems ) |> rofiDmenu |> captureTrim )
                     ( \(e :: SomeException) -> pure "" )

        return $ case elemIndex item renderedItems of
          Just index -> Just $ items !! index
          Nothing    -> Nothing

prettyPickItem :: (Pretty a) => [a] -> IO (Maybe a)
prettyPickItem = pickItem prettyRenderer
