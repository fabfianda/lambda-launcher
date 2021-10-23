{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module App (ask, module App) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import App.Env(Env(..))

newtype App a = App { unApp :: ReaderT Env IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow, MonadReader Env)

runApp :: Env -> App a -> IO a
runApp env action = runReaderT (unApp action) env

inApp :: IO a -> App a
inApp = liftIO



