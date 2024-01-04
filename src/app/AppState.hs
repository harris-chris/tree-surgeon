module AppState
  (
    Env(..)
    , App
  ) where

import Control.Monad.Logger.Aeson
import Control.Monad.Reader
import qualified Data.Text as T

data Env = Env
    { envLog :: !(T.Text -> IO ()) }

class HasLog a where
    getLog :: a -> (T.Text -> IO ())
instance HasLog (T.Text -> IO ()) where
    getLog = id
instance HasLog Env where
    getLog = envLog

type App = ReaderT Env IO

logSomething :: (MonadReader env m, HasLog env, MonadIO m)
             => T.Text
             -> m ()
logSomething msg = do
  env <- ask
  liftIO $ getLog env msg
