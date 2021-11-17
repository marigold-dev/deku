
module Log (module Log, i) where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad
import Data.IORef
import Data.String.Interpolate (i)

import System.IO.Unsafe

import System.IO (hFlush, hPutStrLn, stderr)

data Level = DEBUG | ERROR | CRASH deriving stock (Eq, Ord)

{-# NOINLINE logLevel #-}
logLevel :: IORef Level
logLevel = unsafePerformIO do
  newIORef CRASH

{-# NOINLINE logLock #-}
logLock :: MVar ()
logLock = unsafePerformIO do
  newMVar ()

debug :: MonadIO m => String -> String -> m ()
debug sys msg = liftIO do
  lvl <- readIORef logLevel
  when (lvl == DEBUG) do
    synchronized do
      hPutStrLn stderr $ "DEBUG (" <> sys <> "): " <> msg
      hFlush stderr

err :: MonadIO m => String -> String -> m ()
err sys msg = liftIO do
  synchronized do
    hPutStrLn stderr $ "ERROR (" <> sys <> "): " <> msg

setLogLevel :: MonadIO m => Level -> m ()
setLogLevel level = liftIO do
  writeIORef logLevel level
  return ()

synchronized :: (MonadMask m, MonadIO m) => m a -> m a
synchronized = bracket_
  do liftIO $ takeMVar logLock
  do liftIO $ putMVar  logLock ()
