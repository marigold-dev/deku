-- | All the types needed for cli to work.
module Cli.Types
  ( LigoClientEnv (..)
  , HasLigoClient(..)
  , RawContractCode(..)
  , ligoBinaryPath
  ) where

import Control.Exception.Safe (catch, throwIO)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy.Char8 qualified as S8L
import Data.Default (Default (..))
import Language.Haskell.TH.Syntax (liftString)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)

newtype RawContractCode = RawContractCode
  { unRawContract :: S8L.ByteString
  } deriving stock Show

-- | Environment passed throughout the ligo interaction
newtype LigoClientEnv = LigoClientEnv
  { -- | Ligo binary path
    _lceClientPath :: FilePath
  }
  deriving stock (Show)

-- | Attempts to get the environment variable 'LIGO_BINARY_PATH'. If such
-- variable is not present, defaults to "ligo", assuming it is in PATH.
ligoBinaryPath :: FilePath
ligoBinaryPath =
  $(
    let
      getLigo :: IO FilePath
      getLigo = getEnv "LIGO_BINARY_PATH" `catch` \e ->
        if isDoesNotExistError e
        then pure "ligo"
        else throwIO e
    in liftIO getLigo >>= liftString
  )

instance Default LigoClientEnv where
  def = LigoClientEnv ligoBinaryPath

class (Monad m, MonadIO m, MonadCatch m) => HasLigoClient m where
  getLigoClientEnv :: m LigoClientEnv

-- Mostly for debugging purposes
instance HasLigoClient IO where
  getLigoClientEnv = pure def
