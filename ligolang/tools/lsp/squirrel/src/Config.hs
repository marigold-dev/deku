{-# LANGUAGE RecordWildCards #-}

-- | This module contains glpbal extension configuration

module Config
  ( Config(..)
  , getConfigFromNotification
  ) where

import Data.Aeson
  (FromJSON (parseJSON), Result (Error, Success), ToJSON (toJSON), Value, fromJSON, object,
  withObject, (.!=), (.:), (.:?), (.=))
import Data.Default (Default (def))
import Data.Text qualified as T

import Cli (LigoClientEnv (..))

----------------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------------

-- | Given a DidChangeConfigurationNotification message, this function returns the parsed
-- Config object if possible.
getConfigFromNotification :: Config -> Value -> Either T.Text Config
getConfigFromNotification _old p =
  case fromJSON p of
    Success c -> Right c
    Error err -> Left $ T.pack err

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

-- | Global extension configuration
data Config =
  Config
    { _cLigoBinaryPath :: FilePath -- ^ Path to ligo binary
    , _cMaxNumberOfProblems :: Int -- ^ Maximum amount of errors displayed
    } deriving stock (Eq, Show)

instance Default Config where
  def = Config
    { _cLigoBinaryPath = _lceClientPath def -- Extract ligo binary from $PATH
    , _cMaxNumberOfProblems = 100
    }

----------------------------------------------------------------------------
-- JSON
----------------------------------------------------------------------------

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    s <- v .: "ligoLanguageServer"
    flip (withObject "Config.settings") s $ \o -> do
      _cLigoBinaryPath <- o .:? "ligoBinaryPath" .!= _cLigoBinaryPath def
      _cMaxNumberOfProblems <- o .:? "maxNumberOfProblems" .!= _cMaxNumberOfProblems def
      pure Config
        { _cLigoBinaryPath = _cLigoBinaryPath
        , _cMaxNumberOfProblems = _cMaxNumberOfProblems
        }

instance ToJSON Config where
  toJSON Config {..} = object [ "ligoLanguageServer" .= r ]
    where
      r = object
        [ "ligoBinaryPath" .= _cLigoBinaryPath
        , "maxNumberOfProblems" .= _cMaxNumberOfProblems
        ]
