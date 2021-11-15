module Extension
  ( ElimExt (..)
  , Lang (..)
  , UnsupportedExtension (..)
  , extGlobs
  , getExt
  , onExt
  , supportedExtensions
  ) where

import Control.Monad.Catch
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath

import AST.Skeleton (Lang (..))

data ElimExt a = ElimExt
  { eePascal :: a
  , eeCaml   :: a
  , eeReason :: a
  }

newtype UnsupportedExtension = UnsupportedExtension String
  deriving stock Show
  deriving anyclass Exception

-- TODO: 'lsp' uses the 'Glob' package to deal with globs, but it doesn't
-- support braced globs such as "{,m,re}ligo" even though the LSP spec allows
-- it. Because of this, we return multiple globs instead of one single glob.
extGlobs :: [Text]
extGlobs = Text.pack . (("**" </>) . ("*" <>)) <$> supportedExtensions

getExt :: MonadThrow m => FilePath -> m Lang
getExt path =
  case takeExtension path of
    ".religo" -> return Reason
    ".ligo"   -> return Pascal
    ".mligo"  -> return Caml
    ext       -> throwM $ UnsupportedExtension ext

onExt :: ElimExt a -> FilePath -> IO a
onExt ee path =
  getExt path <&> \case
    Pascal -> eePascal ee
    Caml   -> eeCaml   ee
    Reason -> eeReason ee

supportedExtensions :: [FilePath]
supportedExtensions = [".ligo", ".mligo", ".religo"]
