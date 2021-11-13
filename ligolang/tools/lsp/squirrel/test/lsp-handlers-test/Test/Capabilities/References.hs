module Test.Capabilities.References
  ( unit_references
  , unit_close_open_docs
  , unit_fileChanges
  ) where

import Control.Monad.IO.Class (liftIO)
import Language.LSP.Test
import Language.LSP.Types (List (..), Location (..), Position (..), Range (..), filePathToUri)
import System.Directory (makeAbsolute, removeFile)
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldMatchList)
import Test.Common.Util (openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "find"

unit_references :: Assertion
unit_references = do
  let filename = "heap.ligo"

  List refs <- runHandlersTest contractsDir do
    doc <- openLigoDoc filename
    getReferences doc (Position 7 9) True

  filepath <- makeAbsolute (contractsDir </> filename)
  let uri = filePathToUri filepath
  refs `shouldMatchList` fmap (Location uri)
    [ Range (Position 7 9) (Position 7 16)
    , Range (Position 11 29) (Position 11 36)
    , Range (Position 24 30) (Position 24 37)
    , Range (Position 68 30) (Position 68 37)
    ]

unit_close_open_docs :: Assertion
unit_close_open_docs = do
  let b1fp = "includes" </> "B1.ligo"
  let b2fp = "includes" </> "B2" </> "B2.ligo"
  let b3fp = "includes" </> "B3.ligo"

  (refs1, refs2, refs3) <- runHandlersTest contractsDir do
    doc1 <- openLigoDoc b2fp
    List refs1 <- getReferences doc1 (Position 2 11) True
    closeDoc doc1

    -- We still need to get the same references even after closing and opening
    -- the document. Checks whether `RIO.load` works as intended.
    doc2 <- openLigoDoc b2fp
    List refs2 <- getReferences doc2 (Position 2 11) True
    closeDoc doc2

    -- Should not vary even when extracting from another file.
    doc3 <- openLigoDoc b1fp
    List refs3 <- getReferences doc3 (Position 2 20) True
    closeDoc doc3

    pure (refs1, refs2, refs3)

  b1 <- filePathToUri <$> makeAbsolute (contractsDir </> b1fp)
  b2 <- filePathToUri <$> makeAbsolute (contractsDir </> b2fp)
  b3 <- filePathToUri <$> makeAbsolute (contractsDir </> b3fp)
  refs1 `shouldMatchList`
    [ Location b1 (Range (Position 2 20) (Position 2 22))
    , Location b2 (Range (Position 2 11) (Position 2 13))
    , Location b3 (Range (Position 0  6) (Position 0  8))
    ]

  refs2 `shouldMatchList` refs1
  refs3 `shouldMatchList` refs1

unit_fileChanges :: Assertion
unit_fileChanges = do
  let
    defFile = "default.mligo"
    newFile = contractsDir </> "new.mligo"

  -- FIXME: I couldn't get lsp-test to make this run, so I split it into three
  -- sections below. Doing the same thing manually in VSCode works as intended,
  -- but not in the test.
  {-
  (refsCreate, refsChange, refsDelete) <- runHandlersTest contractsDir do
    doc <- openLigoDoc defFile
    let refsForGreet = getReferences doc (Position 0 4) True

    liftIO $ writeFile newFile "#include \"default.mligo\"\nlet greet = hello\n"
    List refsCreate <- refsForGreet

    liftIO $ appendFile newFile "let hi = hello\n"
    List refsChange <- refsForGreet

    liftIO $ removeFile newFile
    List refsDelete <- refsForGreet

    pure (refsCreate, refsChange, refsDelete)
  -}

  let refsForGreet doc = getReferences doc (Position 0 4) True
  List refsCreate <- runHandlersTest contractsDir do
    doc <- openLigoDoc defFile
    liftIO $ writeFile newFile "#include \"default.mligo\"\nlet greet = hello\n"
    refsForGreet doc
  List refsChange <- runHandlersTest contractsDir do
    doc <- openLigoDoc defFile
    liftIO $ appendFile newFile "let hi = hello\n"
    refsForGreet doc
  List refsDelete <- runHandlersTest contractsDir do
    doc <- openLigoDoc defFile
    liftIO $ removeFile newFile
    refsForGreet doc

  defUri <- filePathToUri <$> makeAbsolute (contractsDir </> defFile)
  newUri <- filePathToUri <$> makeAbsolute newFile

  let
    fstRef = Location defUri (Range (Position 0  4) (Position 0  9))
    sndRef = Location newUri (Range (Position 1 12) (Position 1 17))
    trdRef = Location newUri (Range (Position 2  9) (Position 2 14))

  refsCreate `shouldMatchList` [fstRef, sndRef]
  refsChange `shouldMatchList` [fstRef, sndRef, trdRef]
  refsDelete `shouldMatchList` [fstRef]
