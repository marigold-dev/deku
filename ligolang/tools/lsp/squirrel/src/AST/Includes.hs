{-# LANGUAGE OverloadedLists #-}

module AST.Includes
  ( extractIncludedFiles
  , includesGraph
  , insertPreprocessorRanges
  , MarkerInfo (..)
  ) where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as G
import Control.Arrow (first, (&&&))
import Control.Lens (Lens', _1, view, (&), (+~), (-~), (.~), (^.))
import Control.Monad (join, when)
import Control.Monad.RWS.Strict (RWS, execRWS, gets, modify, tell)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.DList (DList, toList)
import Data.Foldable (for_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text (pack)
import Duplo.Tree (Cofree ((:<)), inject)
import System.FilePath ((</>), takeDirectory)

import AST.Scope.Common (ContractInfo, pattern FindContract, ParsedContractInfo, contractFile, MarkerInfo (..))
import AST.Scope.Fallback (loop, loopM_)
import AST.Skeleton (Error (..), Lang (..), LIGO, SomeLIGO (..))

import Parser
  ( CodeSource (..), Info, LineMarker (..), LineMarkerType (..), ParsedInfo
  , ShowRange (N)
  )
import ParseTree (Source (..))
import Product (Product (..), getElem, modElem, putElem)
import Range
  ( PreprocessedRange (..), Range, getRange, point, rangeLines, rFile, rFinish
  , rStart, startLine, finishLine
  )
import Util (removeDots)

fromOriginalPoint :: Product Info -> Product ParsedInfo
fromOriginalPoint infos = PreprocessedRange (getRange infos) :> infos

insertPreprocessorRanges :: ContractInfo -> ParsedContractInfo
insertPreprocessorRanges = fst . extractIncludedFiles False

-- | Given some contract A, returns a new version of that contract such that:
-- * When a line marker is found, all 'Range's will be adjusted in a manner such
--   that everything in the current document will stay as it is, but ranges from
--   included parts will be "squashed" to the 'Range' where the inclusion was
--   made. In other words, the ranges will be restored to the 'Range's that the
--   user sees.
-- * Additionally, 'PreprocessedRange's will be included, and they will have the
--   actual 'Range's of the files. That is, if A includes B then the ranges will
--   actually point to B. These are the actual ranges which are mapped according
--   to the line markers present in the document.
--
-- Also returns a difference list containing the edges of how to include files.
-- That is, if A includes B, then this list will contain a tuple (A, B).
extractIncludedFiles
  :: Bool  -- ^ Whether to only extract directly included files ('True') or all of them ('False').
  -> ContractInfo  -- ^ The contract to scan for includes.
  -> (ParsedContractInfo, DList (FilePath, FilePath))
extractIncludedFiles directIncludes (FindContract file (SomeLIGO dialect ligo) msgs) =
  let
    ((), toList -> markers) = execRWS (loopM_ collectMarkers ligo) () ()
    (markerInfos, edges) = execRWS (collectMarkerInfos markers) () mempty
    (info :< tree) = loop (go markerInfos) $ fromOriginalPoint <$> ligo
    -- We still want RawContract to start at line 1:
    ligo' = bool (modElem (startLine +~ 1) info :< tree) (info :< tree) (IntMap.null markerInfos)
    msgs' = fmap (first (adjustRange markerInfos)) msgs
  in
  (FindContract file (SomeLIGO dialect ligo') msgs', edges)
  where
    pwd :: FilePath
    pwd = takeDirectory $ srcPath file

    withPwd :: FilePath -> FilePath
    withPwd = removeDots . (pwd </>)

    implies :: Bool -> Bool -> Bool
    prerequisite `implies` conclusion = not prerequisite || conclusion
    infixr 1 `implies`

    collectMarkers :: LIGO Info -> RWS () (DList LineMarker) () ()
    collectMarkers (info :< _) = for_ (getElem @[LineMarker] info) (tell . pure)

    collectMarkerInfos :: [LineMarker] -> RWS () (DList (FilePath, FilePath)) (IntMap MarkerInfo) ()
    collectMarkerInfos markers =
      -- For the actual ranges, we pretend everything is fused into the line
      -- where the include would be located.
      -- For the preprocessed ranges, we use the line markers to map the range
      -- to the file and line they represent.
      for_ (sortOn (view startLine . lmLoc) markers) \lm@(LineMarker (withPwd -> n) f _ r) ->
        let line = r ^. startLine in
        case f of
          RootFile     -> modify $ IntMap.insert line $ MarkerInfo lm r 0
          IncludedFile -> gets (IntMap.lookupLT line) >>= \case
            Nothing -> pure ()
            Just (_, MarkerInfo prev lr d) -> do
              modify $ IntMap.insert
                line
                (MarkerInfo lm (bool lr (r & startLine -~ lmLoc prev ^. finishLine - lmLine prev) $ d == 0) (d + 1))
              when (directIncludes `implies` d == 0) $
                tell [(withPwd $ lmFile prev, n)]
          ReturnToFile -> modify $ IntMap.lookupLT line >>= \case
            Nothing -> id
            Just (_, MarkerInfo _ lr d) -> IntMap.insert line $ MarkerInfo lm lr (d - 1)

    go :: IntMap MarkerInfo -> LIGO ParsedInfo -> LIGO ParsedInfo
    go markers (info :< tree) = info' :< tree
      where
        info' = info
              & adjustPreprocessedRange markers
              & modElem (adjustRange markers)

    adjustPreprocessedRange :: IntMap MarkerInfo -> Product ParsedInfo -> Product ParsedInfo
    adjustPreprocessedRange markers i = case prev of
      Nothing -> i
      Just (_, MarkerInfo marker _ _) ->
        let preRange = range
              & rangeLines -~ (lmLoc marker ^. finishLine - lmLine marker)
              & rFile .~ withPwd (lmFile marker)
         in putElem (PreprocessedRange preRange) i
      where
        prev = IntMap.lookupLE (range ^. startLine) markers
        range = getRange i

    adjustSide :: Lens' Range (Int, Int, Int)
               -> IntMap MarkerInfo
               -> Range
               -> Range
    adjustSide side markers range = case prev of
      Nothing -> range
      Just (_, MarkerInfo marker lastRange depth) ->
        -- In case we are at depth 0, we need to subtract the accumulated range at
        -- the both the start and finish, since we may see new includes while
        -- traversing the tree.
        let newRange = range &
              if depth > 0 then
                side .~ lastRange ^. rStart
              else
                (side . _1) -~ lmLoc marker ^. finishLine - lmLine marker
         in newRange
      where
        prev = IntMap.lookupLE (range ^. (side . _1)) markers

    adjustRange :: IntMap MarkerInfo -> Range -> Range
    adjustRange markers = adjustSide rFinish markers
                        . adjustSide rStart markers

-- | Given a list of contracts, builds a graph that represents how they are
-- included.
includesGraph :: [ContractInfo] -> AdjacencyMap ParsedContractInfo
includesGraph contracts =
  uncurry G.overlay $ bimap (G.edges . toList) G.vertices $ foldr go ([], []) contracts
  where
    knownContracts :: Map FilePath (ParsedContractInfo, DList (FilePath, FilePath))
    knownContracts = Map.fromList ((contractFile &&& extractIncludedFiles False) <$> contracts)

    go
      :: ContractInfo
      -> (DList (ParsedContractInfo, ParsedContractInfo), [ParsedContractInfo])
      -> (DList (ParsedContractInfo, ParsedContractInfo), [ParsedContractInfo])
    go contract (edges, vertices) =
      let
        (vertex', edges') = findContract $ contractFile contract
        edges'' = join bimap (fst . findContract) <$> edges'
      in
      (edges'' <> edges, vertex' : vertices)

    findContract :: FilePath -> (ParsedContractInfo, DList (FilePath, FilePath))
    findContract contract =
      Map.findWithDefault (emptyContract contract, []) contract knownContracts

    emptyContract :: FilePath -> ParsedContractInfo
    emptyContract name =
      let
        p = point (-1) (-1)
        info = PreprocessedRange p :> [] :> [] :> p :> N :> CodeSource "" :> Nil
      in
      FindContract
        (Path name)
        (SomeLIGO Caml (info :< inject (Error ("Missing contract: " <> Text.pack name) [])))
        []
