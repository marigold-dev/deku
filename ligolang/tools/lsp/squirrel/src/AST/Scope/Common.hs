{-# OPTIONS_GHC -Wno-orphans #-}

module AST.Scope.Common
  ( MarkerInfo (..)
  , ParsedContract (..)
  , FindFilepath (..)
  , HasScopeForest (..)
  , Level (..)
  , ScopeError (..)
  , ScopeM
  , Info'
  , ScopeForest (..)
  , ScopeInfo
  , ScopeTree
  , DeclRef (..)
  , MergeStrategy (..)
  , ContractInfo
  , ParsedContractInfo
  , ContractInfo'
  , ContractNotFoundException (..)

  , pattern FindContract

  , contractFile
  , contractTree
  , contractMsgs

  , cFile
  , cTree
  , cMsgs
  , getContract

  , emptyScopeForest
  , ofLevel
  , mergeScopeForest
  , withScopeForest
  , lookupEnv
  , spine
  , addScopes
  , lookupContract
  ) where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as G
import Algebra.Graph.Export qualified as G (export, literal, render)
import Control.Arrow ((&&&))
import Control.Exception.Safe
import Control.Lens (makeLenses)
import Control.Lens.Operators ((&))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.DList (DList, snoc)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (First (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Witherable (ordNub)

import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree hiding (loop)

import AST.Pretty
import AST.Scope.ScopedDecl (DeclarationSpecifics (..), Scope, ScopedDecl (..))
import AST.Skeleton
  (Ctor (..), Lang, Name (..), NameDecl (..), RawLigoList, SomeLIGO, Tree', TypeName (..),
  withNestedLIGO)
import Cli.Types
import ParseTree
import Parser
import Product
import Range
import Util (findKey, unionOrd)
import Util.Graph (traverseAMConcurrently)

-- TODO: Many of these datatypes don't make sense to be defined here. Consider
-- moving into different or new modules.
data MarkerInfo = MarkerInfo
  { miMarker    :: LineMarker
  , miLastRange :: Range
  , miDepth     :: Int
  } deriving stock (Show)

data ParsedContract info = ParsedContract
  { _cFile :: Source -- ^ The path to the contract.
  , _cTree :: info -- ^ The payload of the contract.
  , _cMsgs :: [Msg] -- ^ Messages produced by this contract.
  } deriving stock (Show)

-- | Wraps a 'ParsedContract', allowing it to be stored in a container where its
-- comparison will always be on its source file.
newtype FindFilepath info
  = FindFilepath { _getContract :: ParsedContract info }
  deriving newtype (Show)

instance Eq (FindFilepath info) where
  (==) = (==) `on` contractFile

instance Ord (FindFilepath info) where
  compare = compare `on` contractFile

contractFile :: FindFilepath info -> FilePath
contractFile (FindFilepath pc) = srcPath $ _cFile pc

contractTree :: FindFilepath info -> info
contractTree (FindFilepath pc) = _cTree pc

contractMsgs :: FindFilepath info -> [Msg]
contractMsgs (FindFilepath pc) = _cMsgs pc

makeLenses ''ParsedContract
makeLenses ''FindFilepath

class HasLigoClient m => HasScopeForest impl m where
  scopeForest :: AdjacencyMap ParsedContractInfo -> m (AdjacencyMap (FindFilepath ScopeForest))

instance {-# OVERLAPPABLE #-} Pretty x => Show x where
  show = show . pp

data Level = TermLevel | TypeLevel
  deriving stock Eq

instance Pretty Level where
  pp TermLevel = "TermLevel"
  pp TypeLevel = "TypeLevel"

ofLevel :: Level -> ScopedDecl -> Bool
ofLevel level decl = case (level, _sdSpec decl) of
  (TermLevel, ValueSpec{}) -> True
  (TypeLevel, TypeSpec{}) -> True
  _ -> False

data ScopeError =
  TreeDoesNotContainName
    Doc  -- ^ pprinted tree (used for simplifying purposes for not stacking
         -- type parameters for `ScopeM` which brings plethora of confusion)
    Range -- ^ location where the error has occurred
    Text -- ^ variable name
  deriving Show via PP ScopeError

instance Pretty ScopeError where
  pp (TreeDoesNotContainName tree range name) =
    "Given tree does not contain " <> pp name <> ": " <> tree <> " (" <> pp range <> ")"

instance Exception ScopeError

type ScopeM = ExceptT ScopeError (Reader Lang)

type Info' = Scope ': Maybe Level ': ParsedInfo

data ScopeForest = ScopeForest
  { sfScopes :: [ScopeTree]
  , sfDecls  :: Map DeclRef ScopedDecl
  }
  deriving stock (Eq, Ord)
  deriving Show via PP ScopeForest

emptyScopeForest :: ScopeForest
emptyScopeForest = ScopeForest [] Map.empty

type ScopeInfo = [Set DeclRef, Range]
type ScopeTree = Tree' '[[]] ScopeInfo

data DeclRef = DeclRef
  { drName  :: Text
  , drRange :: Range
  }
  deriving Show via PP DeclRef
  deriving stock (Eq, Ord)

instance Pretty DeclRef where
  pp (DeclRef n r) = pp n <.> "@" <.> pp r

data MergeStrategy
  = OnUnion
  | OnIntersection

-- | Merge two scope forests into one.
mergeScopeForest :: MergeStrategy -> ScopeForest -> ScopeForest -> ScopeForest
mergeScopeForest strategy (ScopeForest sl dl) (ScopeForest sr dr) =
  ScopeForest (descend sl sr) (mapStrategy mergeRefs dl dr)
  where
    mapStrategy :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
    mapStrategy = case strategy of
      OnUnion -> Map.unionWith
      OnIntersection -> Map.intersectionWith

    go :: ScopeTree -> ScopeTree -> [ScopeTree]
    go
      l@(only -> (ldecls :> lr :> Nil, ldeepen))
      r@(only -> (rdecls :> rr :> Nil, rdeepen))
      -- These two are likely different things, so we shouldn't merge them.
      | not (lr `intersects` rr) = [l, r]
      -- Merge the scopes if they have different decls within the same range.
      | lr == rr  = [make (mergeDecls ldecls rdecls :> rr :> Nil, descend ldeepen rdeepen)]
      -- The left scope is more local than the right hence try to find where the
      -- right subscope is more local or equal to the left one.
      | leq lr rr = [make (mergeDecls ldecls rdecls :> rr :> Nil, descend [l] rdeepen)]
      -- The right scope is more local than the left hence try to find where the
      -- left subscope is more local or equal to the right one.
      | otherwise = [make (mergeDecls ldecls rdecls :> lr :> Nil, descend ldeepen [r])]

    zipWithMissing, zipWithMatched, zipWithStrategy :: Ord c => (a -> c) -> (a -> a -> b) -> (a -> b) -> [a] -> [a] -> [b]
    zipWithMissing _ _ g [] ys = g <$> ys
    zipWithMissing _ _ g xs [] = g <$> xs
    zipWithMissing p f g xs'@(x : xs) ys'@(y : ys) = case compare (p x) (p y) of
      LT -> g x   : zipWithMissing p f g xs  ys'
      EQ -> f x y : zipWithMissing p f g xs  ys
      GT -> g   y : zipWithMissing p f g xs' ys

    zipWithMatched _ _ _ [] _ = []
    zipWithMatched _ _ _ _ [] = []
    zipWithMatched p f g xs'@(x : xs) ys'@(y : ys) = case compare (p x) (p y) of
      LT ->         zipWithMatched p f g xs  ys'
      EQ -> f x y : zipWithMatched p f g xs  ys
      GT ->         zipWithMatched p f g xs' ys

    zipWithStrategy = case strategy of
      OnUnion -> zipWithMissing
      OnIntersection -> zipWithMatched

    scopeRange :: ScopeTree -> Range
    scopeRange (only -> (_ :> r :> Nil, _)) = r

    descend :: [ScopeTree] -> [ScopeTree] -> [ScopeTree]
    descend xs ys = concat $ zipWithStrategy fst (go `on` snd) (pure . snd) (sortMap xs) (sortMap ys)
      where
        sortMap = sortOn fst . map (scopeRange &&& id)

    -- Merges the references of two 'ScopedDecl's in a left-biased fashion.
    -- In the current implementation, the compiler's scopes will be on the right
    -- and the fallback ones will be on the left.
    mergeRefs :: ScopedDecl -> ScopedDecl -> ScopedDecl
    mergeRefs l r = l
      { _sdRefs = unionOrd (_sdRefs l) (_sdRefs r)
      , _sdDoc  = unionOrd (_sdDoc  l) (_sdDoc  r)
      }

    -- Merge two sets of DeclRefs preferring decls that have a smaller range
    -- (i.e., is more local than the other).
    mergeDecls :: Set DeclRef -> Set DeclRef -> Set DeclRef
    mergeDecls l r
      = mapStrategy
        (\(DeclRef n lr) (DeclRef _ rr) -> DeclRef n (if leq lr rr then lr else rr))
        (mapFromFoldable drName l)
        (mapFromFoldable drName r)
      & Map.elems
      & Set.fromList

    mapFromFoldable :: (Foldable f, Ord k) => (a -> k) -> f a -> Map k a
    mapFromFoldable f = Map.fromList . map (f &&& id) . toList

withScopeForest
  :: (  ([ScopeTree], Map DeclRef ScopedDecl)
     -> ([ScopeTree], Map DeclRef ScopedDecl)
     )
  -> ScopeForest -> ScopeForest
withScopeForest f (ScopeForest ss ds) = uncurry ScopeForest (f (ss, ds))

instance Pretty ScopeForest where
  pp (ScopeForest sf ds) = go sf `above` decls' ds
    where
      go = sexpr "list" . map go'
      go' :: ScopeTree -> Doc
      go' (only -> (decls :> r :> Nil, list')) =
        sexpr "scope" ([pp r] ++ map pp (Set.toList decls) ++ [go list' | not $ null list'])

      decls' = sexpr "decls" . map pp . Map.toList

lookupEnv :: Text -> Scope -> Maybe ScopedDecl
lookupEnv name = getFirst . foldMap \decl ->
  First do
    guard (_sdName decl == name)
    return decl

envAtPoint :: Range -> ScopeForest -> Scope
envAtPoint r (ScopeForest sf ds) = do
  let sp = sf >>= toList . spine r >>= Set.toList
  map (ds Map.!) sp

spine :: Range -> ScopeTree -> DList (Set DeclRef)
spine r (only -> (i, trees))
  | leq r (getRange i) = foldMap (spine r) trees `snoc` getElem @(Set DeclRef) i
  | otherwise = mempty

addLocalScopes :: MonadCatch m => SomeLIGO ParsedInfo -> ScopeForest -> m (SomeLIGO Info')
addLocalScopes tree forest =
  let
    getPreRange xs = let PreprocessedRange r = getElem xs in r
    defaultHandler f (i :< fs) = do
      fs' <- traverse f fs
      let env = envAtPoint (getPreRange i) forest
      return ((env :> Nothing :> i) :< fs')
  in
  withNestedLIGO tree $
    descent @(Product ParsedInfo) @(Product Info') @RawLigoList @RawLigoList defaultHandler
    [ Descent \(i, Name t) -> do
        let env = envAtPoint (getPreRange i) forest
        return (env :> Just TermLevel :> i, Name t)

    , Descent \(i, NameDecl t) -> do
        let env = envAtPoint (getPreRange i) forest
        return (env :> Just TermLevel :> i, NameDecl t)

    , Descent \(i, Ctor t) -> do
        let env = envAtPoint (getPreRange i) forest
        return (env :> Just TermLevel :> i, Ctor t)

    , Descent \(i, TypeName t) -> do
        let env = envAtPoint (getPreRange i) forest
        return (env :> Just TypeLevel :> i, TypeName t)
    ]

addScopes
  :: forall impl m
   . (HasScopeForest impl m, MonadUnliftIO m)
  => AdjacencyMap ParsedContractInfo
  -> m (AdjacencyMap ContractInfo')
addScopes graph = do
  -- Bottom-up: add children forests into their parents
  forestGraph <- scopeForest @impl graph
  let
    universe = nubForest $ foldr (mergeScopeForest OnUnion . contractTree) emptyScopeForest $ G.vertexList forestGraph
    -- Traverse the graph, uniting decls at each intersection, essentially
    -- propagating scopes
    addScope (_getContract -> sf) = do
      let src = _cFile sf
      let fp = srcPath src
      pc <- maybe (throwM $ ContractNotFoundException fp graph) pure (lookupContract fp graph)
      FindContract src
        <$> addLocalScopes (contractTree pc) (mergeScopeForest OnIntersection (_cTree sf) universe)
        <*> pure (_cMsgs sf)
  traverseAMConcurrently addScope forestGraph
  where
    nubRef sd = sd
      { _sdRefs = ordNub (_sdRefs sd)
      , _sdDoc  = ordNub (_sdDoc  sd)
      }
    nubForest f = f
      { sfScopes = ordNub (sfScopes f)
      , sfDecls  = Map.map nubRef (sfDecls f)
      }

-- | Attempt to find a contract in some adjacency map. O(log n)
lookupContract :: FilePath -> AdjacencyMap (FindFilepath a) -> Maybe (FindFilepath a)
lookupContract fp g = fst <$> findKey contractFile fp (G.adjacencyMap g)

pattern FindContract :: Source -> info -> [Msg] -> FindFilepath info
pattern FindContract f t m = FindFilepath (ParsedContract f t m)
{-# COMPLETE FindContract #-}

type ContractInfo       = FindFilepath (SomeLIGO Info)
type ParsedContractInfo = FindFilepath (SomeLIGO ParsedInfo)
type ContractInfo'      = FindFilepath (SomeLIGO Info')

data ContractNotFoundException where
  ContractNotFoundException :: FilePath -> AdjacencyMap (FindFilepath info) -> ContractNotFoundException

instance Pretty ContractNotFoundException where
  pp (ContractNotFoundException cnfPath cnfGraph) =
    "Could not find contract '" <.> pp (pack cnfPath) <.> "'.\n"
    <.> "Searched graph:\n"
    <.> pp (pack $ G.render $ G.export vDoc eDoc cnfGraph)
    where
      vDoc x   = G.literal (contractFile x) <> "\n"
      eDoc x y = G.literal (contractFile x) <> " -> " <> G.literal (contractFile y) <> "\n"

instance Exception ContractNotFoundException
