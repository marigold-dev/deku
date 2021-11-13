{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}

module RIO
  ( RIO
  , RioEnv
  , run

  , getCustomConfig
  , fetchCustomConfig
  , updateCustomConfig

  , registerDidChangeConfiguration

  , source
  , clearDiagnostics

  , Contract (..)
  , delete
  , preload
  , load
  , collectErrors
  , forceFetch
  , fetch
  , forceFetch'
  , fetch'

  , registerFileWatcher
  ) where

-- TODO: break this module into file loading, diagnostics, lsp wrappers and
-- other parts when it grows too big.
-- FIXME [LIGO-252]: Refactor ASTMap and RIO to deal with files that don't exist.

import Prelude hiding (log)

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as G

import Control.Arrow
import Control.Exception.Safe (MonadCatch, MonadThrow, catchIO)
import Control.Monad
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, runReaderT)

import Data.Aeson qualified as Aeson (Result (..), Value, fromJSON)
import Data.Default (def)
import Data.Foldable (find, toList, traverse_)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.List (groupBy, nubBy, sortOn)
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Set qualified as Set
import Data.String.Interpolate (i)

import Language.LSP.Diagnostics qualified as D
import Language.LSP.Server qualified as J
import Language.LSP.Types qualified as J
import Language.LSP.VFS qualified as V

import UnliftIO.MVar

import Witherable (wither)

import Duplo.Tree (make)

import AST hiding (cTree)
import ASTMap (ASTMap)
import ASTMap qualified
import Cli
import Config (Config (..), getConfigFromNotification)
import Duplo.Lattice (Lattice (leq))
import Extension (extGlobs)
import Log qualified
import Parser
import Product
import Range
import Util.Graph (wcc)

data Contract = Contract
  { cTree :: ContractInfo'
  , cDeps :: [J.NormalizedUri]
  }

type RioEnv =
  Product
    '[ MVar Config
     , ASTMap J.NormalizedUri Contract RIO
     , MVar (HashMap J.NormalizedUri Int)
     , "includes" := MVar (AdjacencyMap ParsedContractInfo)
     ]

newtype RIO a = RIO
  { _unRio :: ReaderT RioEnv (J.LspM Config) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader RioEnv
    , MonadThrow
    , MonadCatch
    , MonadUnliftIO
    , J.MonadLsp Config.Config
    )

-- FIXME: LspT does not provide these instances, even though it is just a ReaderT.
-- (Do not forget to remove -Wno-orphans.)
deriving newtype instance MonadThrow m => MonadThrow (J.LspT config m)
deriving newtype instance MonadCatch m => MonadCatch (J.LspT config m)

instance HasLigoClient RIO where
  getLigoClientEnv = fmap (LigoClientEnv . _cLigoBinaryPath) getCustomConfig

-- TODO: The lsp library provides no way to update the Config in the LspM monad
-- manually. So we have to maintain our own config to store the result of
-- `workspace/configuration` requests. We should get this fixed by the
-- maintainers, if possible.
getCustomConfig :: RIO Config
getCustomConfig = do
  mConfig <- asks getElem
  contents <- tryReadMVar mConfig
  case contents of
    -- TODO: The lsp library only sends the `workspace/configuration` request
    -- after initialization is complete, so during initialization, there is no
    -- way to know the client configuration. We need to get this fixed by the
    -- library maintainers, if possible.
    Nothing -> do
      Log.debug "getCustomConfig" "No config fetched yet, resorting to default"
      pure def
    Just config -> pure config

-- Fetch the configuration from the server and write it to the Config MVar
fetchCustomConfig :: RIO (Maybe Config)
fetchCustomConfig = do
  void $
    J.sendRequest J.SWorkspaceConfiguration configRequestParams handleResponse
  tryReadMVar =<< asks getElem
  where
    configRequestParams =
      J.ConfigurationParams (J.List [J.ConfigurationItem Nothing Nothing])

    handleResponse
        :: Either J.ResponseError (J.ResponseResult 'J.WorkspaceConfiguration)
        -> RIO ()
    handleResponse response = do
      config <- parseResponse response
      mConfig <- asks getElem
      _oldConfig <- tryTakeMVar mConfig
      void $ tryPutMVar mConfig config

    parseResponse
      :: Either J.ResponseError (J.ResponseResult 'J.WorkspaceConfiguration)
      -> RIO Config
    parseResponse (Right (J.List [value])) = do
      case Aeson.fromJSON value of
        Aeson.Success c -> pure c
        Aeson.Error _err -> useDefault
    parseResponse _ = useDefault

    useDefault :: RIO Config
    useDefault = do
      Log.debug
        "fetchCustomConfig"
        "Couldn't parse config from server, using default"
      pure def

updateCustomConfig :: Aeson.Value -> RIO ()
updateCustomConfig config = do
  mConfig <- asks getElem
  tryTakeMVar mConfig >>= \case
    Nothing -> decodeConfig def mConfig
    Just oldConfig -> decodeConfig oldConfig mConfig
  where
    log = Log.debug "updateCustomConfig"

    decodeConfig old mConfig = case getConfigFromNotification old config of
      Left err -> do
        log [i|Failed to decode configuration: #{err}|]
        maybe (void $ tryPutMVar mConfig old) (const $ pure ()) =<< fetchCustomConfig
      Right newConfig -> do
        log [i|Set new configuration: #{newConfig}|]
        void $ tryPutMVar mConfig newConfig

registerDidChangeConfiguration :: RIO ()
registerDidChangeConfiguration = do
  let
    reg = J.Registration "ligoChangeConfiguration" J.SWorkspaceDidChangeConfiguration J.Empty
    params = J.RegistrationParams $ J.List [J.SomeRegistration reg]

  void $ J.sendRequest J.SClientRegisterCapability params (const $ pure ())

registerFileWatcher :: RIO ()
registerFileWatcher = do
  let
    watcher extGlob = J.FileSystemWatcher
      { J._globPattern = extGlob
      , J._kind = Just J.WatchKind
        { J._watchChange = True
        , J._watchCreate = True
        , J._watchDelete = True
        }
      }
    regOpts = J.DidChangeWatchedFilesRegistrationOptions $ J.List $ map watcher extGlobs
    reg = J.Registration "ligoFileWatcher" J.SWorkspaceDidChangeWatchedFiles regOpts
    regParams = J.RegistrationParams $ J.List [J.SomeRegistration reg]

  void $ J.sendRequest J.SClientRegisterCapability regParams (const $ pure ())

source :: Maybe J.DiagnosticSource
source = Just "ligo-lsp"

run :: (J.LanguageContextEnv Config, RioEnv) -> RIO a -> IO a
run (lcEnv, env) (RIO action) = J.runLspT lcEnv $ runReaderT action env

fetch, forceFetch :: J.NormalizedUri -> RIO ContractInfo'
fetch = fmap cTree . fetch'
forceFetch = fmap cTree . forceFetch'

fetch', forceFetch' :: J.NormalizedUri -> RIO Contract
fetch' uri = asks getElem >>= ASTMap.fetchCurrent uri
forceFetch' uri = do
  tmap <- asks getElem
  ASTMap.invalidate uri tmap
  ASTMap.fetchCurrent uri tmap

diagnostic :: J.TextDocumentVersion -> [(J.NormalizedUri, [J.Diagnostic])] -> RIO ()
diagnostic ver = traverse_ \(nuri, diags) -> do
  let diags' = D.partitionBySource diags
  maxDiagnostics <- _cMaxNumberOfProblems <$> getCustomConfig
  Log.debug "LOOP.DIAG" [i|Diags #{diags'}|]
  J.publishDiagnostics maxDiagnostics nuri ver diags'

delete :: J.NormalizedUri -> RIO ()
delete uri = do
  imap <- asks $ getTag @"includes"
  let fpM = J.uriToFilePath $ J.fromNormalizedUri uri
  case fpM of
    Nothing -> pure ()
    Just fp -> do
      let
        -- Dummy
        c = FindContract
          (Path fp)
          (SomeLIGO Caml $ make (emptyParsedInfo, Error "Impossible" []))
          []
      modifyMVar_ imap $ pure . G.removeVertex c

  tmap <- asks $ getElem @(ASTMap J.NormalizedUri Contract RIO)
  deleted <- ASTMap.delete uri tmap

  -- Invalidate contracts that are in the same group as the deleted one, as
  -- references might have changed.
  forM_ deleted \(Contract _ deps) ->
    forM_ deps
      (`ASTMap.invalidate` tmap)

preload
  :: J.NormalizedUri
  -> RIO Source
preload uri = do
  let Just fin = J.uriToFilePath $ J.fromNormalizedUri uri  -- FIXME: non-exhaustive
  mvf <- J.getVirtualFile uri
  return case mvf of
    Just vf -> Text fin (V.virtualFileText vf)
    Nothing -> Path fin

loadWithoutScopes
  :: J.NormalizedUri
  -> RIO ContractInfo
loadWithoutScopes uri = do
  src <- preload uri
  ligoEnv <- getLigoClientEnv
  Log.debug "LOAD" [i|running with env #{ligoEnv}|]
  parsePreprocessed src

-- | Like 'loadWithoutScopes', but if an 'IOException' has ocurred, then it will
-- return 'Nothing'.
tryLoadWithoutScopes :: J.NormalizedUri -> RIO (Maybe ParsedContractInfo)
tryLoadWithoutScopes uri = (Just . insertPreprocessorRanges <$> loadWithoutScopes uri) `catchIO` const (pure Nothing)

load
  :: J.NormalizedUri
  -> RIO Contract
load uri = J.getRootPath >>= \case
  Nothing -> Contract <$> loadDefault <*> pure [uri]
  Just root -> asks (getTag @"includes") >>= flip modifyMVar \includes -> do
    tmap <- asks getElem

    rootContract <- loadWithoutScopes uri
    let rootFileName = contractFile rootContract
    let groups = wcc includes
    rawGraph <- case find (isJust . lookupContract rootFileName) groups of
      -- Possibly the graph hasn't been initialized yet or a new file was created.
      Nothing -> parseContractsWithDependencies (loadWithoutScopes . sourceToUri) root
      Just oldIncludes -> do
        let (rootContract', includeEdges) = extractIncludedFiles True rootContract
        let lookupOrLoad fp = maybe
              (tryLoadWithoutScopes (normalizeFilePath fp))
              (pure . Just)
              (lookupContract fp oldIncludes)
        newIncludes <- wither (lookupOrLoad . snd) (toList includeEdges)
        let
          newSet = Set.fromList newIncludes
          oldSet = Map.keysSet $ G.adjacencyMap oldIncludes
          newVertices = Set.difference newSet oldSet
          removedVertices = Set.difference oldSet newSet
          groups' = filter (isNothing . lookupContract rootFileName) groups
          -- Replacing a contract with itself looks unintuitive but it works
          -- because ordering is decided purely on the file name.
          newGroup =
            G.overlay (G.fromAdjacencySets [(rootContract', newVertices)])
            $ G.replaceVertex rootContract' rootContract'
            $ foldr (G.removeEdge rootContract') oldIncludes removedVertices
        pure $ G.overlays (newGroup : groups')

    (graph, result) <- case J.uriToFilePath $ J.fromNormalizedUri uri of
      Nothing -> (,) <$> addScopes @Fallback rawGraph <*> loadDefault
      Just fp -> case find (isJust . lookupContract fp) (wcc rawGraph) of
        Nothing -> (,) <$> addScopes @Fallback rawGraph <*> loadDefault
        Just graph' -> do
          scoped <- addScopes @Fallback graph'
          (scoped, ) <$> maybe loadDefault pure (lookupContract fp scoped)

    let contracts = (id &&& normalizeFilePath . contractFile) <$> G.vertexList graph
    let nuris = snd <$> contracts
    forM_ contracts \(contract, nuri) ->
      ASTMap.insert nuri (Contract contract nuris) tmap

    pure (rawGraph, Contract result nuris)

  where
    sourceToUri = normalizeFilePath . srcPath
    normalizeFilePath = J.toNormalizedUri . J.filePathToUri
    loadDefault = addShallowScopes @Fallback =<< loadWithoutScopes uri

collectErrors
  :: (J.NormalizedUri -> RIO ContractInfo')
  -> J.NormalizedUri
  -> Maybe Int
  -> RIO ()
collectErrors fetcher uri version = fetcher uri >>= \contract -> do
  -- Correct the ranges of the error messages to correspond to real locations
  -- instead of locations after preprocessing.
  let errs' = nubBy (leq `on` fst) $ collectAllErrors contract
  let diags = errorToDiag <$> errs'
  let extractGroup :: [[(J.NormalizedUri, J.Diagnostic)]] -> [(J.NormalizedUri, [J.Diagnostic])]
      extractGroup [] = []
      extractGroup ([] : xs) = extractGroup xs
      extractGroup (ys@(y : _) : xs) = (fst y, snd <$> ys) : extractGroup xs
  let grouped = extractGroup $ groupBy ((==) `on` fst) $ sortOn fst diags
  diagnostic version grouped

clearDiagnostics :: Foldable f => f J.NormalizedUri -> RIO ()
clearDiagnostics uris = do
  maxDiagnostics <- _cMaxNumberOfProblems <$> getCustomConfig
  forM_ uris $ \nuri ->
    J.publishDiagnostics maxDiagnostics nuri Nothing mempty

errorToDiag :: (Range, Error a) -> (J.NormalizedUri, J.Diagnostic)
errorToDiag (getRange -> (Range (sl, sc, _) (el, ec, _) f), Error what _) =
  ( J.toNormalizedUri $ J.filePathToUri f
  , J.Diagnostic
    (J.Range begin end)
    (Just J.DsError)
    Nothing
    source
    what
    (Just $ J.List [])
    Nothing
  )
  where
    begin = J.Position (sl - 1) (sc - 1)
    end   = J.Position (el - 1) (ec - 1)
