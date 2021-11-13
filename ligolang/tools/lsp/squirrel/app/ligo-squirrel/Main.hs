{-# LANGUAGE PolyKinds #-}

module Main (main) where

import Prelude hiding (log)

import Algebra.Graph.AdjacencyMap qualified as G (empty)
import Control.Exception.Safe (MonadCatch, catchAny, displayException)
import Control.Lens hiding ((:>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, void, when)

import Data.Default
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as T

import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import Language.LSP.Types.Lens qualified as J

import UnliftIO.MVar

import AST
import ASTMap qualified
import Config qualified
import Language.LSP.Util (sendError)
import Log qualified
import Product
import RIO (RIO, RioEnv)
import RIO qualified
import Range
import System.Exit
import System.Log qualified as L
import Util (toLocation)

main :: IO ()
main = do
  Log.setLogLevel Log.ERROR
  exit =<< mainLoop

mainLoop :: IO Int
mainLoop = do
    let
      serverDefinition = S.ServerDefinition
        { S.onConfigurationChange = Config.getConfigFromNotification
        , S.defaultConfig = def
        , S.doInitialize = \lcEnv _msg -> Right . (lcEnv, ) <$> initialize
        , S.staticHandlers = catchExceptions handlers
        , S.interpretHandler = \envs -> S.Iso (RIO.run envs) liftIO
        , S.options = lspOptions
        }

    S.setupLogger Nothing [] L.EMERGENCY
    S.runServer serverDefinition
  where
    syncOptions :: J.TextDocumentSyncOptions
    syncOptions = J.TextDocumentSyncOptions
      { J._openClose         = Just True
      , J._change            = Just J.TdSyncIncremental
      , J._willSave          = Just False
      , J._willSaveWaitUntil = Just False
      , J._save              = Just $ J.InR $ J.SaveOptions $ Just False
      }

    lspOptions :: S.Options
    lspOptions = def
      { S.textDocumentSync       = Just syncOptions
      , S.executeCommandCommands = Just ["lsp-hello-command"]
      , S.signatureHelpTriggerCharacters = Just ['(', ' ']
      , S.signatureHelpRetriggerCharacters = Just [',']
      }

    -- | Handle all uncaught exceptions.
    catchExceptions
      :: forall m config. (MonadCatch m, S.MonadLsp config m)
      => S.Handlers m -> S.Handlers m
    catchExceptions = S.mapHandlers wrapReq wrapNotif
      where
        wrapReq
          :: forall (meth :: J.Method 'J.FromClient 'J.Request).
             S.Handler m meth -> S.Handler m meth
        wrapReq handler msg@J.RequestMessage{_method} resp =
          handler msg resp `catchAny` \e -> do
            Log.err "Uncaught" $ "Handling `" <> show _method <> "`: " <> displayException e
            resp . Left $ J.ResponseError J.InternalError (T.pack $ displayException e) Nothing

        wrapNotif
          :: forall (meth :: J.Method 'J.FromClient 'J.Notification).
             S.Handler m meth -> S.Handler m meth
        wrapNotif handler msg@J.NotificationMessage{_method} =
          handler msg `catchAny` \e -> do
            Log.err "Uncaught" $ "Handling `" <> show _method <> "`: " <> displayException e
            sendError . T.pack $ "Error handling `" <> show _method <> "` (see logs)."

initialize :: IO RioEnv
initialize = do
  config <- newEmptyMVar
  astMap <- ASTMap.empty RIO.load
  openDocs <- newMVar HashMap.empty
  includes <- newMVar G.empty
  pure (config :> astMap :> openDocs :> Tag includes :> Nil)

handlers :: S.Handlers RIO
handlers = mconcat
  [ S.notificationHandler J.SInitialized handleInitialized

  , S.notificationHandler J.STextDocumentDidOpen handleDidOpenTextDocument
  , S.notificationHandler J.STextDocumentDidChange handleDidChangeTextDocument
  , S.notificationHandler J.STextDocumentDidSave (\_msg -> pure ())
  , S.notificationHandler J.STextDocumentDidClose handleDidCloseTextDocument

  , S.requestHandler J.STextDocumentDefinition handleDefinitionRequest
  , S.requestHandler J.STextDocumentTypeDefinition handleTypeDefinitionRequest
  , S.requestHandler J.STextDocumentReferences handleFindReferencesRequest
  , S.requestHandler J.STextDocumentCompletion handleCompletionRequest
  --, S.requestHandler J.SCompletionItemResolve handleCompletionItemResolveRequest
  , S.requestHandler J.STextDocumentSignatureHelp handleSignatureHelpRequest
  , S.requestHandler J.STextDocumentFoldingRange handleFoldingRangeRequest
  , S.requestHandler J.STextDocumentSelectionRange handleSelectionRangeRequest
  , S.requestHandler J.STextDocumentDocumentSymbol handleDocumentSymbolsRequest
  , S.requestHandler J.STextDocumentHover handleHoverRequest
  , S.requestHandler J.STextDocumentRename handleRenameRequest
  , S.requestHandler J.STextDocumentPrepareRename handlePrepareRenameRequest
  , S.requestHandler J.STextDocumentFormatting handleDocumentFormattingRequest
  , S.requestHandler J.STextDocumentRangeFormatting handleDocumentRangeFormattingRequest
  , S.requestHandler J.STextDocumentCodeAction handleTextDocumentCodeAction
  -- , S.requestHandler J.STextDocumentOnTypeFormatting

  , S.notificationHandler J.SCancelRequest (\_msg -> pure ())
  , S.notificationHandler J.SWorkspaceDidChangeConfiguration handleDidChangeConfiguration
  , S.notificationHandler J.SWorkspaceDidChangeWatchedFiles handleDidChangeWatchedFiles
  --, S.requestHandler J.SWorkspaceExecuteCommand _
  ]

handleInitialized :: S.Handler RIO 'J.Initialized
handleInitialized _ = do
  RIO.registerDidChangeConfiguration
  void RIO.fetchCustomConfig
  RIO.registerFileWatcher

handleDidOpenTextDocument :: S.Handler RIO 'J.TextDocumentDidOpen
handleDidOpenTextDocument notif = do
  let uri = notif^.J.params.J.textDocument.J.uri.to J.toNormalizedUri
  let ver = notif^.J.params.J.textDocument.J.version

  openDocsVar <- asks getElem
  modifyMVar_ openDocsVar \openDocs -> do
    RIO.collectErrors RIO.forceFetch uri (Just ver)
    pure $ HashMap.insert uri ver openDocs

-- FIXME: Suppose the following scenario:
-- * VSCode has `squirrel/test/contracts/` open as folder;
-- * You open `find/includes/A3.mligo`;
-- * You quickly edit it to contain some mistake (e.g.: replace 40 with 4f0) so
-- that `handleDidOpenTextDocument` might be still running when this function is
-- called.
-- Now there is something weird going on: VSCode might not put an error on `4f0`
-- because we got an outdated version (from `fetchBundled`). Even more oddly, if
-- we now delete the `f`, it might highlight `40` as incorrect from the previous
-- error.
-- This can be fixed by opening another contract in WCC or, sometimes, just
-- closing and opening this document again.
handleDidChangeTextDocument :: S.Handler RIO 'J.TextDocumentDidChange
handleDidChangeTextDocument notif = do
  tmap <- asks getElem
  let uri = notif^.J.params.J.textDocument.J.uri.to J.toNormalizedUri
  let ver = notif^.J.params.J.textDocument.J.version
  ASTMap.invalidate uri tmap
  RIO.Contract doc nuris <- ASTMap.fetchBundled uri tmap
  -- Clear diagnostics for all contracts in this WCC and then send diagnostics
  -- collected from this uri.
  -- The usage of `openDocsVar` here serves purely as a mutex to prevent race
  -- conditions.
  openDocsVar <- asks (getElem @(MVar (HashMap J.NormalizedUri Int)))
  modifyMVar_ openDocsVar \openDocs -> do
    RIO.clearDiagnostics nuris
    RIO.collectErrors (const (pure doc)) uri ver
    pure openDocs

handleDidCloseTextDocument :: S.Handler RIO 'J.TextDocumentDidClose
handleDidCloseTextDocument notif = do
  let uri = notif^.J.params.J.textDocument.J.uri.to J.toNormalizedUri
  tmap <- asks getElem
  RIO.Contract _ nuris <- ASTMap.fetchCached uri tmap

  openDocsVar <- asks (getElem @(MVar (HashMap J.NormalizedUri Int)))
  modifyMVar_ openDocsVar \openDocs -> do
    let openDocs' = HashMap.delete uri openDocs
    -- Clear diagnostics for all contracts in this WCC group if all of them were closed.
    let nuriMap = HashMap.fromList ((, ()) <$> nuris)
    when (HashMap.null $ HashMap.intersection openDocs' nuriMap) $
      RIO.clearDiagnostics nuris
    pure openDocs'

handleDefinitionRequest :: S.Handler RIO 'J.TextDocumentDefinition
handleDefinitionRequest req respond = do
    -- XXX: They forgot lenses for DefinitionParams :/
    {-
    let uri = req^.J.textDocument.J.uri
    let pos = fromLspPosition $ req^.J.position
    -}
    Log.debug "Definition" [i|Got request: #{req}|]
    let
      J.DefinitionParams{_textDocument, _position} = req ^. J.params
      uri = _textDocument ^. J.uri
      pos = fromLspPosition _position
    tree <- contractTree <$> RIO.fetch (J.toNormalizedUri uri)
    let location = case AST.definitionOf pos tree of
          Just defPos -> [toLocation defPos]
          Nothing     -> []
    Log.debug "Definition" [i|Definition request returned #{location}|]
    respond . Right . J.InR . J.InL . J.List $ location

handleTypeDefinitionRequest :: S.Handler RIO 'J.TextDocumentTypeDefinition
handleTypeDefinitionRequest req respond = do
    let
      J.TypeDefinitionParams{_textDocument, _position} = req ^. J.params
      uri = _textDocument ^. J.uri
      pos = _position ^. to fromLspPosition
    tree <- contractTree <$> RIO.fetch (J.toNormalizedUri uri)
    let wrapAndRespond = respond . Right . J.InR . J.InL . J.List
    let definition = case AST.typeDefinitionAt pos tree of
          Just defPos -> [J.Location uri $ toLspRange defPos]
          Nothing     -> []
    Log.debug "TypeDefinition" [i|Type definition request returned #{definition}|]
    wrapAndRespond definition

handleDocumentFormattingRequest :: S.Handler RIO 'J.TextDocumentFormatting
handleDocumentFormattingRequest req respond = do
  let
    uri = req ^. J.params . J.textDocument . J.uri
  tree <- contractTree <$> RIO.fetch (J.toNormalizedUri uri)
  respond . Right =<< AST.formatDocument tree

handleDocumentRangeFormattingRequest :: S.Handler RIO 'J.TextDocumentRangeFormatting
handleDocumentRangeFormattingRequest req respond = do
  let
    uri = req ^. J.params . J.textDocument . J.uri
    pos = fromLspRange $ req ^. J.params . J.range
  tree <- contractTree <$> RIO.fetch (J.toNormalizedUri uri)
  respond . Right =<< AST.formatAt pos tree

handleFindReferencesRequest :: S.Handler RIO 'J.TextDocumentReferences
handleFindReferencesRequest req respond = do
    let (_, nuri, pos) = getUriPos req
    tree <- contractTree <$> RIO.fetch nuri
    let locations = case AST.referencesOf pos tree of
          Just refs -> toLocation <$> refs
          Nothing   -> []
    Log.debug "FindReferences" [i|Find references request returned #{locations}|]
    respond . Right . J.List $ locations

handleCompletionRequest :: S.Handler RIO 'J.TextDocumentCompletion
handleCompletionRequest req respond = do
    Log.debug "Completion" [i|Request: #{show req}|]
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    let pos = fromLspPosition $ req ^. J.params . J.position
    tree <- contractTree <$> RIO.fetch uri
    let completions = fmap toCompletionItem . fromMaybe [] $ complete pos tree
    Log.debug "Completion" [i|Completion request returned #{completions}|]
    respond . Right . J.InL . J.List $ completions

{-
handleCompletionItemResolveRequest :: S.Handler RIO 'J.CompletionItemResolve
handleCompletionItemResolveRequest req respond = do
    Log.debug "Completion resolve" [i|Request: #{show req}|]
    respond . Right $  req ^. J.params
-}

handleSignatureHelpRequest :: S.Handler RIO 'J.TextDocumentSignatureHelp
handleSignatureHelpRequest req respond = do
  -- XXX: They forgot lenses for  SignatureHelpParams :/
  {-
  let uri = req ^. J.params . J.textDocument . J.uri
  let position = req ^. J.params . J.position & fromLspPosition
  -}
  let
    J.SignatureHelpParams{_textDocument, _position} = req ^. J.params
    uri = _textDocument ^. J.uri
    position = fromLspPosition _position
  tree <- contractTree <$> RIO.fetch (J.toNormalizedUri uri)
  let signatureHelp = getSignatureHelp (tree ^. nestedLIGO) position
  respond . Right $ signatureHelp

handleFoldingRangeRequest :: S.Handler RIO 'J.TextDocumentFoldingRange
handleFoldingRangeRequest req respond = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    tree <- contractTree <$> RIO.fetch uri
    actions <- foldingAST (tree ^. nestedLIGO)
    respond . Right . J.List $ toFoldingRange <$> actions

handleTextDocumentCodeAction :: S.Handler RIO 'J.TextDocumentCodeAction
handleTextDocumentCodeAction req respond = do
    let
      uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
      r = req ^. J.params . J.range . to fromLspRange
      con = req ^. J.params . J.context
    tree <- contractTree <$> RIO.fetch uri
    actions <- collectCodeActions r con (J.fromNormalizedUri uri) tree
    let response = Right . J.List . fmap J.InR $ actions
    respond response

handleSelectionRangeRequest :: S.Handler RIO 'J.TextDocumentSelectionRange
handleSelectionRangeRequest req respond = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    let positions = req ^. J.params . J.positions ^.. folded
    tree <- contractTree <$> RIO.fetch uri
    let results = map (findSelectionRange (tree ^. nestedLIGO)) positions
    respond . Right . J.List $ results

handleDocumentSymbolsRequest :: S.Handler RIO 'J.TextDocumentDocumentSymbol
handleDocumentSymbolsRequest req respond = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    tree <- contractTree <$> RIO.fetch uri
    result <- extractDocumentSymbols (J.fromNormalizedUri uri) tree
    respond . Right . J.InR . J.List $ result

handleHoverRequest :: S.Handler RIO 'J.TextDocumentHover
handleHoverRequest req respond = do
    -- XXX: They forgot lenses for  HoverParams :/
    {-
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    let pos = fromLspPosition $ req ^. J.params . J.position
    -}
    let
      J.HoverParams{_textDocument, _position} = req ^. J.params
      uri = _textDocument ^. J.uri . to J.toNormalizedUri
      pos = fromLspPosition _position
    tree <- contractTree <$> RIO.fetch uri
    respond . Right $ hoverDecl pos tree

handleRenameRequest :: S.Handler RIO 'J.TextDocumentRename
handleRenameRequest req respond = do
    Log.debug "Rename" [i|Request: #{show req}|]
    let (_, nuri, pos) = getUriPos req
    let newName = req ^. J.params . J.newName

    tree <- contractTree <$> RIO.fetch nuri

    case renameDeclarationAt pos tree newName of
      NotFound -> do
        Log.debug "Rename" [i|Declaration not found for: #{show req}|]
        respond . Left $
          J.ResponseError J.InvalidRequest "Cannot rename this" Nothing
      Ok edits -> do
        let
          -- XXX: This interface has two benefits: it allows to refer to a specific
          -- document version and it allows the creation/deletion/renaming of files.
          -- In this case we do not care about the latter and the actual usefulness
          -- of the former is not clear either, but it might be worth switching
          -- to it when we support versions.
          --documentChanges = J.List
          --  [ J.TextDocumentEdit
          --      { _textDocument = J.VersionedTextDocumentIdentifier uri Nothing
          --      , _edits = J.List edits
          --      }
          --  ]

          response =
            J.WorkspaceEdit
              { _changes = Just edits
              , _documentChanges = Nothing
              , _changeAnnotations = Nothing
              }
        Log.debug "Rename" [i|Rename request returned #{response}|]
        respond . Right $ response

handlePrepareRenameRequest :: S.Handler RIO 'J.TextDocumentPrepareRename
handlePrepareRenameRequest req respond = do
    let (_, nuri, pos) = getUriPos req

    tree <- contractTree <$> RIO.fetch nuri

    respond . Right . fmap (J.InL . toLspRange) $ prepareRenameDeclarationAt pos tree

handleDidChangeConfiguration :: S.Handler RIO 'J.WorkspaceDidChangeConfiguration
handleDidChangeConfiguration notif = do
  let config = notif ^. J.params . J.settings
  RIO.updateCustomConfig config

handleDidChangeWatchedFiles :: S.Handler RIO 'J.WorkspaceDidChangeWatchedFiles
handleDidChangeWatchedFiles notif = do
  let J.List changes = notif ^. J.params . J.changes
  for_ changes \(J.FileEvent (J.toNormalizedUri -> uri) change) -> case change of
    J.FcCreated -> do
      log [i|Created #{uri}|]
      void $ RIO.forceFetch' uri
    J.FcChanged -> do
      log [i|Changed #{uri}|]
      void $ RIO.forceFetch' uri
    J.FcDeleted -> do
      log [i|Deleted #{uri}|]
      RIO.delete uri
  where
    log = Log.debug "WorkspaceDidChangeWatchedFiles"

getUriPos
  :: ( J.HasPosition (J.MessageParams m) J.Position
     , J.HasUri a J.Uri
     , J.HasTextDocument (J.MessageParams m) a
     )
  => J.RequestMessage m
  -> (J.Uri, J.NormalizedUri, Range)
getUriPos req =
  let
    uri  = req ^. J.params . J.textDocument . J.uri
    nuri = J.toNormalizedUri uri
    pos  = fromLspPositionUri (req ^. J.params . J.position) uri
  in (uri, nuri, pos)

exit :: Int -> IO ()
exit 0 = exitSuccess
exit n = exitWith (ExitFailure n)
