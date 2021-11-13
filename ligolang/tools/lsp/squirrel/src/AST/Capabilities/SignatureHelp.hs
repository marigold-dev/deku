{-# LANGUAGE RecordWildCards #-}

module AST.Capabilities.SignatureHelp
  ( LSP.ParameterInformation (..)
  , LSP.SignatureInformation (..)
  , findSignature
  , getSignatureHelp
  , toLspParameters
  ) where

import Language.LSP.Types qualified as LSP
  (List (..), ParameterInformation (..), ParameterLabel (..), SignatureHelp (..),
   SignatureHelpDoc (..), SignatureInformation (..))

import Control.Applicative ((<|>))
import Control.Lens (_1, _Just, (^..), (^?))
import Control.Monad (void, when)
import Control.Monad.Trans.RWS
import Data.Foldable (asum)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (..))
import Data.Text (Text)
import Data.Text qualified as Text (intercalate, unwords)
import Duplo.Lattice (leq)
import Duplo.Pretty (fsep, pp, ppToText)
import Duplo.Tree (extract, layer, match, spineTo)

import AST.Capabilities.Find (CanSearch)
import AST.Scope.Common (Level (TermLevel), lookupEnv, ofLevel)
import AST.Scope.ScopedDecl
  ( Parameter (..), Pattern (..), ScopedDecl (..), Type (..), TypeDeclSpecifics (_tdsInit)
  , lppLigoLike, _ValueSpec, vdsParams
  )
import AST.Skeleton (Expr (Apply, Paren, Tuple), LIGO, Lang (..))
import Product (Contains, Product, getElem)
import Range (Range (..), getRange)

-- | Find a 'ScopedDecl' of a function that is applied at the given position and the active
-- parameter number (zero if no parameters).
findNestingFunction
  :: CanSearch xs => LIGO xs -> Range -> Maybe (ScopedDecl, [LIGO xs])
findNestingFunction tree position = do
  (callInfo, fName, args) <- asum (map extractFunctionCall covers)
  let termEnv = filter (ofLevel TermLevel) (getElem callInfo)
  decl <- lookupEnv fName termEnv
  pure (decl, args)
  where
    covers = spineTo (leq position . getElem) tree

-- | If the given tree is a function application, extract it's information
-- characteristics, the function's name and applied parameters.
extractFunctionCall
  :: LIGO xs -> Maybe (Product xs, Text, [LIGO xs])
extractFunctionCall tree = do
  (i, Apply name params) <- match tree
  pure (i, ppToText (void name), params)

matchValuesAndTypes
  :: forall xs. Contains Range xs
  => Range -> [Parameter] -> [LIGO xs] -> (Maybe Int, [Parameter])
matchValuesAndTypes position params' args' =
  (`appEndo` []) <$> execRWS (go params' args') 0 Nothing
  where
    go :: [Parameter] -> [LIGO xs] -> RWS Int (Endo [Parameter]) (Maybe Int) ()
    go (param : params) (arg : args) = do
      ix <- ask
      let
        (getRange -> argPos, argsM) = getArgTuple arg
        (ixIncr, newActiveParamNo, newParams) = case (getParamTuple param, argsM) of
          (Just pats, Just args'') ->
            (length pats, ix + findTupleIx args'', map ParameterPattern pats)
          (_, _) -> (1, ix, [param])
      when (position `leq` argPos) $
        put $ Just newActiveParamNo
      tell $ Endo (newParams <>)
      local (+ ixIncr) $ go params args
    -- There are more arguments than parameters, so the user has typed extra
    -- arguments. We stop here.
    go [] (_ : _) = pure ()
    -- There are more parameters than arguments, so the user has not typed
    -- everything yet.
    go (param : params) [] = do
      ix <- ask
      modify \activeParamNo -> activeParamNo <|> Just ix
      tell $ Endo ((param : params) <>)
    go [] [] = pure ()

    getParamTuple :: Parameter -> Maybe [Pattern]
    getParamTuple = \case
      ParameterPattern (IsAnnot (IsTuple pats) (TupleType _)) -> Just pats
      ParameterPattern (IsTuple pats) -> Just pats
      ParameterPattern (IsParen (IsTuple pats)) -> Just pats
      _ -> Nothing

    findTupleIx :: [LIGO xs] -> Int
    findTupleIx args =
      fromMaybe (length args - 1)
      $ findIndex ((`notToTheLeftOf` position) . getRange) args

    notToTheLeftOf :: Range -> Range -> Bool
    Range _ finish _ `notToTheLeftOf` Range start _ _ = finish >= start

    getArgTuple :: LIGO xs -> (Product xs, Maybe [LIGO xs])
    getArgTuple arg = case match arg of
      Just (r, Tuple vals) -> (r, Just vals)
      Just (r, Paren (layer -> Just (Tuple vals))) -> (r, Just vals)
      _ -> (extract arg, Nothing)

-- | Converts tuples in the form `(a, b) : (x * y)` into `((a : x), (b : y))`
-- for ease of visualization. Such conversion is done if and only if we have a
-- tuple pattern and a tuple type together such that both have the same number
-- of parameters.
annotTupleToTupleAnnots :: Parameter -> Parameter
annotTupleToTupleAnnots = \case
  p@(ParameterPattern (IsAnnot (IsTuple pats) (TupleType tys)))
    | length pats == length tys ->
      ParameterPattern $ IsParen $ IsTuple $ zipWith (\pat ty -> IsAnnot pat $ _tdsInit ty) pats tys
    | otherwise -> p
  p -> p

-- | Find all function signatures (one in this implementation) that could be
-- applied at the given position. A function signature includes its label which
-- is what will represent the function, its documentation comments and its
-- parameters (if present, they must be a part of the label). Parameters might
-- be highlighted by the editor.
findSignature
  :: CanSearch xs
  => LIGO xs -> Range -> Maybe (LSP.SignatureInformation, Maybe Int)
findSignature tree position = do
  (ScopedDecl{..}, args) <- findNestingFunction tree position
  params <- _sdSpec ^? _ValueSpec . vdsParams . _Just
  let prettifiedParams = map annotTupleToTupleAnnots params
  let (activeNo, params') = matchValuesAndTypes position prettifiedParams args
  let label = makeSignatureLabel _sdDialect _sdName (map (ppToText . lppLigoLike _sdDialect) prettifiedParams)
  let sigInfo = LSP.SignatureInformation
        { _label = label
        , _documentation = Just (LSP.SignatureHelpDocString $ ppToText (fsep (map pp _sdDoc)))
        , _parameters = Just . LSP.List $ toLspParameters _sdDialect params'
        , _activeParameter = Nothing
        }
  pure (sigInfo, activeNo)

-- | Make a function signature label by a dialect, a function name and its parameters.
makeSignatureLabel :: Lang -> Text -> [Text] -> Text
makeSignatureLabel Pascal name params
  = "function " <> name <> " (" <> Text.intercalate "; " params <> ")"
makeSignatureLabel Caml name params
  = "let " <> name <> " " <> Text.unwords params
makeSignatureLabel Reason name params
  = "let " <> name <> " = (" <> Text.intercalate ", " params <> ")"

toLspParameters :: Lang -> [Parameter] -> [LSP.ParameterInformation]
toLspParameters dialect = map (toLspParameter . ppToText . lppLigoLike dialect)

-- | Make a 'ParameterInformation' by a parameter's name. For now, we don't
-- support parameter docs.
toLspParameter :: Text -> LSP.ParameterInformation
toLspParameter name = LSP.ParameterInformation (LSP.ParameterLabelString name) Nothing

-- | Find all function signatures at the given position and wrap them up in a
-- 'SignatureHelp'.
--
-- See 'findSignatures'.
getSignatureHelp
  :: CanSearch xs
  => LIGO xs -> Range -> LSP.SignatureHelp
getSignatureHelp tree position = LSP.SignatureHelp
  { _signatures = LSP.List (sigResult ^.. _Just . _1)
  , _activeSignature = Just 0
  , _activeParameter = snd =<< sigResult
  }
  where
    sigResult = findSignature tree position
