-- | ligo version: a98a94dd5cadb791a2d4db1d60dde73b1a132811
-- | The definition of type as is represented in ligo JSON output

{-# LANGUAGE DeriveGeneric, RecordWildCards, TupleSections #-}

module Cli.Json
  ( LigoError (..)
  , LigoErrorContent (..)
  , LigoScope (..)
  , LigoDefinitions (..)
  , LigoDefinitionsInner (..)
  , LigoDefinitionScope (..)
  , LigoTypeFull (..)
  , LigoTypeContent (..)
  , LigoTypeContentInner (..)
  , LigoRecordField (..)
  , LigoRange (..)
  , LigoRangeInner (..)
  , LigoByte (..)
  , mbFromLigoRange
  , fromLigoRangeOrDef
  , fromLigoErrorToMsg
  , fromLigoTypeFull
  , mkLigoError
  , toScopedDecl
  , prepareField
  )
where

import Control.Applicative (Alternative ((<|>)), liftA2)
import Control.Lens.Operators
import Control.Monad.State
import Data.Aeson.Types hiding (Error)
import Data.Char (isUpper, toLower)
import Data.Foldable (asum, toList)
import Data.Function
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text (unpack)
import GHC.Generics
import Text.Read (readEither)
import Text.Regex.TDFA ((=~), getAllTextSubmatches)

import AST.Scope.ScopedDecl (DeclarationSpecifics (..), ScopedDecl (..), ValueDeclSpecifics (..))
import AST.Scope.ScopedDecl.Parser (parseTypeDeclSpecifics)
import AST.Skeleton hiding (String)
import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree
import Parser
import Product
import Range hiding (startLine)

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

-- | Node representing ligo error with additional meta
data LigoError = LigoError
  { -- | `"status"`
    _leStatus :: Text
    -- | Stage on where the error appeared (parser/typechecker)
    -- `"stage"`
  , _leStage :: Text
    -- | Error message block
    -- `"content"`
  , _leContent :: LigoErrorContent
  }
  deriving stock (Eq, Generic, Show)

-- | An actual ligo error
data LigoErrorContent = LigoErrorContent
  { -- | Error message
    -- `"message"`
    _lecMessage :: Text
    -- | Location of the error
    -- `"location"`
  , _lecLocation :: LigoRange
  }
  deriving stock (Eq, Generic, Show)

-- | Whole successfull ligo `get-scope` output
data LigoDefinitions = LigoDefinitions
  { -- | All the definitions
    -- `"definitions"`
    _ldDefinitions :: LigoDefinitionsInner
    -- | Scopes
    -- `"scopes"`
  , _ldScopes :: [LigoScope]
  }
  deriving stock (Generic, Show)

-- | First part under `"variables"` constraint
newtype LigoDefinitionsInner = LigoDefinitionsInner
  { -- | `"variables"`
    _ldiVariables :: HM.HashMap Text LigoDefinitionScope
  }
  deriving stock (Generic, Show)

-- | Scope that goes as a member of the list under `"scopes"` constraint
-- ```
-- { "scopes" : [LigoScope] }
-- ```
data LigoScope = LigoScope
  { -- | We parse it by a chunks of 2, each odd element of array is a name for
    -- the corresponding element which is `LigoRangeInner`.
    -- ```
    -- { "range": [ "<scope>", LigoRangeInner ] }
    -- ```
    _lsRange :: LigoRange
    -- | `"expression_environment"`
  , _lsExpressionEnvironment :: [Text]
    -- | `"type_environment"`
  , _lsTypeEnvironment :: Value
  }
  deriving stock (Generic, Show)

-- | Definition declaration that goes from `"definitions"` constraint
-- ```
-- { "definitions" { a#n : LigoDefinitionScope } }
-- ```
data LigoDefinitionScope = LigoDefinitionScope
  { -- | `"name"`
    _ldsName :: Text
    -- | Binding location
    -- `"location"`
  , _ldsRange :: LigoRange
    -- | Definition body location
    -- `"body_location"`
  , _ldsBodyRange :: LigoRange
    -- | The type itself
    -- `"t"`
  , _ldsT :: Maybe LigoTypeFull
    -- | We parse it in chunks of 2, each odd element of the array is a name for
    -- the corresponding element which is `LigoRangeInner`.
    -- ```
    -- { "references": [ ["<scope>", LigoRangeInner] ] }
    -- ```
    -- `"references"`
  , _ldsReferences :: [LigoRange]
  }
  deriving stock (Generic, Show)

-- | Parameter of a type
-- ```
-- { "parameters": [LigoTypeParameter] }
-- ```
data LigoTypeParameter
  = LigoTypeParameter
      { -- | We parse it by a chunks of 2, each odd element of array is a name for
        -- even element which is `LigoTypeContentInner`.
        -- ```
        -- { "type_content": [ <name>, LigoTypeContentInner ] }
        -- ```
        _ltpTypeContent :: [LigoTypeContent]
        -- | `"type_meta"`
      , _ltpTypeMeta :: Value -- Maybe LigoTypeFull
        -- | `"location"`
      , _ltpLocation :: LigoRange
        -- | `"orig_var"`
      , _ltpOrigVar :: Maybe Text
      }
      deriving stock (Generic, Show)

-- | Whole ligo type.
-- ```
-- { "t" : LigoTypeFull }
-- ```
data LigoTypeFull
  =
    LigoTypeFullCore
      { -- | Location of the definition.
        _ltfcLocation :: LigoRange
      , -- | We parse it by a chunks of 2, each odd element of array is a name for
        -- even element which is `LigoTypeContentInner`.
        -- ```
        -- { "type_content": [ <name>, LigoTypeContentInner ] }
        -- ```
        _ltfcTypeContent :: [LigoTypeContent]
      }
  | LigoTypeFullResolved
      { -- | `"location"`
        _ltfrLocation :: LigoRange
      , -- | *Some* meta constructors (e.g. `Some`).
        -- `"type_meta"`
        _ltfrTypeMeta :: Value
      , -- | `"orig_var"`
        _ltfrOrigVar :: Maybe Text
      , -- | We parse it by a chunks of 2, each odd element of array is a name for
        -- even element which is `LigoTypeContentInner`.
        -- ```
        -- { "type_content": [ <name>, LigoTypeContentInner ] }
        -- ```
        _ltfrTypeContent :: [LigoTypeContent]
      }
  | LigoTypeFullUnresolved Value -- TODO
  deriving stock (Generic, Show)

-- | A pair in "type_content" array `[name, content]`.
-- ```
-- { "type_content": LigoTypeContent }
-- ```
data LigoTypeContent = LigoTypeContent
  { _ltcName :: Text
  , _ltcContentInner :: LigoTypeContentInner
  }
  deriving stock (Generic, Show)

-- | Inner object representing type content that depends on `name` in `LigoTypeContent`.
-- ```
-- { "type_content": [ <type>, LigoTypeContentInner ] }
-- ```
data LigoTypeContentInner
  = -- | Type call represented by the list of arguments and its constructor.
    -- `"t_constant"`
    LTCConstant
      { _ltciParameters :: [LigoTypeParameter]
      , _ltciLanguage :: Text
      , _ltciInjection :: Text
      }
  | -- | `"t_variable"`
    LTCVariable
      { _ltcvName :: Text
      }
  | -- | `"t_record"`
    LTCRecord (HM.HashMap Text LigoRecordField)
  | -- | `"t_app"`
    LTCApp
      { _ltciTypeOperator :: Text
      , _ltciArguments :: [LigoTypeFull]
      }
  | -- | `"t_arrow"`, note that the order of its arguments is reversed.
    LTCArrow -- "type2" -> "type1"
      { _ltciType2 :: LigoTypeFull
      , _ltciType1 :: LigoTypeFull
      }
  deriving stock (Generic, Show)

-- | Record field type value.
-- ```
-- { "type_content": ["T_record", { "key": LigoRecordField } ] }
-- ```
data LigoRecordField = LigoRecordField
  { -- | Declaration position (don't ask me I too don't know what actual
    -- position is this since from all the example it's somewhat always 0).
    _lrfDeclPos :: Int
    -- | How the value is represented in michelson, currently ignored
    -- during parsing.
    -- _lrfLocation :: LigoRange
  , -- | The type itself.
    _lrfAssociatedType :: LigoTypeFull
  }
  deriving stock (Generic, Show)

-- | Location of definition.
-- ```
-- { "location": LigoRange }
-- ```
data LigoRange
  = Virtual Text
  | LigoRange
      { _lrStart :: LigoRangeInner
      , _lrStop :: LigoRangeInner
      }
  deriving stock (Eq, Generic, Show)

-- | Insides of ligo location.
-- ```
-- { ["start" | "stop"]: LigoRangeInner }
-- ```
data LigoRangeInner = LigoRangeInner
  { _lriByte :: LigoByte
  , _lriPointNum :: Int
  , _lriPointBol :: Int
  }
  deriving stock (Eq, Generic, Show)

-- | Byte representation of ligo location.
-- ```
-- { "byte": LigoByte }
-- ```
data LigoByte = LigoByte
  { _lbPosFname :: FilePath
  , _lbPosLnum :: Int
  , _lbPosBol :: Int
  , _lbPosCnum :: Int
  }
  deriving stock (Eq, Generic, Show)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

-- TODO: some ToJSON instances are not opposed to `FromJSON` ones meaning
-- that some `FromJSON (ToJSON a)` instances are not isomorphic to `Identity`

instance FromJSON LigoError where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance ToJSON LigoError where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance FromJSON LigoErrorContent where
  parseJSON = withObject "error_content" $ \o -> do
    _lecMessage <- o .: "message"
    _lecLocation <- parseLigoRange "location_error_inner_range" =<< o .: "location"
    return LigoErrorContent {..}

instance ToJSON LigoErrorContent where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoDefinitions where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance ToJSON LigoDefinitions where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance FromJSON LigoDefinitionsInner where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance ToJSON LigoDefinitionsInner where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoScope where
  parseJSON = withObject "scope" $ \o -> do
    _lsRange <- parseLigoRange "scope_range" =<< o .: "range"
    _lsTypeEnvironment <- o .: "type_environment"
    _lsExpressionEnvironment <- o .: "expression_environment"
    return $ LigoScope {..}

-- TODO: malformed
instance ToJSON LigoScope where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance FromJSON LigoDefinitionScope where
  parseJSON = withObject "scope" $ \o -> do
    _ldsName <- o .: "name"
    _ldsRange <- parseLigoRange "scope_range" =<< o .: "range"
    _ldsBodyRange <- parseLigoRange "scope_body_range" =<< o .: "body_range"
    _ldsT <- o .:? "t"
    _ldsReferences <- traverse (parseLigoRangeArray "scope_references_ranges") =<< o .: "references"
    return $ LigoDefinitionScope {..}

instance ToJSON LigoDefinitionScope where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

-- We trust ligo compiler output for printing even number
-- of array elements.
instance FromJSON LigoTypeFull where
  parseJSON = withObject "type_full" $ \o ->
    asum
      [ o .: "core" >>= parseAsCoreType
      , o .: "resolved" >>= parseAsResolvedType
      , o .: "unresolved" >>= parseAsUnresolvedType
      ]
    where
      parseAsCoreType :: Value -> Parser LigoTypeFull
      parseAsCoreType = do
        withObject "type_full_core" $ \o' -> do
          _ltfcLocation <- parseLigoRange "ligo_type_core_range" =<< o' .: "location"
          type_content <- o' .: "type_content"
          _ltfcTypeContent <-
            withArray "type_content" (mapM parseLigoTypeContent . group 2 . toList) type_content
          return $ LigoTypeFullCore {..}

      parseAsResolvedType :: Value -> Parser LigoTypeFull
      parseAsResolvedType = do
        withObject "type_full_resolved" $ \o' -> do
          _ltfrLocation <- parseLigoRange "ligo_type_core_range" =<< o' .: "location"
          type_content <- o' .: "type_content"
          _ltfrTypeContent <-
            withArray "type_content" (mapM parseLigoTypeContent . group 2 . toList) type_content
          _ltfrTypeMeta <- o' .: "type_meta"
          _ltfrOrigVar <- o' .: "orig_var" >>= parseOrigVar
          return $ LigoTypeFullResolved {..}

      -- TODO: For now we don't know what 'Value's may go into this type,
      -- but we assume it is 'Null' for now.
      parseAsUnresolvedType :: Value -> Parser LigoTypeFull
      parseAsUnresolvedType = pure . LigoTypeFullUnresolved

instance ToJSON LigoTypeFull where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance FromJSON LigoTypeContent where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance ToJSON LigoTypeContent where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoTypeParameter where
  parseJSON = withObject "type_parameter" $ \o -> do
    type_content <- o .: "type_content"
    _ltpTypeContent <-
      withArray "type_content" (mapM parseLigoTypeContent . group 2 . toList) type_content
    _ltpTypeMeta <- o .: "type_meta"
    _ltpLocation <- parseLigoRange "type_parameter_range" =<< o .: "location"
    _ltpOrigVar <- o .: "orig_var" >>= parseOrigVar
    return LigoTypeParameter {..}

instance ToJSON LigoTypeParameter where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoTypeContentInner where
  parseJSON = withObject "type_content" $ \o -> do
    asum
      [
        do -- parse tuple
          content <- o .: "content"
          parsed <- parseJSON content
          pure $ LTCRecord parsed
      , do -- parse record
          parsed <- sequence $ parseJSON <$> o
          pure $ LTCRecord parsed
      , do
          _ltciParameters <- o .: "parameters"
          _ltciLanguage <- o .: "language"
          _ltciInjection <- o .: "injection"
          pure $ LTCConstant {..}
      , do
          _ltciType1 <- o .: "type1" >>= parseTypeInner
          _ltciType2 <- o .: "type2" >>= parseTypeInner
          pure $ LTCArrow {..}
      , do
          _ltciTypeOperator <- o .: "type_operator" >>= \o' -> o' .: "name"
          (arguments :: Value) <- o .: "arguments"
          _ltciArguments <-
            toList <$> withArray "l_type_arguments" (mapM parseTypeInner) arguments
          return LTCApp {..}
      , LTCVariable <$> o .: "name"
      ]

instance ToJSON LigoTypeContentInner where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoRecordField where
  parseJSON = withObject "record_field" $ \o -> do
    _lrfDeclPos <- o .: "decl_pos"
    _lrfAssociatedType <- o .: "associated_type" >>= parseTypeInner
    return $ LigoRecordField {..}

instance ToJSON LigoRecordField where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoRange where
  parseJSON = liftA2 (<|>) parseAsString parseAsObject
    where
      parseAsString (String o) = return $ Virtual o
      parseAsString _ = fail "failed to parse as string"
      parseAsObject = withObject "range" $ \o -> do
        _lrStart <- o .: "start"
        _lrStop <- o .: "stop"
        return $ LigoRange {..}

instance ToJSON LigoRange where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance FromJSON LigoRangeInner where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance ToJSON LigoRangeInner where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance FromJSON LigoByte where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 2}

instance ToJSON LigoByte where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 2}

-- Workarounds

parseOrigVar :: [Value] -> Parser (Maybe Text)
parseOrigVar = \case
  [_, Object (HM.toList -> [("name", name)])] -> do
    name' <- parseJSON @Text name
    return $ Just name'
  _ -> return Nothing

_parseTypeMeta :: [Value] -> Parser (Maybe LigoTypeFull)
_parseTypeMeta = \case
  [_, o] -> do
    tm <- parseTypeInner o
    return $ Just tm
  _ -> return Nothing

-- A workaround since function types are not separated to "core" | "resolved" | "unresolved" and lets consider these as "core" ones
parseTypeInner :: Value -> Parser LigoTypeFull
parseTypeInner = withObject "type_inner" $ \o -> do
  _ltfcLocation <- parseLigoRange "ligo_type_core_range" =<< o .: "location"
  type_content <- o .: "type_content"
  _ltfcTypeContent <-
    withArray "type_content" (mapM parseLigoTypeContent . group 2 . toList) type_content
  return $ LigoTypeFullCore {..}

-- | Construct a parser for ligo ranges that are represented in pairs
-- ```
-- [ "name", <LigoRange> ]
-- ```
parseLigoRange :: String -> Value -> Parser LigoRange
parseLigoRange str val = parseLigoRangeArray str val <|> parseLigoRangeString str val

parseLigoRangeArray :: String -> Value -> Parser LigoRange
parseLigoRangeArray = flip withArray (safeExtract . group 2 . toList)
  where
    safeExtract :: [[Value]] -> Parser LigoRange
    safeExtract ([_, value]:_) = parseJSON @LigoRange value
    safeExtract _ = error "number of range elements in array is not even and cannot be grouped"

-- LIGO sometimes returns the ranges as a human-readable string (wtf), so parse
-- this string for relevant points.
parseLigoRangeString :: String -> Value -> Parser LigoRange
parseLigoRangeString = flip withText safeExtract
  where
    safeExtract :: Text -> Parser LigoRange
    safeExtract str = case parseRange $ matchRange $ Text.unpack str of
      Left err -> fail err
      Right (file, line, colStart, colStop) -> pure LigoRange
        { _lrStart = LigoRangeInner
          { _lriByte = LigoByte
            { _lbPosFname = file
            , _lbPosLnum = line
            , _lbPosBol = 0  -- not used in mbFromLigoRange
            , _lbPosCnum = 0  -- not used in mbFromLigoRange
            }
          , _lriPointNum = colStart
          , _lriPointBol = 0  -- need abs (_lriPointNum - _lriPointBol) == _lriPointNum
          }
        , _lrStop = LigoRangeInner
          { _lriByte = LigoByte
            { _lbPosFname = file
            , _lbPosLnum = line
            , _lbPosBol = 0  -- not used in mbFromLigoRange
            , _lbPosCnum = 0  -- not used in mbFromLigoRange
            }
          , _lriPointNum = colStop
          , _lriPointBol = 0  -- need abs (_lriPointNum - _lriPointBol) == _lriPointNum
          }
        }

    parseRange :: [String] -> Either String (FilePath, Int, Int, Int)
    parseRange = \case
        [_, file, line, colStart, colStop] -> do
          let fp = file
          l <- readEither line
          cStart <- readEither colStart
          cStop <- readEither colStop
          pure (fp, l, cStart, cStop)
        matches -> Left $ "Could not match range with expected format: " <> show matches

    matchRange :: String -> [String]
    matchRange str =
      getAllTextSubmatches (str =~ ("\"(.*)\", line ([0-9]+), characters ([0-9]+)-([0-9]+)" :: Text))

-- | Construct a parser of ligo type content that is represented in pairs
-- ```
-- [ "name", <LigoTypeContent> ]
-- ```
parseLigoTypeContent :: [Value] -> Parser LigoTypeContent
parseLigoTypeContent [name, value] = do
  _ltcName <- parseJSON @Text name
  _ltcContentInner <- parseJSON @LigoTypeContentInner value
  return $ LigoTypeContent {..}
parseLigoTypeContent _ = error "number of type content elements is not even and cannot be grouped"

----------------------------------------------------------------------------
-- Pretty
----------------------------------------------------------------------------

instance Pretty LigoError where
  pp (LigoError _ stage (LigoErrorContent msg at)) = mconcat
    [ text "Error in ", text $ show stage
    , text "\n\nat: ", pp $ fromLigoRangeOrDef at
    , text "\n\n" <> pp msg
    ]
    where
      _fromLigoRange r@(LigoRange _ _) =
        "[" <> pp (fromMaybe (error "impossible") (mbFromLigoRange r)) <> "]"
      _fromLigoRange (Virtual _) = text "virtual"

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Convert ligo error to its corresponding internal representation.
fromLigoErrorToMsg :: LigoError -> Msg
fromLigoErrorToMsg LigoError
  { _leContent = LigoErrorContent
      { _lecMessage = err
      , _lecLocation = fromLigoRangeOrDef -> at
      }
  } = (at, Error (err :: Text) [])

-- | Helper function that converts qualified field to its JSON counterpart.
--
-- >>> prepareField 2 "__llFooBar"
-- "foo_bar"
prepareField :: Int -> String -> String
prepareField dropAmount = Prelude.drop (dropAmount + 2) . concatMap process
  where
    process c
      | isUpper c = "_" <> [toLower c]
      | otherwise = [c]

-- | Splits an array onto chunks of n elements, throws error otherwise.
--
-- >>> group 2 [1, 2, 3, 4]
-- [[1,2],[3,4]]
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"

-- | Converts ligo ranges to our internal ones.
-- Note: ligo team allows for start file of a range be different from end file.
-- Either if this intentional or not we throw an error if they are so.
-- >>> :{
-- mbFromLigoRange
--   (LigoRange
--     (LigoRangeInner (LigoByte "contracts/test.ligo" 2 undefined undefined) 3 6)
--     (LigoRangeInner (LigoByte "contracts/test.ligo" 5 undefined undefined) 11 12)
--   )
-- :}
-- contracts/test.ligo:2:4-5:2
mbFromLigoRange :: LigoRange -> Maybe Range
mbFromLigoRange (Virtual _) = Nothing
mbFromLigoRange
  (LigoRange
    (LigoRangeInner LigoByte { _lbPosLnum = startLine , _lbPosFname = startFilePath } startCNum startBol)
    (LigoRangeInner LigoByte { _lbPosLnum = endLine   , _lbPosFname = endFilePath   } endCNum   endBol)
  )
  | startFilePath /= endFilePath = error "start file of a range does not equal to its end file"
  | otherwise = Just Range
      { _rStart = (startLine, abs (startCNum - startBol) + 1, 0)
      , _rFinish = (endLine, abs (endCNum - endBol) + 1, 0)
      , _rFile = startFilePath
      }

fromLigoRangeOrDef :: LigoRange -> Range
fromLigoRangeOrDef = fromMaybe (point (-1) (-1)) . mbFromLigoRange

-- | Reconstruct `LIGO` tree out of `LigoTypeFull`.
fromLigoTypeFull :: LigoTypeFull -> LIGO Info
fromLigoTypeFull = enclose . \case
  LigoTypeFullCore
    { _ltfcTypeContent = [tc]
    , _ltfcLocation
    } -> do
        modify . putElem . fromLigoRangeOrDef $ _ltfcLocation
        fromLigoType tc

  LigoTypeFullResolved
    { _ltfrTypeContent = [tc]
    , _ltfrLocation
    } -> do
        modify . putElem . fromLigoRangeOrDef $ _ltfrLocation
        fromLigoType tc

  LigoTypeFullCore {} ->
    mkErr "malformed core type given (the array is not of length of 2)"

  LigoTypeFullResolved {} ->
    mkErr "malformed resolved type given (the array is not of length of 2)"

  LigoTypeFullUnresolved _ ->
    mkErr "unresolved type given"

  where

    fromLigoPrimitive :: Text -> State (Product Info) (LIGO Info)
    fromLigoPrimitive p = do
      st <- get
      return $ make' (st, TypeName p)

    -- TODO: parse type meta
    fromLigoParameter
      LigoTypeParameter
        { _ltpTypeContent = [t]
        , _ltpLocation
        } = do
          modify . putElem . fromLigoRangeOrDef $ _ltpLocation
          fromLigoType t
    fromLigoParameter
      LigoTypeParameter
        { _ltpLocation
        } = do
          modify . putElem . fromLigoRangeOrDef $ _ltpLocation
          mkErr "malformed type parameter"

    fromLigoConstant name [] = fromLigoPrimitive name
    fromLigoConstant name params = do
      st <- get
      n <- fromLigoPrimitive name
      p <- sequence $ fromLigoParameter <$> params
      return $ make' (st, TApply n p)

    fromLigoType
      :: LigoTypeContent
      -> State (Product Info) (LIGO Info)
    fromLigoType LigoTypeContent {..} = case _ltcContentInner of
      LTCConstant
        { _ltciParameters
        , _ltciInjection
        } -> fromLigoConstant _ltciInjection _ltciParameters

      LTCVariable name ->
        fromLigoPrimitive name

      LTCRecord record -> do
        st <- get
        record' <-
            record
          & HM.mapWithKey fromLigoRecordField
          & toList
          & sequence
        return $ make' (st, TRecord record')

      LTCApp {..} -> do
        st <- get
        p <- fromLigoPrimitive _ltciTypeOperator
        return . make' . (st,) $
          TApply p (fromLigoTypeFull <$> _ltciArguments)

      LTCArrow {..} -> do
        st <- get
        let mkArrow = TArrow `on` fromLigoTypeFull
        return $ make' (st, mkArrow _ltciType2 _ltciType1)

    fromLigoRecordField
      :: Text
      -> LigoRecordField
      -> State (Product Info) (LIGO Info)
    fromLigoRecordField name LigoRecordField {..} = do
      st <- get
      n <- fromLigoPrimitive name
      return $ make' (st, TField n (fromLigoTypeFull _lrfAssociatedType))

    mkErr = gets . flip mkLigoError

    enclose
      :: State (Product Info) (LIGO Info)
      -> LIGO Info
    enclose = flip evalState defaultState

    defaultState :: Product Info
    defaultState = [] :> [] :> point 1 1 :> N :> CodeSource "" :> Nil

mkLigoError :: Product Info -> Text -> LIGO Info
mkLigoError p msg = make' . (p,) $ Error msg [p :< inject (Name "ligo error")]

-- | Variant of `make` that constructs a tree out of annotation and node
-- that recovers range from previous subnodes by merging them, this helps to
-- reconstruct `["Virtual", "generated"]` types out of their subnodes which
-- by some onorthodox opportunity may have proper ranges.
make'
  :: forall fs f .
     ( Element f fs
     , Foldable f
     , Apply Functor fs
     ) => (Product Info, f (Tree fs (Product Info)))
       -> Tree fs (Product Info)
make' (i, f)
  | null ges = i :< inject f
  | otherwise = i' :< inject f
  where
    ges = List.filter (not . (`leq` i)) (extract <$> toList f)
    r = getElem (List.minimum ges) `merged` getElem (List.maximum ges)
    i' = putElem r i

-- | Converts ligo scope to our internal representation.
toScopedDecl :: LigoDefinitionScope -> ScopedDecl
toScopedDecl
  LigoDefinitionScope
    { _ldsName = _sdName
    , _ldsRange = (fromMaybe (error "no origin range") . mbFromLigoRange -> _sdOrigin)
    , _ldsBodyRange = (mbFromLigoRange -> _vdsInitRange)
    , _ldsT
    } =
    ScopedDecl -- TODO fill in full information when we actually use ligo scopes
      { _sdName
      , _sdOrigin
      , _sdRefs = []
      , _sdDoc = []
      , _sdDialect = Pascal -- TODO: we have no information regarding dealect in scope output
      , _sdSpec = ValueSpec $ ValueDeclSpecifics
        { _vdsInitRange
        , _vdsParams = Nothing
        , _vdsTspec = parseTypeDeclSpecifics . fromLigoTypeFull <$> _ldsT
        }
      }
