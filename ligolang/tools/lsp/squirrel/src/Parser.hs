{-# OPTIONS_GHC -Wno-orphans #-}

module Parser
  ( Msg
  , ParserM
  , LineMarkerType (..)
  , LineMarker (..)
  , Failure (..)
  , ShowRange (..)
  , CodeSource (..)
  , Info
  , ParsedInfo

  , runParserM
  , collectTreeErrors
  , parseLineMarkerText
  , flag
  , field
  , fieldOpt
  , fields
  , emptyParsedInfo
  , fillInfo
  , withComments
  , boilerplate
  , boilerplate'
  , fallthrough
  ) where

import Control.Arrow
import Control.Monad.Catch
import Control.Monad.RWS hiding (Product)
import Data.Functor
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)

import Duplo.Pretty
import Duplo.Tree

import AST.Skeleton (Error (..), SomeLIGO, getLIGO)
import ParseTree
import Product
import Range

{-
  Comment grabber has 2 buffers: 1 and 2.

  1) We collect all comments before the node and append them into buffer 1.
  2) We collect all comments after the node and put them into buffer 2.
  3) `grabComments` takes all comments from buffer 1.
  4) On leaving, move move comments from 2 to 1.
-}

runParserM :: ParserM a -> IO (a, [Msg])
runParserM p = (\(a, _, errs) -> (a, errs)) <$> runRWST p [] ([], [])

type Msg      = (Range, Error ())
type ParserM  = RWST [RawTree] [Msg] ([Text], [Text]) IO

collectTreeErrors :: Contains Range info => SomeLIGO info -> [Msg]
collectTreeErrors = map (getElem *** void) . collect . getLIGO

-- | The flag of some line marker.
--
-- Note that we make the assumption that a flag may only be 1 or 2, since LIGO
-- should not have system header files or be wrapped in `extern "C"` blocks.
data LineMarkerType
  = RootFile      -- ^ No flag.
  | IncludedFile  -- ^ Flag 1.
  | ReturnToFile  -- ^ Flag 2.
  deriving stock (Eq, Show)

-- | A inclusion line marker left by running `ligo preprocess`.
--
-- Note that we assume that we may only have zero or one flag instead of zero or
-- more, since flags 1 and 2 are mutually exclusive. See 'LineMarkerType'.
--
-- See also: https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html
data LineMarker = LineMarker
  { lmFile :: FilePath  -- ^ The file that was included.
  , lmFlag :: LineMarkerType  -- ^ The "parsed" flag of the line marker.
  , lmLine :: Int  -- ^ The line number that should be used after the inclusion.
  , lmLoc  :: Range  -- ^ The location in the preprocessed file where the line marker was added.
  } deriving stock (Eq, Show)

parseLineMarkerText :: Text -> Maybe (FilePath, LineMarkerType, Int)
parseLineMarkerText marker = do
  "#" : lineStr : fileText : flags <- Just $ Text.words marker
  line <- readMaybe $ Text.unpack lineStr
  let file = Text.unpack $ Text.init $ Text.tail fileText
  let markerType = case flags of
        -- TODO: There is an edge case when there are line markers and comments
        -- (see src/test/contracts/includer.{,m,re}ligo, so we assume for now
        -- that any extra fields after "1" or "2" are comments, and anything
        -- else is a root file (possibly with comments).
        "1" : _ -> IncludedFile
        "2" : _ -> ReturnToFile
        _       -> RootFile
  pure (file, markerType, line)

parseLineMarker :: (RawInfo, ParseTree RawTree) -> Maybe LineMarker
parseLineMarker (getRange -> range, ParseTree ty _ marker) = do
  guard (ty == "line_marker")
  (file, markerType, line) <- parseLineMarkerText marker
  pure $ LineMarker file markerType line range

newtype Failure = Failure String
  deriving stock (Show)
  deriving anyclass (Exception)

instance Scoped (Product [Range, Text]) ParserM RawTree ParseTree where
  before (_ :> _ :> _) (ParseTree _ cs _) = do
    let (comms, rest) = allComments cs
    let (comms1, _)   = allComments $ reverse rest
    modify $ first  (++ comms)
    modify $ second (++ reverse comms1)

    tell $ allErrors cs

  after _ _ = do
    modify $ \(_, y) -> (y, [])

grabComments :: ParserM [Text]
grabComments = do
  ls <- gets fst
  modify \(_, y) -> ([], y)
  return ls

allComments :: [RawTree] -> ([Text], [RawTree])
allComments = first (map getBody . filter isComment) . break isMeaningful
  where
    isMeaningful :: RawTree -> Bool
    isMeaningful (extract -> _ :> "" :> _) = False
    isMeaningful  _                        = True

    isComment :: RawTree -> Bool
    isComment (gist -> ParseTree ty _ _) = "comment" `Text.isSuffixOf` ty

allErrors :: [RawTree] -> [(Range, Error ())]
allErrors = mapMaybe extractUnnamedError
  where
    extractUnnamedError :: RawTree -> Maybe (Range, Error ())
    extractUnnamedError tree = case only tree of
      (r :> "" :> _, ParseTree "ERROR" children _)
        -> Just (r, void (Error ("Unexpected: " <> getBody tree) children))
      _ -> Nothing

getBody :: RawTree -> Text
getBody (gist -> f) = ptSource f

flag :: Text -> ParserM Bool
flag name = fieldOpt name <&> isJust

field :: Text -> ParserM RawTree
field name =
  fieldOpt name
    >>= maybe (throwM $ Failure [i|Cannot find field #{name}|]) return

fieldOpt :: Text -> ParserM (Maybe RawTree)
fieldOpt name = go <$> ask
  where
    go (tree@(extract -> _ :> n :> _) : rest)
      | n == name = Just tree
      | otherwise = go rest

    go _ = Nothing

fields :: Text -> ParserM [RawTree]
fields name = go <$> ask
  where
    go (tree : rest)
      | _ :> n :> _ <- extract tree, n == name = tree : go rest
      | errorAtTheTop tree = tree : go rest
      | otherwise = go rest

    go _ = []

    errorAtTheTop :: RawTree -> Bool
    errorAtTheTop (match -> Just (_, ParseTree "ERROR" _ _)) = True
    errorAtTheTop _ = False

data ShowRange
  = Y | N
  deriving stock Eq

instance Pretty ShowRange where
  pp Y = "Yau"
  pp N = "Nah"

newtype CodeSource = CodeSource { unCodeSource :: Text }
  deriving newtype (Eq, Ord, Show, Pretty)

type Info = [[Text], [LineMarker], Range, ShowRange, CodeSource]

type ParsedInfo = PreprocessedRange ': Info

emptyParsedInfo :: Product ParsedInfo
emptyParsedInfo =
  PreprocessedRange emptyPoint :> [] :> [] :> emptyPoint :> N :> CodeSource "" :> Nil
  where
    emptyPoint = point (-1) (-1)

instance
  ( Contains Range xs
  , Contains [Text] xs
  , Contains ShowRange xs
  )
  => Modifies (Product xs)
  where
    ascribe xs
      = ascribeRange (getElem @Range xs) (getElem xs)
      . ascribeComms (getElem xs)

fillInfo :: Functor f => f (Product xs) -> f (Product ([Text] : Range : ShowRange : xs))
fillInfo = fmap \it -> [] :> point (-1) (-1) :> N :> it

ascribeComms :: [Text] -> Doc -> Doc
ascribeComms comms
  | null comms = id
  | otherwise  = \d ->
      block $ map pp comms ++ [d]

ascribeRange :: Pretty p => p -> ShowRange -> Doc -> Doc
ascribeRange r Y = (pp r $$)
ascribeRange _ _ = id

withComments :: ParserM (Product xs, a) -> ParserM (Product ([Text] : xs), a)
withComments act = do
  comms <- grabComments
  first (comms :>) <$> act

getMarkers :: [RawTree] -> [LineMarker]
getMarkers = mapMaybe (parseLineMarker . fromJust . match)

boilerplate
  :: (Text -> ParserM (f RawTree))
  -> (RawInfo, ParseTree RawTree)
  -> ParserM (Product Info, f RawTree)
boilerplate f (r :> _, ParseTree ty cs src) =
  withComments do
    -- TODO: What is exactly the appropriate action in case something ever
    -- returns 'Nothing'? 'catMaybes'? If something goes wrong, then we will
    -- probably get unwanted behavior in 'AST.Parser'.
    let markers = getMarkers cs
    f' <- local (const cs) $ f ty
    return (markers :> r :> N :> CodeSource src :> Nil, f')

boilerplate'
  :: ((Text, Text) -> ParserM (f RawTree))
  -> (RawInfo, ParseTree RawTree)
  -> ParserM (Product Info, f RawTree)
boilerplate' f (r :> _, ParseTree ty cs src) =
  withComments do
    let markers = getMarkers cs
    f' <- local (const cs) $ f (ty, src)
    return (markers :> r :> N :> CodeSource src :> Nil, f')

fallthrough :: MonadThrow m => m a
fallthrough = throwM HandlerFailed
