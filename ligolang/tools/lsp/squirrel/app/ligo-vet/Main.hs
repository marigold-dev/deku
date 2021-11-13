{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Arrow (first)
import Control.Monad (unless)
import Data.Foldable (for_)
import Duplo.Pretty (Pretty, pp, render)
import Main.Utf8 (withUtf8)
import Options.Applicative
  (Parser, ParserInfo, command, execParser, help, helper, hsubparser, info, long, metavar, progDesc,
  short, strOption, switch)

import AST (Fallback, FindFilepath, Msg, ParsedContract (..), _getContract, parsePreprocessed, parseWithScopes)
import ParseTree (Source (Path))

newtype Command = PrintSexp PrintSexpOptions

data PrintSexpOptions = PrintSexpOptions
  { psoContract :: FilePath
  , psoWithScopes :: Bool
  }

commandParser :: Parser Command
commandParser = hsubparser $ mconcat
  [ printSubCommand ]
  where
    printSubCommand = command "print-sexp"
      (info printSexp
        (progDesc "Parse a given contract and print a s-exp representing it."))

printSexp :: Parser Command
printSexp = do
  psoContract <- contractFileOption
  psoWithScopes <- withScopesOption
  pure (PrintSexp PrintSexpOptions{ .. })
  where
    withScopesOption = switch $
      long "with-scopes" <>
      short 's' <>
      help "Whether to add fallback scopes to the tree"

-- | Parser of a path to a contract.
contractFileOption :: Parser FilePath
contractFileOption = strOption $
  long "contract" <>
  metavar "FILEPATH" <>
  help "Path to contract file"

programInfo :: ParserInfo Command
programInfo = info (helper <*> commandParser) mempty

data SomePretty where
  SomePretty :: Pretty a => a -> SomePretty

instance Pretty SomePretty where
  pp (SomePretty a) = pp a

main :: IO ()
main = withUtf8 $
  execParser programInfo >>= \case
    PrintSexp PrintSexpOptions{ .. } -> do
      let treeMsgs (ParsedContract _ tree msgs) = (tree, msgs)
      let toPretty :: Pretty info => IO (FindFilepath info) -> IO (SomePretty, [Msg])
          toPretty = fmap (first SomePretty . treeMsgs . _getContract)
      let parser = if psoWithScopes
            then toPretty . parseWithScopes @Fallback
            else toPretty . parsePreprocessed
      (tree, messages) <- parser (Path psoContract)
      putStrLn (render (pp tree))
      unless (null messages) $ do
        putStrLn "The following errors have been encountered: "
        for_ messages $ \(range, err) ->
          putStrLn (render (pp range <> ": " <> pp err))
