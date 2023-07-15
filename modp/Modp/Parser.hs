{-
# Parser

## Principle

The modular parser is an extensible PEG parser interpreted by a Packrat parser.
The main difference compared to Lean's parser (which we take heavy inspiration
from) is the lack of a longest-match rule. There is a longest match rule at the
lexer level but not in the parser, meaning that ambiguous inputs at the grammar
level are resolved arbitrarily. Custom rules must avoid this explicitly.

## Packrat parsing

A packrat parser is basically a backtracking parser with memoization. The
input to the memoization table is the pair (remaining text, rule ID).
-}

module Modp.Parser where
import qualified Data.Map as M
import Control.Monad.State.Strict

-- Parsing Expression with tokens identified by values of type t and non-
-- terminals identified by values of type s.
data PE t {- tokenID -} s {- symbolID -} =
  PETerminal t |
  PETerminalPredicated t (String -> Bool) |
  PENonTerminal s |
  PESequence (PE t s) (PE t s) |
  PESequenceNode s [PE t s] |
  PEAlternative (PE t s) (PE t s) |
  PEStar (PE t s) |
  PEPlus (PE t s) |
  PEOptional (PE t s) |
  PEAnd (PE t s) |
  PENot (PE t s)

instance Show (PE t s) where
  show p = "<a parser expression>"

-- Parser Expression Grammar: non-terminal => production rules.
-- The name of the non-terminal also serves as a rule identifier. The list of
-- rules is implicitly an ordered alternative.
newtype PEG t s = PEG (M.Map s [PE t s])

emptyPEG :: PEG t s
emptyPEG = PEG M.empty

getPEGRule :: (Ord s) => PEG t s -> s -> Maybe (PE t s)
getPEGRule (PEG m) sym = do
  rules <- M.lookup sym m
  case rules of
    [] -> Nothing
    _ -> Just $ foldr1 PEAlternative rules

-- Packrat parsing

-- TODO: Major bottlenecks:
-- 1. Using [t] instead of a position within the list (and possibly an array)
-- 2. Same in Result
-- TODO:
-- * Replace (t, String) with `Token t` once it's parameterized in Lexer

data ParseTree t {- tokenID -} s {- symbolID -} =
  ParseTreeAtom (t, String) |
  ParseTreeSequence [ParseTree t s] |
  ParseTreeNode s [ParseTree t s]

showParseTreeGen :: (t -> String) -> (s -> String) -> Int ->
                    ParseTree t s -> String
showParseTreeGen showT showS indent tree =
    take (2*indent) (repeat ' ') ++ aux tree
  where aux (ParseTreeAtom (t, str)) =
          "#" ++ showT t ++ " \"" ++ str ++ "\"\n"
        aux (ParseTreeSequence trees) =
          let subs = map (showParseTreeGen showT showS (indent+1)) trees in
          "[]\n" ++ concat subs
        aux (ParseTreeNode s trees) =
          let subs = map (showParseTreeGen showT showS (indent+1)) trees in
          showS s ++ "\n" ++ concat subs

showParseTree :: (Show t, Show s) => Int -> ParseTree t s -> String
showParseTree = showParseTreeGen show show

instance (Show t, Show s) => Show (ParseTree t s) where
  show = showParseTree 0

type Input t = [(t, String)]
type Result t s = Maybe (ParseTree t s, Input t)
type Cache t s = M.Map (Input t, s) (Result t s)

data ParserState t s = ParserState {
  -- | Full grammar used for parsing input
  psGrammar :: PEG t s,
  -- | Packrat parser cache
  psCache :: Cache t s
}

type ParserM t s = State (ParserState t s)

queryCache :: (Ord t, Ord s) => Input t -> s -> ParserM t s (Maybe (Result t s))
queryCache tokens sym = do
  ps <- get
  return $ M.lookup (tokens, sym) (psCache ps)

updateCache :: (Ord t, Ord s) => Input t -> s -> Result t s -> ParserM t s ()
updateCache tokens sym r = do
  modify $ \ps -> ps { psCache = M.insert (tokens, sym) r (psCache ps) }
  return ()

parse :: (Ord t, Ord s) => Input t -> s -> ParserM t s (Result t s)
parse tokens sym = do
  mr <- queryCache tokens sym
  case mr of
    Just r -> return r
    Nothing -> do
      r <- parseGo tokens sym
      updateCache tokens sym r
      return r

parseGo :: (Ord t, Ord s) => Input t -> s -> ParserM t s (Result t s)
parseGo tokens sym = do
  ps <- get
  case getPEGRule (psGrammar ps) sym of
    Nothing -> return Nothing
    Just peg ->
      parsePE tokens peg

parsePE :: (Ord t, Ord s) => Input t -> PE t s -> ParserM t s (Result t s)

parsePE tokens (PETerminal tID) =
  case tokens of
    [] -> return Nothing
    (tok@(tID', value)):tokens ->
      if tID' == tID then
        return $ Just (ParseTreeAtom tok, tokens)
      else
        return Nothing

parsePE tokens (PETerminalPredicated tID predicate) =
  case tokens of
    [] -> return Nothing
    (tok@(tID', value)):tokens ->
      if tID' == tID && predicate value then
        return $ Just (ParseTreeAtom tok, tokens)
      else
        return Nothing

parsePE tokens (PENonTerminal sym) =
  parse tokens sym

parsePE tokens (PESequence e f) = do
  r1 <- parsePE tokens e
  case r1 of
    Nothing -> return Nothing
    Just (tree, tokens') -> do
      r2 <- parsePE tokens' f
      case r2 of
        Nothing -> return Nothing
        Just (tree', tokens'') ->
          return $ Just (ParseTreeSequence [tree, tree'], tokens'')

parsePE tokens (PESequenceNode sym es) = do
  rs <- parsePEs tokens es
  case rs of
    Nothing -> return Nothing
    Just (trees, tokens') ->
      return $ Just (ParseTreeNode sym trees, tokens')

parsePE tokens (PEAlternative e f) = do
  r1 <- parsePE tokens e
  case r1 of
    Nothing -> parsePE tokens f
    Just _ -> return r1

parsePE tokens (PEStar e) = do
  (trees, tokens') <- parsePERepeatedly tokens e
  return $ Just (ParseTreeSequence trees, tokens')

parsePE tokens (PEPlus e) = do
  (trees, tokens') <- parsePERepeatedly tokens e
  case trees of
    [] -> return Nothing
    _ -> return $ Just (ParseTreeSequence trees, tokens')

parsePE tokens (PEOptional e) = do
  r <- parsePE tokens e
  case r of
    Nothing -> return $ Just (ParseTreeSequence [], tokens)
    _ -> return r

parsePE tokens (PEAnd e) = do
  r <- parsePE tokens e
  case r of
    Nothing -> return Nothing
    _ -> return $ Just (ParseTreeSequence [], tokens)

parsePE tokens (PENot e) = do
  r <- parsePE tokens e
  case r of
    Nothing -> return $ Just (ParseTreeSequence [], tokens)
    _ -> return Nothing

parsePERepeatedly :: (Ord t, Ord s) =>
  Input t -> PE t s -> ParserM t s ([ParseTree t s], Input t)
parsePERepeatedly tokens e = do
  r <- parsePE tokens e
  case r of
    Nothing -> return ([], tokens)
    Just (tree, tokens') -> do
      (trees, tokens'') <- parsePERepeatedly tokens' e
      return (tree : trees, tokens'')

parsePEs :: (Ord t, Ord s) =>
  Input t -> [PE t s] -> ParserM t s (Maybe ([ParseTree t s], Input t))
parsePEs tokens [] =
  return $ Just ([], tokens)
parsePEs tokens (e:es) = do
  r <- parsePE tokens e
  case r of
    Nothing -> return Nothing
    Just (tree, tokens') -> do
      rs <- parsePEs tokens' es
      case rs of
        Nothing -> return Nothing
        Just (trees, tokens'') ->
          return $ Just (tree:trees, tokens'')
