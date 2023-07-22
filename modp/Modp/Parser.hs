{-
# Parser

## Principle

The modular parser is an extensible PEG parser interpreted by a Packrat parser.
The main difference compared to Lean's parser (which we take heavy inspiration
from) is the lack of a longest-match rule. There is a longest match rule at the
lexer level but not in the parser, meaning that ambiguous inputs at the grammar
level are resolved arbitrarily. Custom rules must avoid this explicitly.

The parsing algorithm is Packrat parsing, which is basically a backtracking
parser with memoization. Compared to other methods such as LR, predictive or
combinator parsers, this method offers pretty friendly grammar expressions
(because there is no need to predict; we can backtrack) and linear-time
performance, but it uses a cache that can grow up to Θ(n*m) space, where n is
the size of the parsed input and m is the number of nonterminals.

## Parser Expressions

The grammar is defined with Parser Expressions (PEs).
TODO: Describe Parser Expressions

## Precedence

It is common for expression-like structures to have operators with varying
precedence and associativity. We support dividing a symbol into _precedence
levels_ with the intent that higher levels correspond to tighter-binding
recursive rules for the symbol. For example, arithmetic expressions might use
the following precedence levels:

  0: expr + expr, expr - expr                 (left associative)
  1: expr * expr, expr / expr, expr % expr    (left associative)
  2: expr ^ expr                              (right associative)
  atoms: integers, ( expr )

Rules for this hierarchy (with left recursion) are written in the following
way, with each associativity corresponding to a different pattern of increasing
precedence level in recursive occurrences.

  expr@0 ::= expr@1 "^" expr@0
  expr@1 ::= expr@1 "*" expr@2 | expr@1 "/" expr@2 | expr@1 "%" expr@2
  expr@2 ::= expr@2 "+" expr@3 | expr@2 "-" expr@3
  expr@3 ::= %integer | "(" expr@0 ")"

This approach is similar to having new symbols "expr_0", "expr_1", each
included in its predecessor, except that:

1. Each level is automatically included in lower levels: %integer would also
   parse as an expr@1.
2. These inclusion rules are not materialized in parse trees, making them
   easier to traverse and less dependent on the number of levels.
3. It is easier to extend because levels can be left unused (eg. use levels 5,
   10, 15) while still being referenced (eg. level 11 in the left-associative
   rule expr@10 ::= expr@10 "+" expr@11), leaving room for programmatic
   expansion.

## Left recursion

Parser Expressions do not support direct or indirect left recursion natively.
Like some Packrat parsers, we rewrite it out automatically in limited cases;
there are more thorough approach such as the one used by OMeta [1].

We support "direct" left recursion where a symbol directly refers to itself.

  sym ::= sym %integer "!"

We do not support indirect left recursion where loops go through multiple
symbols:

  /* error */
  sym ::= numbered_sym "!"
  numbered_sym ::= sym %integer

Direct left recursion is rewritten out by the following process. For any symbol
with base (non-left-recursive) rules b1, ... bn and left-recursive rules
r1, ..., rm:

  sym ::= b1 | ... | bn | sym r1 | ... | sym rm

The equivalent of the following single non-left-recursive rule is used:

  sym ::= (b1 | ... | bn) (r1 | ... | rm)*

with the difference that the parse tree uses properly named nodes whereas the
single rule above would have anonymous nodes.

[1] A. Warth, J.R. Douglass, T. Millstein.
    "Packrat Parsers Can Support Left Recursion."
    https://tinlizzie.org/VPRIPapers/tr2007002_packrat.pdf
-}

module Modp.Parser where
import qualified Data.Map as M
import Control.Monad.State.Strict
import Control.Applicative

type PrecedenceLevel = Int

-- Parser Expression.
--   t: token ID type
--   s: symbol (non-terminal) ID type
-- This is the user-facing language for defining parsers; it is intentionally
-- rich/redundant for ease of use.
data PE t s =
  -- | Any token of the specified type, regardless of its value.
  PETerminal t |
  -- | A token of the specified type, if its value matches the predicate.
  -- | TODO: Make it (Token t -> Bool) so extra data can be matched.
  PETerminalPredicated t (String -> Bool) |
  -- | A literal token, with the _literal token type_ specified by the grammar
  -- | and the exact provided string value.
  PELiteral String |
  -- | A substring matching the provided grammar non-terminal symbol.
  PENonTerminal s | -- PrecedenceLevel |
  -- | An ordered sequence of substrings.
  PESequence [PE t s] |
  -- | Like PESequence, but with a node name. Typically used for a top-level
  -- | parser expression defining a symbol.
  PESequenceNode s [PE t s] |
  -- | Ordered alternative.
  PEAlternative [PE t s] |
  -- | A sequence of 0 or more matches of a sub-expression.
  PEStar (PE t s) |
  -- | A sequence of 1 or more matches of a sub-expression.
  PEPlus (PE t s) |
  -- | An optional match of a sub-expression (sequence of 0 or 1 matches).
  PEOptional (PE t s) |
  -- | Positive look-ahead: matches if the sub-expression matches, but does not
  -- | consume any input and returns a unit node.
  -- | TODO: Bind the other expression so we can remove the unit node from the
  -- |       parse tree altogether.
  PEAnd (PE t s) |
  -- | Negative look-ahead: matches if the sub-expression _does not_ match, and
  -- | does not consume any input.
  PENot (PE t s)

-- Operator precedence in PE notation:
--   0: e | f                   (associativity: full)
--   1: e f, [name] e f ...)    (associativity: full)
--   2: e*, e+, e?              (associativity: none)
--   3: &e, !e                  (associativity: none)
--   atoms: %terminal, nonterminal@prec
showsPrecPE :: (t -> String) -> (s -> String) -> Int -> PE t s -> ShowS
showsPrecPE showT showS = aux
 where
  aux _ (PETerminal t) = showString ("%" ++ showT t)
  aux _ (PETerminalPredicated t p) = showString ("%" ++ showT t ++ "[pred.]")
  aux _ (PELiteral str) = showString (show str)
  aux _ (PENonTerminal s) = showString (showS s)
  aux _ (PESequence []) = showString "<empty sequence>"
  aux p (PESequence es) =
    brackets p 1 "(" ")" $
    foldr1 (\f g -> f . showString " " . g) (map (aux 1) es)
  aux p (PESequenceNode s []) =
    brackets p 1 "(" ")" $
    showString ("#" ++ showS s ++ " <empty sequence>")
  aux p (PESequenceNode s es) =
    brackets p 1 "(" ")" $
    showString ("#" ++ showS s ++ " ") .
    foldr1 (\f g -> f . showString " " . g) (map (aux 1) es)
  aux _ (PEAlternative []) = showString "<empty alternative>"
  aux p (PEAlternative es) =
    brackets p 0 "(" ")" $
    foldr1 (\f g -> f . showString " | " . g) (map (aux 0) es)
  aux p (PEStar     e) = brackets p 2 "(" ")" $ aux 3 e . showString "*"
  aux p (PEPlus     e) = brackets p 2 "(" ")" $ aux 3 e . showString "+"
  aux p (PEOptional e) = brackets p 2 "(" ")" $ aux 3 e . showString "?"
  aux p (PEAnd e) = brackets p 3 "(" ")" $ showString "&" . aux 4 e
  aux p (PENot e) = brackets p 3 "(" ")" $ showString "!" . aux 4 e
  brackets p p_ref left right f =
    if p > p_ref then showString left . f . showString right else f

showPE :: (t -> String) -> (s -> String) -> PE t s -> String
showPE showT showS e = showsPrecPE showT showS 0 e ""

instance (Show t, Show s) => Show (PE t s) where
  showsPrec = showsPrecPE show show

-- Parser Expression Grammar.
data PEG t s = PEG {
  -- | Parsing results associating each symbol with a PE.
  pegRules :: M.Map s (PE t s),
  -- | Token type for literals, used by the shorthand PETerminal expression.
  pegLiteralTokenType :: t
}

pegEmpty :: t -> PEG t s
pegEmpty litType = PEG { pegRules = M.empty, pegLiteralTokenType = litType }

pegGetRule :: (Ord s) => PEG t s -> s -> Maybe (PE t s)
pegGetRule peg sym = M.lookup sym (pegRules peg)

showsPrecPEG :: (t -> String) -> (s -> String) -> Int -> PEG t s -> ShowS
showsPrecPEG showT showS _ peg =
  showString "rules:\n" .
  foldr (.) id (map showRule $ M.toList $ pegRules peg) .
  showString "literal_token_type:\n" .
  showString ("  " ++ showT (pegLiteralTokenType peg) ++ "\n")
  where
  showRule (sym, PEAlternative rules) =
    showString ("  " ++ showS sym ++ " ::=\n") .
    foldr (.) id (map showExpression $ rules)
  showRule (sym, rule) =
    showString ("  " ++ showS sym ++ " ::= ") .
    showsPrecPE showT showS 0 rule .
    showString "\n"
  showExpression e =
    showString "    | " .
    showsPrecPE showT showS 0 e .
    showString "\n"

showPEG :: (t -> String) -> (s -> String) -> PEG t s -> String
showPEG showT showS peg = showsPrecPEG showT showS 0 peg ""

instance (Show t, Show s) => Show (PEG t s) where
  showsPrec = showsPrecPEG show show

-- TODO: Detect and report left recurion

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
          "%" ++ showT t ++ " \"" ++ str ++ "\"\n"
        aux (ParseTreeSequence trees) =
          let subs = map (showParseTreeGen showT showS (indent+1)) trees in
          "[]\n" ++ concat subs
        aux (ParseTreeNode s trees) =
          let subs = map (showParseTreeGen showT showS (indent+1)) trees in
          "#" ++ showS s ++ "\n" ++ concat subs

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
      ps <- get
      r <- case pegGetRule (psGrammar ps) sym of
             Nothing -> return Nothing
             Just pe -> parsePE tokens pe
      updateCache tokens sym r
      return r

parsePE :: (Ord t, Ord s) => Input t -> PE t s -> ParserM t s (Result t s)

parsePE tokens (PETerminal tID) =
  case tokens of
    (tok@(tID', value)):tokens | tID' == tID ->
      return $ Just (ParseTreeAtom tok, tokens)
    _ -> return Nothing

parsePE tokens (PETerminalPredicated tID predicate) =
  case tokens of
    (tok@(tID', value)):tokens | tID' == tID && predicate value ->
      return $ Just (ParseTreeAtom tok, tokens)
    _ -> return Nothing

parsePE tokens (PELiteral str) = do
  ps <- get
  case tokens of
    (tok@(tID, value)):tokens
      | tID == pegLiteralTokenType (psGrammar ps) && value == str ->
          return $ Just (ParseTreeAtom tok, tokens)
    _ -> return Nothing

parsePE tokens (PENonTerminal sym) =
  parse tokens sym

parsePE tokens (PESequence es) = do
  rs <- parsePEs tokens es
  return $ (\(trees, tokens') -> (ParseTreeSequence trees, tokens')) <$> rs

parsePE tokens (PESequenceNode sym es) = do
  rs <- parsePEs tokens es
  return $ (\(trees, tokens') -> (ParseTreeNode sym trees, tokens')) <$> rs

parsePE tokens (PEAlternative es) =
  parsePEsAlternative tokens es

parsePE tokens (PEStar e) = do
  (trees, tokens') <- parsePERepeatedly tokens e
  return $ Just (ParseTreeSequence trees, tokens')

parsePE tokens (PEPlus e) = do
  (trees, tokens') <- parsePERepeatedly tokens e
  case trees of
    [] -> return Nothing
    _  -> return $ Just (ParseTreeSequence trees, tokens')

parsePE tokens (PEOptional e) = do
  r <- parsePE tokens e
  return $ r <|> Just (ParseTreeSequence [], tokens)

parsePE tokens (PEAnd e) = do
  r <- parsePE tokens e
  return $ r >> Just (ParseTreeSequence [], tokens)

parsePE tokens (PENot e) = do
  r <- parsePE tokens e
  case r of
    Nothing -> return $ Just (ParseTreeSequence [], tokens)
    _       -> return Nothing

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

parsePEsAlternative :: (Ord t, Ord s) =>
  Input t -> [PE t s] -> ParserM t s (Maybe (ParseTree t s, Input t))
parsePEsAlternative tokens [] =
  return Nothing
parsePEsAlternative tokens (e:es) = do
  r <- parsePE tokens e
  case r of
    Nothing -> parsePEsAlternative tokens es
    _ -> return r
