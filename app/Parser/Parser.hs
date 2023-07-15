{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser.Parser where
import qualified Data.Map as M
import qualified Modp.Lexer as ML
import qualified Modp.Parser as MP
import Parser.Lexer
import Control.Monad.State.Strict

type PE = MP.PE String String
type Grammar = MP.PEG String String

-- Grammar rules

pLiteral :: String -> PE
pLiteral name = MP.PETerminalPredicated "literal" (== name)

_sort = [
  [pLiteral "Prop"],
  [pLiteral "Type", MP.PEOptional (MP.PENonTerminal "universe")]]

_universe = [
  [MP.PETerminal "int"],
  [pLiteral "_"]]

_binding = [
  [MP.PETerminal "ident"],
  [pLiteral "(", MP.PETerminal "ident", pLiteral ":",
   MP.PENonTerminal "term", pLiteral ")"]]

_term_atom = [
  [MP.PETerminal "ident"],
  [pLiteral "(", MP.PENonTerminal "term", pLiteral ")"]]

_term_trailing = [
  [pLiteral "fun", MP.PEPlus (MP.PENonTerminal "binding"), pLiteral "=>",
   MP.PENonTerminal "term"],
  [pLiteral "forall", MP.PEPlus (MP.PENonTerminal "binding"), pLiteral ",",
   MP.PENonTerminal "term"],
  [MP.PENonTerminal "sort"]]

_term = [
  [MP.PEPlus (MP.PENonTerminal "term_atom"),
   MP.PEOptional (MP.PENonTerminal "term_trailing")],
  [MP.PENonTerminal "term_trailing"]]

{-
sort          ::= "Prop" | "Type" universe?
universe      ::= #int | "_"
binding       ::= ident | "(" ident ":" term ")"
term_atom     ::= ident | "(" term ")"
term_trailing ::= "fun" binding+ "=>" term | "forall" binding+ "," term
term          ::= term_atom+ term_trailing? | term_trailing

term ("->" term)*
-}

-- Parsing functions

makeRules :: String -> [[PE]] -> (String, [PE])
makeRules sym rules = (sym, map aux rules)
  where aux [r] = r
        aux rs = MP.PESequenceNode sym rs

makeQocGrammar :: Grammar
makeQocGrammar = MP.PEG $ M.fromList [
    makeRules "sort" _sort,
    makeRules "universe" _universe,
    makeRules "binding" _binding,
    makeRules "term_atom" _term_atom,
    makeRules "term_trailing" _term_trailing,
    makeRules "term" _term
  ]

isUsefulToken :: ML.Token -> Bool
isUsefulToken ("ws", _) = False
isUsefulToken _ = True

qocParserDumpString :: String -> IO ()
qocParserDumpString string = do
  let lexer = makeQocLexer
  let (tokens, ls) = ML.allTokens lexer (ML.lsStartString "qoc" string)
  -- TODO: Handle lexing errors
  -- TODO: Better handle ws and comments?! (we want them in CST ranges)
  let tokens' = filter isUsefulToken tokens

  let ps = MP.ParserState {
    MP.psGrammar = makeQocGrammar,
    MP.psCache = M.empty
  }
  let (r, _) = runState (MP.parse tokens' "term") ps
  putStrLn "--- Parse tree (term) ---"
  case r of
    Nothing -> print "Nothing"
    Just (tree, input) -> do
      putStr $ MP.showParseTreeGen id id 0 tree
      putStrLn "Input remaining:"
      print input
