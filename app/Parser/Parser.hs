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

_sort = [
  [MP.PELiteral "Prop"],
  [MP.PELiteral "Type", MP.PEOptional (MP.PENonTerminal "universe")]]

_universe = [
  [MP.PETerminal "int"],
  [MP.PELiteral "_"]]

_binding = [
  [MP.PETerminal "ident"],
  [MP.PELiteral "(", MP.PETerminal "ident", MP.PELiteral ":",
   MP.PENonTerminal "term", MP.PELiteral ")"]]

_term_atom = [
  [MP.PETerminal "ident"],
  [MP.PELiteral "(", MP.PENonTerminal "term", MP.PELiteral ")"]]

_term_trailing = [
  [MP.PELiteral "fun", MP.PEPlus (MP.PENonTerminal "binding"),
   MP.PELiteral "=>", MP.PENonTerminal "term"],
  [MP.PELiteral "forall", MP.PEPlus (MP.PENonTerminal "binding"),
   MP.PELiteral ",", MP.PENonTerminal "term"],
  [MP.PENonTerminal "sort"]]

_term = [
  [MP.PEPlus (MP.PENonTerminal "term_atom"),
   MP.PEOptional (MP.PENonTerminal "term_trailing")],
  [MP.PENonTerminal "term_trailing"]]

{-
sort          ::= "Prop" | "Type" universe?
universe      ::= %int | "_"
binding       ::= ident | "(" ident ":" term ")"
term_atom     ::= ident | "(" term ")"
term_trailing ::= "fun" binding+ "=>" term | "forall" binding+ "," term
term          ::= term_atom+ term_trailing? | term_trailing

term ("->" term)*
-}

-- Parsing functions

makeRules :: String -> [[PE]] -> (String, PE)
makeRules sym rules = (sym, makeAlt $ map makeSeq rules)
  where makeSeq [r] = r
        makeSeq rs = MP.PESequenceNode sym rs
        makeAlt [r] = r
        makeAlt rs = MP.PEAlternative rs

makeQocGrammar :: Grammar
makeQocGrammar = MP.PEG {
  MP.pegRules = M.fromList [
      makeRules "sort" _sort,
      makeRules "universe" _universe,
      makeRules "binding" _binding,
      makeRules "term_atom" _term_atom,
      makeRules "term_trailing" _term_trailing,
      makeRules "term" _term
    ],
  MP.pegLiteralTokenType = "literal"
}

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
  putStrLn "--- Grammar ---"
  putStr $ MP.showPEG id id makeQocGrammar
  putStrLn "--- Parse tree (term) ---"
  case r of
    Nothing -> print "Nothing"
    Just (tree, input) -> do
      putStr $ MP.showParseTreeGen id id 0 tree
      putStrLn "Input remaining:"
      print input
