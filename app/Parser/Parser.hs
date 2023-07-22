{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser.Parser where
import qualified Data.Map as M
import qualified Modp.Lexer as ML
import qualified Modp.Parser as MP
import Parser.Lexer
import Control.Monad.State.Strict
import Expr
import Name

type PE = MP.PE String String
type Grammar = MP.PEG String String
type ParseTree = MP.ParseTree String String

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

_term_2 = [
  [MP.PETerminal "ident"],
  [MP.PELiteral "(", MP.PENonTerminal "term", MP.PELiteral ")"]]

_term_1 = [
  -- Order is important :x
  [MP.PENonTerminal "term_2", MP.PELiteral "->", MP.PENonTerminal "term_1"],
  [MP.PENonTerminal "term_2"]]

_term_trailing = [
  [MP.PELiteral "fun", MP.PEPlus (MP.PENonTerminal "binding"),
   MP.PELiteral "=>", MP.PENonTerminal "term"],
  [MP.PELiteral "forall", MP.PEPlus (MP.PENonTerminal "binding"),
   MP.PELiteral ",", MP.PENonTerminal "term"],
  [MP.PENonTerminal "sort"]]

_term = [
  [MP.PEPlus (MP.PENonTerminal "term_1"),
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
makeRules sym rules = (sym, makeAlt $ makeTags rules)
  where makeTags = reverse . fst . foldl (\(rs,i) r -> (makeSeq r i : rs, i+1)) ([], 1)
        makeSeq r i = MP.PESequenceNode (sym ++ "_" ++ show i) r
        makeAlt [r] = r
        makeAlt rs = MP.PEAlternative rs

makeQocGrammar :: Grammar
makeQocGrammar = MP.PEG {
  MP.pegRules = M.fromList [
      makeRules "sort" _sort,
      makeRules "universe" _universe,
      makeRules "binding" _binding,
      makeRules "term" _term,
      makeRules "term_1" _term_1,
      makeRules "term_2" _term_2,
      makeRules "term_trailing" _term_trailing
    ],
  MP.pegLiteralTokenType = "literal"
}

isUsefulToken :: ML.Token -> Bool
isUsefulToken ("ws", _) = False
isUsefulToken _ = True

qocParseExpr :: String -> Maybe (ParseTree, [(String, String)])
qocParseExpr string =
  let lexer = makeQocLexer in
  let (tokens, ls) = ML.allTokens lexer (ML.lsStartString "qoc" string) in
  -- TODO: Handle lexing errors
  -- TODO: Better handle ws and comments?! (we want them in CST ranges)
  let tokens' = filter isUsefulToken tokens in

  let ps = MP.ParserState {
    MP.psGrammar = makeQocGrammar,
    MP.psCache = M.empty
  } in
  fst $ runState (MP.parse tokens' "term") ps

qocParseAndElabExpr :: String -> Maybe Expr
qocParseAndElabExpr string = do
  (tree, input) <- qocParseExpr string
  case input of
    [] -> elabExpr tree
    _ -> Nothing

qocParserDumpString :: String -> IO ()
qocParserDumpString string = do
  putStrLn "--- Grammar ---"
  putStr $ MP.showPEG id id makeQocGrammar
  putStrLn "--- Parse tree (term) ---"
  case qocParseExpr string of
    Nothing -> print "Nothing"
    Just (tree, input) -> do
      putStr $ MP.showParseTreeGen id id 0 tree
      putStrLn "Input remaining:"
      print input

-- Naming convention: "t_" means "(parse) tree"

doApps :: [ParseTree] -> ParseTree -> Maybe Expr
doApps t_args t_trailing = do
  let t_args' = case t_trailing of
                  MP.ParseTreeSequence [] -> t_args
                  t -> t_args ++ [t]
  args <- mapM elabExpr t_args'
  return $ mkApps args

elabExpr :: ParseTree -> Maybe Expr
elabExpr (MP.ParseTreeNode "term_1" [MP.ParseTreeSequence t_args, t_trailing]) =
  doApps t_args t_trailing
elabExpr (MP.ParseTreeNode "term_2" [t_trailing]) =
  doApps [] t_trailing
elabExpr (MP.ParseTreeNode "term_1_1" [t_e, _, t_f]) = do
  e <- elabExpr t_e
  f <- elabExpr t_f
  return $ mkArrow e f
elabExpr (MP.ParseTreeNode "term_1_2" [t_e]) =
  elabExpr t_e
elabExpr (MP.ParseTreeNode "term_2_1" [MP.ParseTreeAtom ("ident", ident)]) =
  return $ mkLocal (Name.localNameFromString ident)
elabExpr (MP.ParseTreeNode "term_2_2" [_, t_e, _]) =
  elabExpr t_e
elabExpr (MP.ParseTreeNode "term_trailing_1"
    [_, MP.ParseTreeSequence t_bindings, _, t_e]) = do
  bindings <- mapM elabBinding t_bindings
  e <- elabExpr t_e
  return (mkFuns bindings e)
elabExpr (MP.ParseTreeNode "term_trailing_2"
    [_, MP.ParseTreeSequence t_bindings, _, t_e]) = do
  bindings <- mapM elabBinding t_bindings
  e <- elabExpr t_e
  return (mkForalls bindings e)
elabExpr (MP.ParseTreeNode "term_trailing_3" [t_sort]) =
  elabSort t_sort


elabBinding :: ParseTree -> Maybe Binding
elabBinding (MP.ParseTreeNode "binding_1" _) = Nothing
elabBinding (MP.ParseTreeNode "binding_2"
    [_, MP.ParseTreeAtom ("ident", ident), _, t_expr, _]) =
  (Binding $ localNameFromString ident) <$> elabExpr t_expr

elabSort :: ParseTree -> Maybe Expr
elabSort (MP.ParseTreeNode "sort_1" []) = Just $ mkSort Zero
elabSort (MP.ParseTreeNode "sort_2" [_, t]) = mkSort <$> elabUniverse t

elabUniverse :: ParseTree -> Maybe Level
elabUniverse (MP.ParseTreeNode "universe_1" [MP.ParseTreeAtom ("int", i)]) =
  Just $ iterate Succ Zero !! (read i :: Int)
elabUniverse _ = Nothing
