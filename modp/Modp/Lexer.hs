{-
# Lexer

## Principle

The modular lexer reads textual input and groups up individual characters into
tokens. The toplevel mechanism for identifying the next token in a stream
favors versatility over performance, and is a set of simple functions of type

`TokenFunction = LexerState -> Maybe (Token, LexerState)`

which are allowed to perform any computable test on the input stream. The next
token at any point in a stream is the longest token returned by any registered
token function.

The entire input stream *must* be partitioned into tokens even if some tokens
are dropped later. This allows constructing concrete syntax trees that can be
reserialized into complete input files.

## Lexing scopes

In order to allow mixing different notation and syntax scopes in a single file,
the set of tokens recognized at any point in the input stream can be modified
dynamically.

Each token function belongs to a particular lexing scope, and which lexing
functions are considered at any point is determined by the current stack of
open scopes. Either the parser or an explicit API call can open/close scopes,
in two different methods:

- Cumulative: tokens of the opened scope are recognized in addition to any
  token previously recognized in other open scopes.
- Exclusive: only tokens of the opened scope are recognized until the scope is
  closed.

TODO: Consider variations of the scope opening model
* To allow subdivision as need (avoid "identity requirements")
* To allow special-scope closing notations (eg. "]" in "[block| ... ]")

## Advanced lexing functions

To improve performance, more complex token functions recognizing multiple
tokens can be combined. For instance, a DFA can be used to recognize an entire
set of regex-defined tokens. The only limitation on grouping is that a single
function cannot handle multiple scopes.

TODO: Provide DFA/regex-based lexers
-}

module Modp.Lexer where
import qualified Data.Map as M
import Data.Char
import Data.List
import Data.Maybe

{-
## Support types for identifying positions
-}

data Loc = Loc {
  -- | Number of code points since start of file
  locIdx :: Int,
  -- | Current line in file
  locRow :: Int,
  -- | Current column in line
  locCol :: Int

} deriving Eq

instance Show Loc where
  show loc = show (locRow loc) ++ (':' : show (locCol loc))

locStartOfFile :: Loc
locStartOfFile = Loc { locIdx = 0, locRow = 1, locCol = 1 }

locAddColumns :: Int -> Loc -> Loc
locAddColumns n (Loc idx row col) = Loc (idx+n) row (col+n)

locNewline :: Loc -> Loc
locNewline (Loc idx row col) = Loc idx (row+1) 1

locAdvance :: Char -> Loc -> Loc
locAdvance '\n' = locNewline . locAddColumns 1
locAdvance _ = locAddColumns 1

locSkip :: String -> Loc -> Loc
locSkip str loc = foldl (flip locAdvance) loc str

data Span = Span { spanStartLoc :: Loc, spanEndLoc :: Loc }

mergeSpans :: Span -> Span -> Span
mergeSpans sp₁ sp₂ =
  -- Skip sp₂ if it's empty (which it often is when using return in parsers)
  if spanStartLoc sp₂ == spanEndLoc sp₂
  then sp₁
  else Span (spanStartLoc sp₁) (spanEndLoc sp₂)

instance Show Span where
  show sp = show (spanStartLoc sp) ++ ".." ++ show (spanEndLoc sp)

{-
## Core lexing types
-}

type LexError = (Loc, String)
type LexScope = String

-- Naming: "ls" is a LexerState
data LexerState = LexerState {
  -- | Current location
  lsLoc :: Loc,
  -- | Remaining input to parse
  lsInput :: String,
  -- | Stack of lexing errors (most recent one first)
  lsErrors :: [LexError],
  -- | Stack of lexing scopes
  lsScopes :: [(LexScope, Bool)]
  -- | TODO: Extra data
}

type TokenID = String
type Token = (TokenID, String)
type TokenFunction = LexerState -> Maybe (Token, LexerState)

-- TODO: Better names and EOF marker
tokenIsEOF :: Token -> Bool
tokenIsEOF = null . fst

{-
## Utilities for working with the LexerState
-}

showLexError :: LexError -> String
showLexError (loc, str) = show loc ++ ": " ++ str

instance Show LexerState where
  showsPrec _ ls =
    showString "--- Lexer state at " .
    showsPrec 0 (lsLoc ls) .
    showString " ---\n" .
    showString (lsInput ls) .
    showString "\n[Errors]:" .
    (if null (lsErrors ls)
     then showString " (none)\n"
     else showString $
       "\n" ++ concatMap ((++"\n") . showLexError) (reverse $ lsErrors ls))

lsPushError :: String -> LexerState -> LexerState
lsPushError msg ls = ls { lsErrors = (lsLoc ls, msg) : lsErrors ls }

lsEOF :: LexerState -> Bool
lsEOF = null . lsInput

lsNextChar :: LexerState -> Char
lsNextChar (LexerState { lsInput = [] }) = '\0'
lsNextChar (LexerState { lsInput = (c:input) }) = c

lsAdvanceChar :: LexerState -> LexerState
lsAdvanceChar ls@(LexerState { lsInput = [] }) = ls
lsAdvanceChar ls@(LexerState { lsInput = (c:input) }) =
  ls { lsLoc = locAdvance c (lsLoc ls),
       lsInput = input }

lsAdvanceN :: Int -> LexerState -> LexerState
lsAdvanceN n ls = iterate lsAdvanceChar ls !! n

lsAdvanceToEOL :: LexerState -> LexerState
lsAdvanceToEOL ls =
  let (endOfLine, nextLine) = span (/= '\n') $ lsInput ls in
  let newLoc = locAddColumns (length endOfLine) (lsLoc ls) in
  ls { lsLoc = newLoc,
       lsInput = nextLine }

lsRecoverError :: LexerState -> LexerState
lsRecoverError ls =
  -- Skip one character
  case lsInput ls of
    "" -> lsPushError "no matching token at EOF" ls -- impossible
    c:_ -> lsAdvanceChar (lsPushError ("no token for '" ++ c : "'") ls)

lsStartString :: LexScope -> String -> LexerState
lsStartString scope str =
  LexerState { lsLoc = locStartOfFile,
               lsInput = str,
               lsErrors = [],
               lsScopes = [(scope, True)] }

{-
## Lexer rules and data
-}

data Lexer = Lexer {
  -- | Map of scope names to token functions
  lxScopes :: M.Map LexScope [TokenFunction]
}

emptyLexer :: Lexer
emptyLexer =
  Lexer {
    lxScopes = M.empty
  }

createScope :: Lexer -> LexScope -> Lexer
createScope l scope =
  -- Initialize scope with empty list of functions, unless already present
  l { lxScopes = M.insertWith (flip const) scope [] (lxScopes l) }

addTokenFunctions :: Lexer -> LexScope -> [TokenFunction] -> Lexer
addTokenFunctions l scope functions =
  l { lxScopes = M.insertWith (++) scope functions (lxScopes l) }

eligibleTokens :: Lexer -> LexerState -> [TokenFunction]
eligibleTokens l ls = aux (lxScopes l) (lsScopes ls)
  where
    aux rules [] = []
    aux rules ((scope, True):_) =
      M.findWithDefault [] scope rules
    aux rules ((scope, False):s) =
      M.findWithDefault [] scope rules ++ aux rules s

nextToken :: Lexer -> LexerState -> (Token, LexerState)
nextToken l ls =
  if lsEOF ls then
    (("", ""), ls)
  else
    let options = mapMaybe ($ ls) (eligibleTokens l ls) in
    -- This is a stable sort, preserving order
    case sortOn (\((tokenid, lexeme), ls) -> -length lexeme) options of
      [] -> nextToken l (lsRecoverError ls)
      (res:_) -> res

allTokens :: Lexer -> LexerState -> ([Token], LexerState)
allTokens l ls =
  if lsEOF ls then
    ([], ls)
  else
    let (t, ls') = nextToken l ls in
    let (ts, ls'') = allTokens l ls' in
    (t:ts, ls'')

{-
## Debugging functions
-}

dumpLexer :: Lexer -> LexerState -> IO LexerState
dumpLexer l ls =
  if lsEOF ls then
    return ls
  else
    let (token, ls') = nextToken l ls in do
      putStr $ show $ lsLoc ls
      putStr " "
      print token
      dumpLexer l ls'
