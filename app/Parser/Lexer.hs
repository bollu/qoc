{-
# Lexer

The lexer reads source code and groups up individual characters into tokens.
The scanning method used here is highly convenient and correspondingly
inefficient.
-}

module Parser.Lexer where
import Data.Char
import Data.List
import Data.Maybe
import Util

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

-- Naming: "ls" is a LexerState
data LexerState = LexerState {
  -- | Current location
  lsLoc :: Loc,
  -- | Remaining input to parse
  lsInput :: String,
  -- | Whether we've met non-whitespace characters on the current line
  lsIndentFinished :: Bool,
  -- | Stack of lexing errors (most recent one first)
  lsErrors :: [LexError]
}

data TokenID =
    TokenIdent      -- ^ An identifier.
  | TokenInteger    -- ^ An integer literal (including prefix).
  | TokenString     -- ^ A quoted string.
  | TokenLiteral    -- ^ A literal string.
  | TokenSpace      -- ^ Whitespace.
  | TokenComment    -- ^ Comment.
  | TokenEOF        -- ^ EOF.
  deriving Show

type Token = (TokenID, String)
type TokenFunction = LexerState -> Maybe (Token, LexerState)

{-
## Utilities for working with the LexerState
-}

showLexError :: LexError -> String
showLexError (loc, str) = show loc ++ ": " ++ str

instance Show LexerState where
  showsPrec _ ls =
    showString "--- Lexer state at " .
    showString (if lsIndentFinished ls then ">" else "=") .
    showsPrec 0 (lsLoc ls) .
    showString " ---\n" .
    showString (lsInput ls) .
    showString "\n[Errors]:" .
    (if null (lsErrors ls)
     then showString " (none)\n"
     else showString $
       "\n" ++ concatMap ((++"\n") . showLexError) (lsErrors ls))

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
       lsInput = input,
       lsIndentFinished = lsIndentFinished ls || (c /= ' ') }

lsAdvanceN :: Int -> LexerState -> LexerState
lsAdvanceN n ls = iterate lsAdvanceChar ls !! n

lsAdvanceToEOL :: LexerState -> LexerState
lsAdvanceToEOL ls =
  let (endOfLine, nextLine) = span (/= '\n') $ lsInput ls in
  let newLoc = locAddColumns (length endOfLine) (lsLoc ls) in
  let hadNonWS = any (/= ' ') endOfLine in
  ls { lsLoc = newLoc,
       lsInput = nextLine,
       lsIndentFinished = lsIndentFinished ls || hadNonWS }

lsRecoverError :: LexerState -> LexerState
lsRecoverError ls =
  -- TODO: Skip to the next space
  lsAdvanceChar (lsPushError "no matching token" ls)

lsStartString :: String -> LexerState
lsStartString str =
  LexerState { lsLoc = locStartOfFile,
               lsInput = str,
               lsIndentFinished = False,
               lsErrors = [] }

{-
## Lexing functions
-}

-- Identifiers
-- ident = \w[\w0-9₀-₉_]*|_[\w0-9?₀-₉_]+   (where \w is Data.Char.isAlpha)

identValidLeading :: Char -> Bool
identValidLeading c = isAlpha c

identValidTrailing :: Char -> Bool
identValidTrailing c =
  isAlphaNum c || isSubscriptDigit c || (c == '_') || (c == '?')

identAux :: String -> Maybe String
identAux "" = Nothing
identAux ('_':str) =
  case takeWhile identValidTrailing str of
    "" -> Nothing
    name -> Just ('_':name)
identAux (c:str)
  | identValidLeading c = Just (c : takeWhile identValidTrailing str)
  | otherwise = Nothing

tIdent :: TokenFunction
tIdent ls = mkToken <$> identAux (lsInput ls)
  where mkToken name = ((TokenIdent, name), lsAdvanceN (length name) ls)

-- Literals

dropPrefix :: String -> String -> Maybe String
dropPrefix "" str = Just str
dropPrefix (c:pre) "" = Nothing
dropPrefix (c:pre) (c':str)
  | c == c' = dropPrefix pre str
  | otherwise = Nothing

tLiteral :: String -> TokenFunction
tLiteral lit ls = mkToken <$> dropPrefix lit (lsInput ls)
  where mkToken _ = ((TokenLiteral, lit), lsAdvanceN (length lit) ls)

{-
## Base tokens
-}

allTokens :: [TokenFunction]
allTokens = [
  tIdent,
  tLiteral "fun",
  tLiteral "forall",
  tLiteral "Sort",
  tLiteral "Type",
  tLiteral "Prop"]

nextToken :: LexerState -> (Token, LexerState)
nextToken ls =
  if null (lsInput ls) then
    ((TokenEOF, ""), ls)
  else
    let options = mapMaybe ($ ls) allTokens in
    case sortOn (\((tokenid, lexeme), ls) -> -length lexeme) options of
      [] -> nextToken (lsRecoverError ls)
      (res:_) -> res


