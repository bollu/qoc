{-# LANGUAGE ViewPatterns #-}
module ExprIO where 

import Control.Applicative
import Data.Either
import Data.List
import Data.Char
import Expr
import Name
import Util

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
## Parser support types
-}

data Token a = Token { token :: a, tokenSpan :: Span }

instance Show a => Show (Token a) where
  show t = show (token t) ++ "(" ++ show (tokenSpan t) ++ ")"

instance Functor Token where
  fmap f (Token t sp) = Token (f t) sp

type ParseError = (Loc, String)

showParseError :: ParseError -> String
showParseError (loc, msg) = show loc ++ ": " ++ msg

-- Naming: "ps" is a ParserState
data ParserState = ParserState {
  -- | Current location
  psLoc :: Loc,
  -- | Remaining input to parse
  psInput :: String,
  -- | Current minimum-indent requirement and whether we've met non-whitespace
  -- | characters on the current line
  psMinimumIndent :: Int,
  psIndentFinished :: Bool,
  -- | Stack of parse errors (most recent one first)
  psErrors :: [ParseError]
}

instance Show ParserState where
  show ps =
    "--- Parser state at " ++ show (psLoc ps) ++ " (indent " ++
    show (psMinimumIndent ps) ++ ", " ++ show (psIndentFinished ps) ++
    ")\n--- Remaining text:\n" ++ psInput ps ++ "\n--- Errors:" ++
    (if length (psErrors ps) == 0
     then " (none)\n"
     else "\n" ++ intercalate "\n" (map showParseError $ psErrors ps) ++ "\n")

psInitialString :: String -> ParserState
psInitialString input =
    ParserState {
      psLoc = locStartOfFile,
      psInput = input,
      psMinimumIndent = 0,
      psIndentFinished = False,
      psErrors = []
    }

-- Utilities for working with the parser state and input string

psPushError :: String -> ParserState -> ParserState
psPushError msg ps = ps { psErrors = (psLoc ps, msg) : psErrors ps }

psEOF :: ParserState -> Bool
psEOF ps = (psInput ps == [])

psNextChar :: ParserState -> Char
psNextChar (ParserState { psInput = [] }) = '\0'
psNextChar (ParserState { psInput = (c:input) }) = c

psAdvanceChar :: ParserState -> ParserState
psAdvanceChar ps@(ParserState { psInput = [] }) = ps
psAdvanceChar ps@(ParserState { psInput = (c:input) }) =
  ps { psLoc = locAdvance c (psLoc ps),
       psInput = input,
       psIndentFinished = psIndentFinished ps || (c /= ' ') }

psAdvanceN :: Int -> ParserState -> ParserState
psAdvanceN n ps = iterate psAdvanceChar ps !! n

psAdvanceToEOL :: ParserState -> ParserState
psAdvanceToEOL ps =
  let (endOfLine, nextLine) = span (/= '\n') $ psInput ps in
  let newLoc = locAddColumns (length endOfLine) (psLoc ps) in
  let hadNonWS = any (/= ' ') endOfLine in
  ps { psLoc = newLoc,
       psInput = nextLine,
       psIndentFinished = psIndentFinished ps || hadNonWS }

-- The indent requirement is met unless we have a non-whitespace character on a
-- column lower than psMinimumIndent. Comments are whitespace.
psSkipWhitespace :: ParserState -> ParserState
psSkipWhitespace ps = checkIndent (skip ps) where
  skip ps = case psInput ps of
    ' ':_ -> skip $ psAdvanceChar ps
    '\t':_ -> skip $ psPushError "tabs are verboten!" $ psAdvanceChar ps
    '-':'-':_ -> skip $ psAdvanceToEOL ps
    _ -> ps
    -- TODO: multi-line /- ... -/ comment
  checkIndent ps =
    if psInput ps /= [] && locCol (psLoc ps) < psMinimumIndent ps
    then psPushError "more indent needed" ps
    else ps

-- Utilities for working with the result object of parsing functions, when
-- constructing parsers manually.
-- t_ is a Maybe (Token a) ("_" alludes to "?")

newtype ParseResult a = ParseResult (Maybe (Token a), ParserState)

instance Show a => Show (ParseResult a) where
  show (ParseResult (t_, ps)) = show t_ ++ "\n" ++ show ps

instance Functor ParseResult where
  fmap f (ParseResult (t_, ps)) = ParseResult (fmap f <$> t_, ps)

instance Semigroup (ParseResult a) where
  ParseResult (Nothing, _) <> r = r
  l@(ParseResult (Just _, _)) <> _ = l

resultToken :: a -> Span -> ParserState -> ParseResult a
resultToken r span ps = ParseResult (Just (Token r span), ps)

resultTokenBetween :: a -> ParserState -> ParserState -> ParseResult a
resultTokenBetween r ps ps' = resultToken r (Span (psLoc ps) (psLoc ps')) ps'

resultEmptyToken :: a -> ParserState -> ParseResult a
resultEmptyToken r ps = resultToken r (Span (psLoc ps) (psLoc ps)) ps

resultError :: String -> ParserState -> ParseResult a
resultError msg ps = ParseResult (Nothing, psPushError msg ps)

resultGenericError :: ParserState -> ParseResult a
resultGenericError = resultError "syntax error"

resultBind :: ParseResult a -> (Token a -> ParserState -> ParseResult b) -> ParseResult b
resultBind (ParseResult (Nothing, ps)) k = ParseResult (Nothing, ps)
resultBind (ParseResult (Just t, ps)) k = k t ps

{-
## Parsing monad with parser combinators
-}

newtype Parser a = Parser { runParser :: ParserState -> ParseResult a }

parserToken :: a -> Span -> Parser a
parserToken = (Parser .) . resultToken

parserEmptyToken :: a -> Parser a
parserEmptyToken = Parser . resultEmptyToken

parserError :: String -> Parser a
parserError = Parser . resultError

parserGenericError :: Parser a
parserGenericError = Parser resultGenericError

parserWhitespace :: Parser ()
parserWhitespace = Parser $ \ps ->
  let ps' = psSkipWhitespace ps in
  resultToken () (Span (psLoc ps) (psLoc ps')) ps'

instance Functor Parser where
  -- <$> post-processes the token's held value with a pure function
  fmap f p = Parser $ (f <$>) . runParser p

instance Applicative Parser where
  -- return yields a value at the current location with an empty span
  pure = parserEmptyToken
  p₁ <*> p₂ = Parser $ \ps ->
    resultBind (runParser p₁ ps) $ \(Token t₁ span₁) ps ->
    resultBind (runParser p₂ ps) $ \(Token t₂ span₂) ps ->
    resultToken (t₁ t₂) (mergeSpans span₁ span₂) ps

instance Monad Parser where
  -- (>>=) binds parsers, accepting whitespace in the middle, and produces a
  -- token with merged spans
  p >>= k = Parser $ \ps ->
    resultBind (runParser p ps)                 $ \(Token t₁ span₁) ps ->
    resultBind (runParser parserWhitespace ps)  $ \_ ps ->
    resultBind (runParser (k t₁) ps)            $ \(Token t₂ span₂) ps ->
    resultToken t₂ (mergeSpans span₁ span₂) ps

instance Alternative Parser where
  -- empty is the parser failure function (TODO: message is useless)
  empty = parserGenericError
  -- <|> tries both options in the given order
  p₁ <|> p₂ = Parser $ \sp -> runParser p₁ sp <> runParser p₂ sp

{-
## Lexical elements
-}

singleChar :: Char -> Parser ()
singleChar c = Parser $ \ps ->
  if psEOF ps then
    resultError ("expected '" ++ (c : "', found EOF")) ps
  else if psNextChar ps /= c then
    resultError ("expected '" ++ (c : "', found '" ++ (psNextChar ps : "'"))) ps
  else
    resultTokenBetween () ps $ psAdvanceChar ps

keyword :: String -> Parser ()
keyword kw = Parser $ \ps ->
  if psEOF ps then
    resultError ("expected '" ++ kw ++ "', found EOF") ps
  else if not (kw `isPrefixOf` psInput ps) then
    resultError ("expected '" ++ kw ++ "'") ps
  else
    resultTokenBetween () ps $ psAdvanceN (length kw) ps

-- Identifiers
-- ident = \w[\w0-9₀-₉_]*|_[\w0-9₀-₉_]+   (where \w is Data.Char.isAlpha)

identValidLeading :: Char -> Bool
identValidLeading c = isAlpha c

identValidTrailing :: Char -> Bool
identValidTrailing c = isAlphaNum c || isSubscriptDigit c || (c == '_')

identAux :: String -> Maybe String
identAux "" = Nothing
identAux ('_':str) =
  case takeWhile identValidTrailing str of
    "" -> Nothing
    name -> Just ('_':name)
identAux (c:str)
  | identValidLeading c = Just (c : takeWhile identValidTrailing str)
  | otherwise = Nothing

ident :: Parser String
ident = Parser $ \ps ->
  case identAux (psInput ps) of
    Nothing ->
      resultError "expected identifier" ps
    Just name ->
      resultTokenBetween name ps $ psAdvanceN (length name) ps

{-
## Syntactic elements
-}

binding :: Parser Binding
binding =
    keyword "(" >> ident >>= (\i ->
    keyword ":" >> expr >>= (\t ->
    return $ Binding (localNameFromString i) t))

exprParen :: Parser Expr
exprParen = keyword "(" >> expr >>= (\x -> keyword ")" >> return x)

exprFun :: Parser Expr
exprFun =
  keyword "fun" >> binding >>= (\b ->
  keyword "=>" >> expr >>= (\e ->
  return $ mkFun b e))

expr :: Parser Expr
expr =
  (ident >>= return . mkLocal . localNameFromString)
  <|> exprParen
  <|> exprFun
  <|> parserError "invalid expression"

-- TODO


{-
## Tests
-}

parserTwoIdents :: Parser (String, String)
parserTwoIdents = do
  n1 <- ident
  n2 <- ident
  return (n1, n2)

test :: String -> ParseResult (String, String)
test = runParser parserTwoIdents . psInitialString
