{-# LANGUAGE ViewPatterns #-}
module ExprIO(parseExpr, Token(..), ParseResult(..)) where

import Control.Applicative
import Data.Either
import Data.Maybe
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

data CharacterClass = CCWord | CCSymbol | CCPunct | CCSpace
  deriving (Show, Eq)

charClass :: Char -> CharacterClass
charClass c
  | c == '?' = CCWord
  | isAlphaNum c = CCWord
  | isSpace c = CCSpace
  | isPunctuation c = CCPunct
  | otherwise = CCSymbol

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
  -- | Class of the previous character (for class boundaries)
  psPreviousCharClass :: CharacterClass,
  -- | Stack of parse errors (most recent one first)
  psErrors :: [ParseError]
}

instance Show ParserState where
  show ps =
    "--- Parser state at " ++ show (psLoc ps) ++ " (indent " ++
    show (psMinimumIndent ps) ++ ", " ++ show (psIndentFinished ps) ++
    ") (last class: " ++ show (psPreviousCharClass ps) ++
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
      psPreviousCharClass = CCSpace,
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
       psIndentFinished = psIndentFinished ps || (c /= ' '),
       psPreviousCharClass = charClass c }

psAdvanceN :: Int -> ParserState -> ParserState
psAdvanceN n ps = iterate psAdvanceChar ps !! n

psAdvanceToEOL :: ParserState -> ParserState
psAdvanceToEOL ps =
  let (endOfLine, nextLine) = span (/= '\n') $ psInput ps in
  let newLoc = locAddColumns (length endOfLine) (psLoc ps) in
  let hadNonWS = any (/= ' ') endOfLine in
  ps { psLoc = newLoc,
       psInput = nextLine,
       psIndentFinished = psIndentFinished ps || hadNonWS,
       psPreviousCharClass =
         if null endOfLine then psPreviousCharClass ps
         else charClass (last endOfLine) }

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

-- Kleene star (repeats p zero or more times)
star :: Parser α -> Parser [α]
star p =
  (p >>= (\v -> star p >>= (\l -> return $ v:l)))
  <|> return []

-- Plus (repeats p one or more times)
plus :: Parser α -> Parser [α]
plus p = p >>= (\v -> star p >>= (\l -> return $ v:l))

-- Maybe (p zero or one time)
maybe :: Parser α -> Parser (Maybe α)
maybe p = (p >>= return . Just) <|> return Nothing

-- Kleene star with separator
starSepBy :: Parser α -> Parser β -> Parser [α]
starSepBy p sep =
  (p >>= (\v -> sep >> starSepBy p sep >>= (\l -> return $ v:l)))
  <|> return []

-- Plus with separator
plusSepBy :: Parser α -> Parser β -> Parser [α]
plusSepBy p sep = p >>= (\v -> star (sep >> p) >>= (\l -> return $ v:l))

{-
## Lexical elements
-}

classBoundary :: Parser ()
classBoundary = Parser $ \ps ->
  let next_class = fromMaybe CCSpace (charClass <$> listToMaybe (psInput ps)) in
  if next_class /= psPreviousCharClass ps
  then resultEmptyToken () ps
  else resultGenericError ps

singleChar :: Char -> Parser ()
singleChar c = Parser $ \ps ->
  if psEOF ps then
    resultError ("expected '" ++ (c : "', found EOF")) ps
  else if psNextChar ps /= c then
    resultError ("expected '" ++ (c : "', found '" ++ (psNextChar ps : "'"))) ps
  else
    resultTokenBetween () ps $ psAdvanceChar ps

fixedString :: String -> Parser ()
fixedString kw = Parser $ \ps ->
  if psEOF ps then
    resultError ("expected '" ++ kw ++ "', found EOF") ps
  else if not (kw `isPrefixOf` psInput ps) then
    resultError ("expected '" ++ kw ++ "'") ps
  else
    resultTokenBetween () ps $ psAdvanceN (length kw) ps

keyword :: String -> Parser ()
keyword kw = fixedString kw <* classBoundary

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

ident :: Parser String
ident = Parser $ \ps ->
  case identAux (psInput ps) of
    Nothing ->
      resultError "expected identifier" ps
    Just name ->
      if name `elem` language_keywords then
        resultError "expected identifier" ps
      else
        resultTokenBetween name ps $ psAdvanceN (length name) ps

{-
## Syntactic elements
-}

language_keywords :: [String]
language_keywords =
  ["fun", "forall", "Sort", "Type", "Prop"]

binding :: Parser Binding
binding =
    keyword "(" >> ident >>= (\i ->
    keyword ":" >> expr >>= (\t ->
    keyword ")" >>
    return (Binding (localNameFromString i) t)))

telescope :: Parser [Binding]
telescope = plus binding

exprParen :: Parser Expr
exprParen = keyword "(" >> expr >>= (\x -> keyword ")" >> return x)

exprFun :: Parser Expr
exprFun =
  keyword "fun" >> telescope >>= (\bs ->
  keyword "=>" >> expr >>= (\e ->
  return $ mkFuns bs e))

exprForall :: Parser Expr
exprForall =
  keyword "forall" >> telescope >>= (\bs ->
  keyword "," >> expr >>= (\e ->
  return $ mkForalls bs e))

{- exprArrow :: Parser Expr
exprArrow =
  expr >>= (\e ->
  keyword "->" >> expr >>= (\f ->
  return $ mkForall _ _)) -}

exprNoApp :: Parser Expr
exprNoApp =
      exprParen
  <|> exprFun
  <|> exprForall
  <|> (ident >>= return . mkLocal . localNameFromString)
  <|> parserError "invalid expression"

expr :: Parser Expr
expr = mkApps <$> plus exprNoApp

-- TODO


{-
## Tests
-}

parseExpr :: String -> ParseResult Expr
parseExpr = runParser expr . psInitialString
