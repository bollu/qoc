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
  locIdx :: Int -- number of code points.
  , locRow :: Int -- row  in the current line
  , locCol :: Int -- column number
}

instance Show Loc where
  show loc = show (locRow loc) ++ (':' : show (locCol loc))

advanceLoc :: Char -> Loc -> Loc 
advanceLoc '\n' (Loc idx row col) = Loc (idx+1) (row+1) 1
advanceLoc _ (Loc idx row col) = Loc (idx+1) row (col+1)

advanceLocStr :: String -> Loc -> Loc
advanceLocStr str loc = foldl (flip advanceLoc) loc str

data Span = Span { spanStartLoc :: Loc, spanEndLoc :: Loc }

mergeSpans :: Span -> Span -> Span
mergeSpans sp₁ sp₂ = Span (spanStartLoc sp₁) (spanEndLoc sp₂)

instance Show Span where
  show sp = show (spanStartLoc sp) ++ ".." ++ show (spanEndLoc sp)

data Token a = Token { token :: a, tokenSpan :: Span }

type ParseError = String 
newtype Parser a = Parser { runParser :: Loc -> String -> Either ParseError (Loc, String, Token a) }

parserReturn :: a -> Parser a
parserReturn r = Parser $ \loc str ->
  Right (loc, str, Token r $ Span loc loc)

{-
## Lexical elements
-}

singleChar :: Char -> Parser Char
singleChar = Parser . singleCharAux where
  singleCharAux c loc [] = Left $ "expected '" ++ (c : "', found EOF")
  singleCharAux c loc (c':str)
    | c' == c    = let loc' = advanceLoc c loc in Right (loc', str, Token c (Span loc loc'))
    | otherwise  = Left $ "expected '" ++ (c : "', found '" ++ (c' : "'"))

keyword :: String -> Parser ()
keyword kw = Parser $ \loc str ->
  if kw `isPrefixOf` str then
    let loc' = advanceLocStr kw loc in
    Right (loc', drop (length kw) str, Token () (Span loc loc'))
  else
    Left $ "expected '" ++ kw ++ "'"

-- Identifiers
-- ident = [a-zA-Z][a-zA-Z0-9₀-₉_]*|_[a-zA-Z0-9₀-₉_]+
-- TODO: Allow more Unicode characters in identifiers

identValidLeading :: Char -> Bool
identValidLeading c = isAlpha c

identValidTrailing :: Char -> Bool
identValidTrailing c = isAlphaNum c || isSubscriptDigit c || (c == '_')

identAux :: String -> Maybe (String, String)
identAux ('_':str) =
  let (trailing, str) = span identValidTrailing str in
  case trailing of
    "" -> Nothing
    _ -> Just ('_':trailing, str)
identAux (c:str)
  | identValidLeading c =
      let (trailing, str) = span identValidTrailing str in
      Just (c:trailing, str)
  | otherwise = Nothing
identAux _ = Nothing

ident :: Parser String
ident = Parser $ \loc str ->
  case identAux str of
    Nothing -> Left $ "expected identifier"
    Just (name, str) ->
      let loc' = advanceLocStr name loc in
      Right (loc', str, Token name (Span loc loc'))

-- Whitespace and comments (-- single line, /- multi-line -/)
-- TODO: Whitespace sensivity?!

whitespaceAux :: String -> (Int, String)
whitespaceAux (' ':s) = whitespaceAux s
whitespaceAux ('\t':s) = whitespaceAux s
whitespaceAux ('\n':s) = whitespaceAux s
whitespaceAux ('-':'-':s) = whitespaceAux $ snd $ break (= '\n') s
whitespaceAux ('/':'-':s) = whitespaceAux (skipMultiLineComment 1 s)
  where skipMultiLineComment 0 s = s
        skipMultiLineComment n ('/':'-':s) = skipMultiLineComment (n+1) s
        skipMultiLineComment n ('-':'/':s) = skipMultiLineComment (n-1) s
        skipMultiLineComment n (c:s) = skipMultiLineComment n s
        skipMultiLineComment n [] = Nothing
whitespaceAux s = Just s

{- WIP
whitespace :: Parser ()
whitespace = Parser $\loc str ->
  case whitespaceAux str of
    Nothing -> Left "unterminated comment"
    Just str -> Right (loc, str
-}

{-
## Parser combinator structures
-}

parserFail :: Parser a
parserFail = Parser $ \loc str ->
  Left $ "syntax error at " ++ show loc

instance Functor Parser where
  -- <$> post-processes the token with a pure function
  fmap f p = Parser $ \loc str -> do
    (loc, str, Token t sp) <- runParser p loc str
    return (loc, str, Token (f t) sp)

instance Applicative Parser where
  -- return yields a value at the current location with an empty span
  pure = parserReturn
  p₁ <*> p₂ = Parser $ \loc str -> do
    (loc, str, Token t₁ sp₁) <- runParser p₁ loc str
    (loc, str, Token t₂ sp₂) <- runParser p₂ loc str
    return (loc, str, Token (t₁ t₂) (mergeSpans sp₁ sp₂))

instance Monad Parser where
  -- (>>=) binds parseres and produces a token with merged spans
  p >>= k = Parser $ \loc str -> do
    (loc, str, Token t₁ sp₁) <- runParser p loc str
    (loc, str, Token t₂ sp₂) <- runParser (k t₁) loc str
    return (loc, str, Token t₂ (mergeSpans sp₁ sp₂))

instance Alternative Parser where
  -- empty is the parser failure function (parserFail is more explicit)
  empty = parserFail
  -- <|> tries both options in the given order
  p₁ <|> p₂ = Parser $ \loc str -> do
    runParser p₁ loc str <> runParser p₂ loc str

{-
## Syntactic elements
-}

-- TODO
