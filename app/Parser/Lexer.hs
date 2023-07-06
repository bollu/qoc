{-
# Lexer

The lexer reads source code and groups up individual characters into tokens.
The scanning method used here is highly convenient and correspondingly
inefficient.
-}

module Parser.Lexer where

import qualified Modp.Lexer as ML
import Control.Monad.Identity
import Data.Char
import Data.List
import Data.Maybe
import Util

-- Whitespace

tWs :: ML.TokenFunction
tWs ls =
  case takeWhile isSpace (ML.lsInput ls) of
    "" -> Nothing
    ws -> Just (("ws", ws), ML.lsAdvanceN (length ws) ls)

-- Literals

dropPrefix :: String -> String -> Maybe String
dropPrefix "" str = Just str
dropPrefix (c:pre) "" = Nothing
dropPrefix (c:pre) (c':str)
  | c == c' = dropPrefix pre str
  | otherwise = Nothing

tLiteral :: String -> ML.TokenFunction
tLiteral lit ls = mkToken <$> dropPrefix lit (ML.lsInput ls)
  where mkToken _ = (("literal", lit), ML.lsAdvanceN (length lit) ls)

-- Identifiers
-- ident ::= \w[\w0-9₀-₉_]*|_[\w0-9?₀-₉_]+   (where \w is Data.Char.isAlpha)

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

tIdent :: ML.TokenFunction
tIdent ls = mkToken <$> identAux (ML.lsInput ls)
  where mkToken name = (("ident", name), ML.lsAdvanceN (length name) ls)

-- Integers
-- int ::= [+-]?([0-9]+|0x[0-9a-fA-F]+|0o[0-7]+|0b[01]+)

isBinDigit :: Char -> Bool
isBinDigit c = (c == '0' || c == '1')

tInt :: ML.TokenFunction
tInt ls =
  case ML.lsInput ls of
    '+':str -> auxBase "+" str
    '-':str -> auxBase "-" str
    str -> auxBase "" str
  where
    auxBase prefix str =
      case str of
        '0':'x':str -> auxDigits (prefix ++ "0x") isHexDigit str
        '0':'X':str -> auxDigits (prefix ++ "0X") isHexDigit str
        '0':'o':str -> auxDigits (prefix ++ "0o") isOctDigit str
        '0':'O':str -> auxDigits (prefix ++ "0O") isOctDigit str
        '0':'b':str -> auxDigits (prefix ++ "0b") isBinDigit str
        '0':'B':str -> auxDigits (prefix ++ "0B") isBinDigit str
        str -> auxDigits prefix isDigit str
    auxDigits prefix predicate str =
      case takeWhile predicate str of
        "" -> Nothing
        digits -> let t = prefix ++ digits in
                  Just (("int", t), ML.lsAdvanceN (length t) ls)

-- Strings
-- TODO: String tokens

-- Comments
-- TODO: Comment tokens


-- Lexer definition

{- TODO:
   - Keep track of whether we've met non-whitespace characters on current line
   - Then, when checking tokens use the lexer state to check their indent
-}
makeQocLexer :: ML.Lexer
makeQocLexer =
  let l1 = ML.emptyLexer in
  let l2 = ML.createScope l1 "qoc" in
  let l3 = ML.addTokenFunctions l2 "qoc" [
         tWs,
         tInt,
         tLiteral "fun",
         tLiteral "forall",
         tLiteral "Sort",
         tLiteral "Type",
         tLiteral "Prop",
         tIdent] in
  l3
