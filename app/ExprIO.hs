{-# LANGUAGE ViewPatterns #-}
module ExprIO where 
import Control.Applicative
import Data.Either

data Loc = Loc {
  locIdx :: Int -- number of code points.
  , locRow :: Int -- row  in the current line
  , locCol :: Int -- column number
}

advanceLoc :: Char -> Loc -> Loc 
advanceLoc '\n' (Loc idx row col) = Loc (idx+1) (row+1) 1
advanceLoc _ (Loc idx row col) = Loc (idx+1) row (col+1)

data Span = Span { spanStartLoc :: Loc, spanEndLoc :: Loc }

data Token a = Token { token :: a, tokenSpan :: Span }

type ParseError = String 
newtype Parser a = Parser { runParser :: Loc -> String -> Either ParseError (Loc, String, Token a) }


-- <|> -- choose between parses
-- <*> -- context free sequencing f (a -> b)-> f a -> f b
-- >>= : context sensitive seuqnece (a -> f b) -> f a -> f b

singleChar :: Char -> Parser Char
singleChar c = Parser $ \loc str -> 
  case str of
    []       -> Left $ "expected '" ++ (c:"' found EOF")
    head:str ->
      if head == c then
        let loc' = advanceLoc c loc in Right (loc', str, Token c (Span loc loc'))
      else
        Left $ "expected '" ++ (c:"' found '" ++ (head:"'."))

parserFail :: Parser a; parserFail = Parser $ \loc str -> Left ""

{-
instance Alternative Parser where
  empty = parserFail
  p1 <|> p2 = Parser $ \loc str -> 
    case runParser p1 loc str of
      Left err1 -> 
        case runParser p2 loc str of
          Right out -> Right out
          Left err2 -> error "unimplemented: do we keep a tree of errors"
      Right out -> Right out
-}

{-
singleChar c = Parser $ \loc str ->
  case grab str c of
  | Nothing  => Left _
  | Some str =>
      let loc' = advanceLoc c loc in (loc', str, Token (Span loc loc') c)
  where
    grab [] _ = Nothing
    grab (h:s) c = if h == c then Just s else Nothing

loc [] = Left _
singleChar c loc (head:str)
  | head == c    = Right (advanceLoc c loc, Token (Span loc (advanceLoc c loc)) c)
  | otherwise    = Left _


-- <|>

parserAlternative :: 
-}

