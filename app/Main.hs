{-# LANGUAGE BlockArguments #-}

module Main where
import Expr
import ExprIO
import CaseTree
import CoreM
import Name
import System.IO
import Control.Monad.Trans
import Parser.Lexer
import Parser.Parser

fvar :: Expr
fvar = mkLocal (localNameFromString "x")

const_Int :: Expr
const_Int = mkConst (ResolvedName ["Int"])

id_Int :: Expr
id_Int = mkFun (Binding (localNameFromString "x") const_Int)
               (mkLocal (localNameFromString "x"))

id_ :: Expr
id_ = mkFun (Binding (localNameFromString "α") (mkSort (Succ Zero)))
      (mkFun (Binding (localNameFromString "x") (mkLocal (localNameFromString "α")))
       (mkLocal (localNameFromString "x")))

getExpr :: CoreM ()
getExpr = do
  lift $ putStr "\x1b[36m@>\x1b[0m "
  lift $ hFlush stdout
  l <- lift getLine
  case parseExpr l of
    ParseResult (Just t, ps) -> do
      str <- prettyExpr 0 (token t)
      lift $ putStrLn (str "")
    ParseResult (Nothing, ps) -> lift $ print ps

  lift $ qocLexerDumpString l
  lift $ qocParserDumpString l

repl :: CoreM ()
repl = getExpr >> repl

main :: IO ()
main = runCoreM do
  lift $ putStrLn "### Example expression ###"
  prettyExpr 0 id_ <*> pure "" >>= (lift . putStrLn)
  lift $ putStrLn "### Expression echo REPL ###"
  repl
