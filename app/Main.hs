{-# LANGUAGE BlockArguments #-}

module Main where
import Expr
import ExprIO
import CaseTree
import CoreM
import Name
import System.IO
import Control.Monad.Trans

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

getExpr :: IO ()
getExpr = do
  putStr "\x1b[36m@>\x1b[0m "
  hFlush stdout
  l <- getLine
  case parseExpr l of
    ParseResult (Just e, _) -> print e
    ParseResult (Nothing, ps) -> print ps

repl :: IO ()
repl = getExpr >> repl

main :: IO ()
main = runCoreM do
  lift $ putStrLn "### Example expression ###"
  prettyExpr 0 id_ <*> pure "" >>= (lift . putStrLn)
  lift $ putStrLn "### Expression echo REPL ###"
  lift repl
