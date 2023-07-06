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
import qualified Modp.Lexer as ML

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

dumpLexer :: ML.Lexer -> ML.LexerState -> IO ML.LexerState
dumpLexer l ls =
  if ML.lsEOF ls then return ls else
  let (token, ls') = ML.nextToken l ls in do
    putStr $ show $ ML.lsLoc ls
    putStr " "
    print token
    dumpLexer l ls'

getExpr :: ML.Lexer -> CoreM ()
getExpr lexer = do
  lift $ putStr "\x1b[36m@>\x1b[0m "
  lift $ hFlush stdout
  l <- lift getLine
  case parseExpr l of
    ParseResult (Just t, ps) -> do
      str <- prettyExpr 0 (token t)
      lift $ putStrLn (str "")
    ParseResult (Nothing, ps) -> lift $ print ps
  let ls = ML.lsStartString "qoc" l
  lift $ (dumpLexer lexer ls >>= print)

repl :: ML.Lexer -> CoreM ()
repl lexer = getExpr lexer >> repl lexer

main :: IO ()
main = runCoreM do
  let lexer = makeQocLexer
  lift $ putStrLn "### Example expression ###"
  prettyExpr 0 id_ <*> pure "" >>= (lift . putStrLn)
  lift $ putStrLn "### Expression echo REPL ###"
  repl lexer
