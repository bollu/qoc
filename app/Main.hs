{-# LANGUAGE BlockArguments #-}

module Main where
import Expr
import ExprIO
import CaseTree
import CoreM
import Name
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

main :: IO ()
main = runCoreM do
  prettyExpr 0 id_ <*> pure "" >>= (lift . putStrLn)
