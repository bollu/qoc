{- Based on:
     Functional Pearl: I am not a Number—I am a Free Variable
     https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.365.2479&rep=rep1&type=pdf -}
module Expr (Expr) where

import Control.Monad

{-
A stack of naming "scopes" with successive prefixes. Each item is either the
name of a variable (eg. ("x", 0) for `x_0`) or the name of a subscope, serving
as a unique prefix for that scope to generate its own unique names.
-}
type Name = [(String, Int)]

{- Names for constant terms -}
type ConstantName = String

infixl 9 :$
infixr 6 :->
infixr 6 :=>

data Expr =
    Free Name
  | Bound Int
  | Expr :$ Expr
  | Expr :-> Scope {- forall -}
  | Expr :=> Scope {- fun -}
  deriving (Show, Eq)

newtype Scope = Scope Expr
  deriving (Show, Eq)

{- A binder (<var>: <type>) -}
data Binding = Binding Name Expr
type Bindings = [Binding]

abstract :: Name -> Expr -> Scope
abstract name expr = Scope (abstractAtDepth 0 expr) where
  abstractAtDepth depth (Free name')
    | name == name' = Bound depth
    | otherwise     = Free name'
  abstractAtDepth _ (Bound depth') =
    Bound depth'
  abstractAtDepth depth (e :$ f) =
    abstractAtDepth depth e :$ abstractAtDepth depth f
  abstractAtDepth depth (e :-> Scope f) =
    abstractAtDepth depth e :-> Scope (abstractAtDepth (depth+1) f)
  abstractAtDepth depth (e :=> Scope f) =
    abstractAtDepth depth e :=> Scope (abstractAtDepth (depth+1) f)

instantiate :: Expr -> Scope -> Expr
instantiate image (Scope e) = replaceAtDepth 0 e where
  replaceAtDepth depth (Free name) =
    Free name
  replaceAtDepth depth (Bound depth')
    | depth == depth' = image
    | otherwise       = Bound depth'
  replaceAtDepth depth (e :$ f) =
    replaceAtDepth depth e :$ replaceAtDepth depth f
  replaceAtDepth depth (e :-> Scope f) =
    replaceAtDepth depth e :-> Scope (replaceAtDepth (depth+1) f)
  replaceAtDepth depth (e :=> Scope f) =
    replaceAtDepth depth e :=> Scope (replaceAtDepth (depth+1) f)

mkForall :: Binding -> Expr -> Expr
mkForall (Binding name domain) body = domain :-> abstract name body

mkForalls :: Bindings -> Expr -> Expr
mkForalls bindings body = foldl (\expr b -> mkForall b expr) body bindings

splitForall :: MonadPlus m => Name -> Expr -> m (Binding, Expr)
splitForall name (e :-> s) = return (Binding name e, instantiate (Free name) s)
splitForall name _ = mzero

mkFun :: Binding -> Expr -> Expr
mkFun (Binding name domain) body = domain :=> abstract name body

mkFuns :: Bindings -> Expr -> Expr
mkFuns bindings body = foldl (\expr b -> mkFun b expr) body bindings

splitFun :: MonadPlus m => Name -> Expr -> m (Binding, Expr)
splitFun name (e :=> s) = return (Binding name e, instantiate (Free name) s)
splitFun name _ = mzero


{- TODO: Like the McBride paper, make the use of a root more systematic. But
   rather than the somewhat vague "agency", probably put it in a term
   manipulation monad?
   Edit: This is actually suggested at the end of the paper. -}

{- TODO: Also, I don't really like the fact that you have to invent a
   completely new name every time you go down a binder. If would be much more
   useful if it could remember its original user-provided name and use it as a
   base automatically. -}

{- TODO:
   * Record the names of the binders in the Expr structure
   * Define a monad for manipulating the terms: BinderM
   * Use Name for hierarchical namespaces, not just agents/subfunctions -}
