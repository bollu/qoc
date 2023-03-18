{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{- Based on:
     Functional Pearl: I am not a Number—I am a Free Variable
     https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.365.2479&rep=rep1&type=pdf -}
module Expr (
  Expr,
  Binding(..),
  -- TODO: Other make-expression functions (and keep constructors private)
  mkForall,
  mkForalls,
  mkFun,
  mkFuns,
  mkLocal,
) where

import Control.Monad
import Name

infixl 9 :$
infixr 6 :->
infixr 6 :=>

{- A binder (<var>: <type>) -}
data Binding = Binding LocalName Expr deriving (Show, Eq)
type Bindings = [Binding]

{- data Level =
   Zero
 | Max
 | Succ -}

data Expr =
    Free LocalName
  | Bound Int
  | Const ResolvedName
  | Expr :$ Expr
  | Binding :-> Scope {- forall -}
  | Binding :=> Scope {- fun -}
  deriving (Show, Eq)

{- We use De Bruijn indices to store terms, so that f.i.
     `fun x => x` is `Binding "x" (Sort 0) :=> Scope (Bound 0)`

   Note how the RHS of :=>, `Bound 0`, isn't a well-formed term since it's
   missing a binder. To enforce that this RHS isn't mistakenly interpreted as a
   term, it is classified as a `Scope` rathen than an `Expr`. -}
newtype Scope = Scope Expr
  deriving (Show, Eq)

-- `abstract name e` turns `name` into bound variable #0 in `e`.
abstract :: LocalName -> Expr -> Scope
abstract name expr = Scope (abstractAtDepth 0 expr) where
  abstractAtDepth depth (Free name')
    | name == name' = Bound depth
    | otherwise     = Free name'
  abstractAtDepth _ (Bound depth') =
    Bound depth'
  abstractAtDepth depth (e :$ f) =
    abstractAtDepth depth e :$ abstractAtDepth depth f
  abstractAtDepth depth ((Binding n e) :-> Scope f) =
    Binding n (abstractAtDepth depth e) :-> Scope (abstractAtDepth (depth+1) f)
  abstractAtDepth depth (Binding n e :=> Scope f) =
    Binding n (abstractAtDepth depth e) :=> Scope (abstractAtDepth (depth+1) f)

-- `instantiate image s` replaces bound variable #0 in `s` with `image`.
instantiate :: Expr -> Scope -> Expr
instantiate image (Scope e) = replaceAtDepth 0 e where
  replaceAtDepth depth (Free name) =
    Free name
  replaceAtDepth depth (Bound depth')
    | depth == depth' = image
    | otherwise       = Bound depth'
  replaceAtDepth depth (e :$ f) =
    replaceAtDepth depth e :$ replaceAtDepth depth f
  replaceAtDepth depth (Binding n e :-> Scope f) =
    Binding n (replaceAtDepth depth e) :-> Scope (replaceAtDepth (depth+1) f)
  replaceAtDepth depth (Binding n e :=> Scope f) =
    Binding n (replaceAtDepth depth e) :=> Scope (replaceAtDepth (depth+1) f)

mkForall :: Binding -> Expr -> Expr
mkForall b@(Binding name _) body = b :-> abstract name body

mkForalls :: Bindings -> Expr -> Expr
mkForalls bindings body = foldl (\expr b -> mkForall b expr) body bindings

-- `splitForall name (forall x, e)` introduces x as `name`. This is a low-level
-- function. The caller must check that `name` is available.
splitForall :: MonadPlus m => LocalName -> Expr -> m (Binding, Expr)
splitForall name (Binding _ e :-> s) =
  return (Binding name e, instantiate (Free name) s)
splitForall name _ = mzero

mkFun :: Binding -> Expr -> Expr
mkFun b@(Binding name _) body = b :=> abstract name body

mkFuns :: Bindings -> Expr -> Expr
mkFuns bindings body = foldl (\expr b -> mkFun b expr) body bindings

-- `splitFun name (fun x => e)` introduces x as `name`. This is a low-level
-- function. The caller must check that `name` is available.
splitFun :: MonadPlus m => LocalName -> Expr -> m (Binding, Expr)
splitFun name (Binding _ e :=> s) =
  return (Binding name e, instantiate (Free name) s)
splitFun name _ = mzero

mkLocal :: LocalName -> Expr
mkLocal = Free

-- TODO: | Should have a stack of introduced binders
-- TODO: | Think harder about the operations to put there
class (MonadPlus m) => MonadTerm m where
  forallTelescoping :: Maybe String -> Expr -> ([Binding] -> Expr -> m a) -> m a
  lambdaTelescoping :: Maybe String -> Expr -> ([Binding] -> Expr -> m a) -> m a
  -- introForall :: Maybe String -> Expr -> m (Binding, Expr)
  -- introFun :: Maybe String -> Expr -> m (Binding, Expr)
