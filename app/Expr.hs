{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{- Based on:
     Functional Pearl: I am not a Number—I am a Free Variable
     https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.365.2479&rep=rep1&type=pdf -}
module Expr (Expr) where

import Control.Monad

{- An indexed name (eg. ("x", 0) for `x_0`). -}
type Name = (String, Int)

{- Qualified names including namespaces -}
-- | TODO: Are these always fully resolved?
type QualifiedName = [Name]

infixl 9 :$
infixr 6 :->
infixr 6 :=>

{- A binder (<var>: <type>) -}
data Binding = Binding Name Expr deriving (Show, Eq)
type Bindings = [Binding]


data Expr =
    Free Name
  | Bound Int
  | Const QualifiedName
  | Expr :$ Expr
  | Binding :-> Scope {- forall -}
  | Binding :=> Scope {- fun -}
  deriving (Show, Eq)

{- data Level = 
   Zero
 | Max
 | Succ -}

-- \x x. is ("x", Sort 0) :-> Scope (Bound 0)
-- Note that the RHS 'Scope (Bound 0)' is missing a binder. The LHS is the 
-- actual binder '("x", Sort 0)'

-- | Expr lacking a toplevel binder (Bound 0 does not have a binder parent)
-- | fi. Expr lam lam (0 1) is lam x y. x y
-- | and Scope lam (0 1) is the same (but the toplevel binder is missing)
newtype Scope = Scope Expr
  deriving (Show, Eq)


abstract :: Name -> Expr -> Scope
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

-- | TODO: Use name if supplied, something based on n otherwise?
-- | Or maybe have a different function (freshness monad-related) that builds a
-- | suitable name based on n.
splitForall :: MonadPlus m => Name -> Expr -> m (Binding, Expr)
splitForall name (Binding _ e :-> s) = return (Binding name e, instantiate (Free name) s)
splitForall name _ = mzero

mkFun :: Binding -> Expr -> Expr
mkFun b@(Binding name _) body = b :=> abstract name body

mkFuns :: Bindings -> Expr -> Expr
mkFuns bindings body = foldl (\expr b -> mkFun b expr) body bindings

splitFun :: MonadPlus m => Name -> Expr -> m (Binding, Expr)
splitFun name (Binding _ e :=> s) = return (Binding name e, instantiate (Free name) s)
splitFun name _ = mzero


{- TODO: Like the McBride paper, make the use of a root more systematic. But
   rather than the somewhat vague "agency", probably put it in a term
   manipulation monad?
   Edit: This is actually suggested at the end of the paper.

   What operations?
   ...
-}

class Monad m => MonadFresh m name | m -> name where 
  mkFresh :: name -> m name
  scopeFreshness :: m a -> m a -- ^ clear fresh names outside scope.

class (MonadPlus m, MonadFresh m Name) => MonadTerm m where
  introForall :: Maybe Name -> Expr -> m (Binding, Expr)


{- Question:
   Should we identify user-input names like "x: Int" and
   environment-local unique names with different types? -}

{- TODO:
   * Record the names of the binders in the Expr structure
   * Define a monad for manipulating the terms: BinderM
   * Use Name for hierarchical namespaces, not just agents/subfunctions -}
