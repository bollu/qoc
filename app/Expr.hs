{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{- Based on:
     Functional Pearl: I am not a Number—I am a Free Variable
     https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.365.2479&rep=rep1&type=pdf -}
module Expr (
  Level(..),
  Expr,
  Binding(..),
  -- TODO: Other make-expression functions (and keep constructors private)
  mkForall,
  mkForalls,
  mkFun,
  mkFuns,
  mkLocal,
  splitForall,
  forallName,
  splitFun,
  funName,
  MonadTerm(..),
) where

import Control.Monad
import Data.Maybe
import Name

infixl 9 :$
infixr 6 :->
infixr 6 :=>

{- A binder (<var>: <type>) -}
data Binding = Binding LocalName Expr deriving (Show, Eq)
type Bindings = [Binding]

data Level =
    Zero              -- 0
  | Max Level Level   -- max _ _ (precedence 1)
  | Succ Level        -- _+1 (predence 2)
  deriving (Eq)

succDepth :: Level -> (Int, Maybe Level)
succDepth (Succ l) = (n+1, ml) where (n, ml) = succDepth l
succDepth Zero = (0, Nothing)
succDepth l = (0, Just l)

showLevel :: Int -> Level -> ShowS
showLevel _ Zero = showString "0"
showLevel _ l@(Succ _) =
  let (depth, ml) = succDepth l in
  case ml of
    Nothing -> showString $ show depth
    Just l -> showLevel 2 l . showString ("+" ++ show depth)
showLevel d (Max l₁ l₂) =
  showParen (d >= 1) $
    showString "max " . showLevel 1 l₁ . showString " " . showLevel 1 l₂

instance Show Level where
  showsPrec = showLevel

data Expr =
    Free LocalName
  | Bound Int
  | Const ResolvedName
  | Meta Integer
  | Sort Level
  | Expr :$ Expr        -- _ _ (precedence 3)
  | Binding :-> Scope   -- forall _, _ (precedence 2)
  | Binding :=> Scope   -- fun _ => _ (precedence 2)
  deriving (Eq)

-- TODO: This needs to live in a context where it knows what variables names are
-- available, so it can use fresh names for binders (eg. write "fun x => x"
-- as "fun x₁ => x₁" if "x" is already in context)
showExpr :: Int -> Expr -> ShowS
showExpr _ (Free l) = showString $ show l
showExpr _ (Bound b) = showString $ show b
showExpr _ (Const rn) = showString $ show rn
showExpr _ (Meta m) = showString "?m." . showString (show m)
showExpr _ (Sort Zero) = showString "Prop"
showExpr _ (Sort l) = showString "Type " . showsPrec 2 l
showExpr _ (e₁ :$ e₂) = showsPrec 3 e₁ . showString " " . showsPrec 3 e₂
showExpr p e@(b@(Binding n _) :-> _) =
  showString "forall " . showString (show b) . showString ", " .
  showsPrec 2 (snd . fromJust $ splitForall e n)
showExpr p e@(b@(Binding n _) :=> _) =
  showString "fun " . showString (show b) . showString " => " .
  showsPrec 2 (snd . fromJust $ splitFun e n)

instance Show Expr where
  showsPrec = showExpr

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
splitForall :: MonadPlus m => Expr -> LocalName -> m (Binding, Expr)
splitForall (Binding _ e :-> s) name =
  return (Binding name e, instantiate (Free name) s)
splitForall _ name = mzero

forallName :: MonadPlus m => Expr -> m LocalName
forallName (Binding name _ :-> _) = return name
forallName _ = mzero

mkFun :: Binding -> Expr -> Expr
mkFun b@(Binding name _) body = b :=> abstract name body

mkFuns :: Bindings -> Expr -> Expr
mkFuns bindings body = foldl (\expr b -> mkFun b expr) body bindings

-- `splitFun name (fun x => e)` introduces x as `name`. This is a low-level
-- function. The caller must check that `name` is available.
splitFun :: MonadPlus m => Expr -> LocalName -> m (Binding, Expr)
splitFun (Binding _ e :=> s) name =
  return (Binding name e, instantiate (Free name) s)
splitFun _ name = mzero

funName :: MonadPlus m => Expr -> m LocalName
funName (Binding name _ :=> _) = return name
funName _ = mzero

mkLocal :: LocalName -> Expr
mkLocal = Free

-- TODO: | Should have a stack of introduced binders
-- TODO: | Think harder about the operations to put there
class MonadTerm m where
  forallTelescoping ::
      [Maybe String] -- ^ proposed names for binders
   -> Expr -- ^ expression we are telescoping into
   -> ([Binding] -> Expr -> m a) -- ^ kontinuation
   -> m a
  funTelescoping ::
      [Maybe String] -- ^ proposed names for binders
   -> Expr -- ^ expression we are telescoping into
   -> ([Binding] -> Expr -> m a) -- ^ kontinuation
   -> m a
  -- introForall :: Maybe String -> Expr -> m (Binding, Expr)
  -- introFun :: Maybe String -> Expr -> m (Binding, Expr)

-- TODO: MonadEnv to handle global context (and provide a show function)
-- TODO: CoreM to handle local context (and provide a show function)
