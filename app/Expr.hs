{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

{-
## Expressions

`Expr` is the type of kernel terms. It's basically the same design as Lean
without the optimizations.

The term manipulation API is inspired by the paper "Functional Pearl: I am not
a Number—I am a Free Variable" [https://doi.org/10.1145/1017472.1017477].
-}

module Expr (
  -- Support types
  Level(..),
  Expr,
  Binding(..),
  -- Expression construction functions
  -- TODO: Properly design the expression construction API (also with MonadTerm)
  MonadTerm(..),
  mkForall,
  mkForalls,
  mkFun,
  mkFuns,
  mkLocal,
  mkConst,
  mkSort,
  -- Expression destruction
  splitForall,
  forallName,
  splitFun,
  funName,
  -- Printing
  prettyExpr,
) where

import Control.Monad
import Data.Maybe
import Data.List
import Name

infixl 9 :$
infixr 6 :->
infixr 6 :=>

{- A binder (<var>: <type>) -}
data Binding = Binding {
  bindingName :: LocalName,
  bindingType :: Expr
} deriving (Show)
type Bindings = [Binding]

data Level =
    Zero              -- 0
  | Max Level Level   -- max _ _ (precedence 1)
  | Succ Level        -- _+1 (predence 2)

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

{- We use De Bruijn indices to store terms, so that f.i.
     `fun x => x` is `Binding "x" (Sort 0) :=> Scope (Bound 0)`

   Note how the RHS of :=>, `Bound 0`, isn't a well-formed term since it's
   missing a binder. To enforce that this RHS isn't mistakenly interpreted as a
   term, it is classified as a `Scope` rather than an `Expr`. -}
newtype Scope = Scope Expr

showExpr :: Int -> Expr -> ShowS
showExpr _ (Free l) = showString $ show l
showExpr _ (Bound b) = showString $ "\\" ++ show b
showExpr _ (Const rn) = showString $ show rn
showExpr _ (Meta m) = showString "?m." . showString (show m)
showExpr _ (Sort Zero) = showString "Prop"
showExpr _ (Sort l) = showString "Type " . showsPrec 2 l
showExpr _ (e₁ :$ e₂) = showsPrec 3 e₁ . showString " " . showsPrec 3 e₂
showExpr p (Binding n t :-> f) =
  showString "forall (" . showString (show n) . showString ": " . showExpr 0 t .
  showString "), " . showsPrec 2 f
showExpr p (Binding n t :=> f) =
  showString "fun (" . showString (show n) . showString ": " . showExpr 0 t .
  showString ") => " . showsPrec 2 f

showScope :: Int -> Scope -> ShowS
showScope p (Scope e) = showsPrec p e

instance Show Expr where
  showsPrec = showExpr
instance Show Scope where
  showsPrec = showScope

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

mkConst :: ResolvedName -> Expr
mkConst = Const

mkSort :: Level -> Expr
mkSort = Sort

-- TODO: | Should have a stack of introduced binders
-- TODO: | Think harder about the operations to put there
class Monad m => MonadTerm m where
  {- Introduce fun or forall binders for the duration of the provided action -}
  forallTelescoping ::
      -- | Maximum number of binders to introduce (Nothing: no limit)
      Maybe Int ->
      -- | Names under which to introduce binders (Nothing: choose one)
      (Int -> Maybe LocalName) ->
      -- | Expression we are telescoping into
      Expr ->
      -- | Delimited continuation
      ([Binding]   -- ^ Introduced binders
       -> Expr     -- ^ Remaining expression
       -> m a)
      -> m a
  funTelescoping ::
      -- | Maximum number of binders to introduce (Nothing: no limit)
      Maybe Int ->
      -- | Names under which to introduce binders (Nothing: choose one)
      (Int -> Maybe LocalName) ->
      -- | Expression we are telescoping into
      Expr ->
      -- | Delimited continuation
      ([Binding]   -- ^ Introduced binders
       -> Expr     -- ^ Remaining expression
       -> m a)
      -> m a

-- TODO: MonadEnv to handle global context (and provide a show function)
-- TODO: CoreM to handle local context (and provide a show function)

prettyExpr :: MonadTerm m => Int -> Expr -> m ShowS
prettyExpr _ (Free l) = return $ showString $ show l
prettyExpr _ (Const rn) = return $ showString $ show rn
prettyExpr _ (Meta m) = return $ showString "?m." . showString (show m)
prettyExpr _ (Sort Zero) = return $ showString "Prop"
prettyExpr _ (Sort l) = return $ showString "Type " . showsPrec 2 l
prettyExpr _ (e₁ :$ e₂) = do
  e₁S <- prettyExpr 3 e₁
  e₂S <- prettyExpr 3 e₂
  return $ e₁S . showString " " . e₂S
prettyExpr p e@(_ :-> _) =
  forallTelescoping Nothing (const Nothing) e (\bindings f -> do
    bindingsS <- prettyTelescope bindings
    fS <- prettyExpr 2 f
    return $ showString "forall " . bindingsS . showString ", " . fS)
prettyExpr p e@(_ :=> _) =
  funTelescoping Nothing (const Nothing) e (\bindings f -> do
    bindingsS <- prettyTelescope bindings
    fS <- prettyExpr 2 f
    return $ showString "fun " . bindingsS . showString " => " . fS)

prettyTelescope :: MonadTerm m => [Binding] -> m ShowS
prettyTelescope bs = foldl1 (\s₁ s₂ -> s₁ . showString " " . s₂) <$>
                     mapM prettyBinding bs

prettyBinding :: MonadTerm m => Binding -> m ShowS
prettyBinding (Binding name typ) =
  prettyExpr 0 typ >>= (\typ_fun ->
    return $ showString ("(" ++ show name ++ ": ") . typ_fun . showString ")")
