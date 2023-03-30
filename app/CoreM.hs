{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
## Core monad

This monad (taken from Lean's design) sits at the bottom of the monad stack. It
provides basic utilities for the proof assistant's environment.

1. Local context: CoreM tracks the local context, which allows it to generate
   fresh local names and thus navigate through terms, which is required for
   most common tasks including printing, parsing, etc. CoreM offers this
   functionality by being an instance of `MonadTerm`.

2. Global environment: Loaded modules, names in the global scope, currently
   open namespace, etc.

TODO:
* Global environment and modules
-}

module CoreM where
import qualified Data.Map.Strict as M
import qualified Control.Monad.State.Strict as S
import Control.Applicative
import Control.Monad.Loops
import Data.Traversable
import Data.Maybe
import Expr
import Name
import Control.Monad
import RList

data State  = State {
  name2val :: M.Map LocalName Expr
}

type CoreM = S.StateT State IO

runCoreM x = S.evalStateT x (State { name2val = M.empty })

-- | Check if LocalName has been bound in the context.
member :: LocalName -> CoreM Bool
member name = (M.member name . name2val) <$> S.get


-- Generate a fresh name from a raw name. Breaks the
-- name to create a local name, then finds the latest index
-- to generate a fresh name,
genFreshName :: LocalName -> CoreM LocalName
genFreshName (LocalName (s, n)) =
  fromJust <$> firstM (fmap not . member) [LocalName (s, i) | i <- [n,n+1..]]

-- Insert the local name n with e as the key.
-- This is low level, and does not check that n is fresh.
insert :: LocalName -> Expr -> CoreM ()
insert n e = S.modify (\st -> st { name2val = M.insert n e (name2val st) })

binderTelescoping :: forall a.
  -- | Splitter of expression
  (Expr -> LocalName -> Maybe (Binding, Expr)) ->
  -- | Name extractor
  (Expr -> Maybe LocalName) ->
  -- | Maximum number of introductions (Nothing: no limit)
  Maybe Int ->
  -- | Proposed names for binders
  (Int -> Maybe LocalName) ->
  -- | Expression we are telescoping into
  Expr ->
  -- | Delimited continuation ->
  ([Binding] -> Expr -> CoreM a) ->
  CoreM a

binderTelescoping splitter getName maxIntros nameHints e k = go mempty 0 e
  where
  go :: RList Binding -> Int -> Expr -> CoreM a
  go bs introsDone e
    | Just introsDone == maxIntros = k (toList bs) e
    | otherwise = do
        let nm = nameHints introsDone <|> getName e
        nm <- sequence (genFreshName <$> nm)
        case nm >>= splitter e of
          Nothing -> k (toList bs) e
          Just (b, f) -> go (snoc bs b) (introsDone+1) f

instance MonadTerm CoreM where
  forallTelescoping = binderTelescoping splitForall forallName
  funTelescoping = binderTelescoping splitFun funName
