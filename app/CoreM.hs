{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- This file implements the instance of 'Expr.MonadTerm' that is used throughout
-- the proof assistant.
module CoreM where
import qualified Data.Map.Strict as M
import qualified Control.Monad.Trans.State.Strict as S
import Control.Applicative
import Data.Traversable
import Data.Maybe
import Expr
import Name
import Control.Monad
import RList

-- Name -> Val
-- freshness

data State  = State {
  name2val :: M.Map LocalName Expr
}

type CoreM = S.State State


-- | Check if LocalName has been bound in the context.
member :: LocalName -> CoreM Bool
member name = (M.member name . name2val) <$> S.get


-- Generate a fresh name from a raw name. Breaks the
-- name to create a local name, then finds the latest index
-- to generate a fresh name,
genFreshName :: LocalName -> CoreM LocalName
genFreshName (LocalName (s, n)) =
  head <$> filterM (fmap not . member) [LocalName (s, i) | i <- [n,n+1..]]

-- Insert the local name n with e as the key.
-- This is low level, and does not check that n is fresh.
insert :: LocalName -> Expr -> CoreM ()
insert n e = S.modify (\st -> st { name2val = M.insert n e (name2val st) })


binderTelescopingGo ::
      RList Binding -- ^ accumulator of binder stack
   -> (Expr -> LocalName -> Maybe (Binding, Expr)) -- ^ splitter of expression
   -> (Expr -> Maybe LocalName) -- ^ name extractor
   -- vv arguments from {forall,fun}Telescoping vv --
   -> [Maybe String] -- ^ proposed names for binders
   -> Expr -- ^ expression we are telescoping into
   -> ([Binding] -> Expr -> CoreM a) -- ^ kontinuation
   -> CoreM a
binderTelescopingGo bs splitter getName [] e k = k (toList bs) e
binderTelescopingGo bs splitter getName (mn:mns) e k = do
  let nm = ((localNameFromString <$> mn) <|> getName e)
  nm <- sequence (genFreshName <$> nm)
  case nm >>= splitter e of
    Nothing -> k (toList bs) e
    Just (b, e) -> binderTelescopingGo (snoc bs b) splitter getName mns e k

instance MonadTerm CoreM where
  forallTelescoping = binderTelescopingGo mempty splitForall forallName
  funTelescoping = binderTelescopingGo mempty splitFun funName
