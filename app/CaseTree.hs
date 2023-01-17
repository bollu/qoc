module CaseTree where
import Data.List.NonEmpty(NonEmpty)
import Data.Traversable
import Control.Monad
{- 
Copyright (c) 2022 Siddharth Bhat. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-}

type FnName = String
type VarName = String
type CtorName = String

type M a = IO a -- we live in the IO monad, yes. easy debugging.


data Program = Program
  { programFn :: FnName
  , programContext :: Context
  , programType :: Term
  , clauses :: NonEmpty Clause
  }

data Clause = Clause
  { clauseFn :: FnName
  , clauseTerms :: [Term]
  , clauseRhs :: Rhs
  }

data Rhs =
  RhsTerm Term
  | RhsRefuted VarName

data Term = 
  TermVar VarName 
  | TermCtor CtorName [Term]
  | TermFnApp FnName [Term]

type Context = [(VarName, Term)]

data Pattern =
  PatternVar VarName
  | PatternCtor CtorName [Pattern]
  | PatternInaccessible Term

data Lhs = Lhs 
  { lhsFnName :: FnName
  , lhsArgs :: [Pattern]
  }

data Splitting =
  SplittingComp
  { splittingContext :: Context
  , splittingLhs :: Lhs
  } 
  | SplittingMatch 
  { splittingContext :: Context
  , splittingLhs :: Lhs
  , splittingType :: Term
  , splittingSplits :: [Splitting]
}


type Subst = [(VarName, Term)]
type MatchError = (Pattern, Term)
-- if matching succeeds, return a substitution
-- Otherwise, return (pattern, term) pair that does not unify.
unifyPattern :: Pattern -> Term -> Either MatchError Subst
unifyPattern (PatternVar x) t = Right $ [(x,t)]
unifyPattern (PatternCtor ctor ps) (TermCtor ctor' ts) = 
  if ctor == ctor' && length ps == length ts then
    foldM (\σ (p, t) -> (<> σ) <$> (unifyPattern p t)) mempty (zip ps ts) 
  else Left (PatternCtor ctor ps, TermCtor ctor' ts)
unifyPattern (PatternInaccessible _) t = Right mempty
unifyPattern p t = Left (p, t)

