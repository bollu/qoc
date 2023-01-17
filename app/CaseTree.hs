module CaseTree where
import Data.List.NonEmpty
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

