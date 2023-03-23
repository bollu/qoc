{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module RList where
import Control.Applicative

-- This module implements the reverse of a list, where `snoc`
-- appends to the end of the list.

newtype RList a = RList [a]
  deriving (Eq, Ord, Show, Functor, Applicative,
            Alternative, Semigroup, Monoid)

snoc :: RList a -> a -> RList a
snoc (RList as) a = RList (a:as)

toList :: RList a -> [a]
toList (RList as) = reverse as
