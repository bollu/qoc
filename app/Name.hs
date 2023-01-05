{-# LANGUAGE FunctionalDependencies #-}

{-
## Names, namespaces and hierarchies

We distinguish names from two different contexts. The global context holds
modules', namespaces' and constants' names, which are all Unicode strings
limited to certain letter-like characters. Here, we distinguish partially-
qualified constant names (`ConstantName`), whose interpretation depends on what
namespaces are opened, from fully-resolved constant names (`ResolvedName`).

In local contexts, there is a single type of non-nested name (`LocalName`).
Because tactics manipulate these and commonly want to introduce many variables
at once, we allow indexed identifies as pairs `(String, Int)`. This way, a
tactic can introduce, f.i., x₀, x₁, ... xₙ in one go.

Summary:
  ConstantName -- Partially-qualified constant name ("show"), ambiguous,
                  depends on open namespaces
  ResolvedName -- Fully-qualified constant name ("GHC.Show.show"), unambiguous
  LocalName    -- Indexed identifier used in local contexts ("x_0")
-}
module Name (
  ConstantName(..),
  ResolvedName(..),
  LocalName(..),
  localNameFromString,
) where

import Control.Monad
import Data.List
import Data.Char

{- We add constructors to make sure that `ConstantName` and `ResolvedName` are
   not interoperable. We also intentionally do not provide `Eq` for
   `ConstantName` since it is ambiguous and should be resolved first. -}
newtype ConstantName = ConstantName [String]

instance Show ConstantName where
  show (ConstantName l) = '`' : intercalate "." l

newtype ResolvedName = ResolvedName [String]
  deriving Eq

instance Show ResolvedName where
  show (ResolvedName l) = "``" ++ intercalate "." l

{- An indexed name local name. eg. ("x", 0) for `x₀`. The index is written as a
   subscript using Unicode characters, and the string is not allowed to end
   with a Unicode subscript digit. Any negative integer means no suscript. -}
newtype LocalName = LocalName (String, Int)

instance Eq LocalName where
  LocalName (x, i) == LocalName (y, j) = x == y && ((i < 0 && j < 0) || i == j)

toSubscriptDigits :: Int -> String
toSubscriptDigits = map (chr . (\x -> x - ord '0' + ord '₀') . ord) . show

ofSubscriptDigits :: String -> Int
ofSubscriptDigits = read . map (chr . (\x -> x - ord '₀' + ord '0') . ord)

isSubscriptDigit :: Char -> Bool
isSubscriptDigit c = ord c >= ord '₀' && ord c <= ord '₉'

instance Show LocalName where
  show (LocalName (x, i)) = if i >= 0 then x ++ toSubscriptDigits i else x

localNameFromString :: String -> LocalName
localNameFromString s =
  let (r_name, r_digits) = span isSubscriptDigit (reverse s) in
  LocalName (reverse r_name, ofSubscriptDigits $ reverse r_digits)
