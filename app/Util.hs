module Util (
  toSubscriptDigits,
  ofSubscriptDigits,
  isSubscriptDigit,
) where

import Data.Char

toSubscriptDigits :: Int -> String
toSubscriptDigits = map (chr . (\x -> x - ord '0' + ord '₀') . ord) . show

ofSubscriptDigits :: String -> Int
ofSubscriptDigits = read . map (chr . (\x -> x - ord '₀' + ord '0') . ord)

isSubscriptDigit :: Char -> Bool
isSubscriptDigit c = ord c >= ord '₀' && ord c <= ord '₉'
