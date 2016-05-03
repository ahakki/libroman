{- |
Module      :  $Header$
Description :  Converts roman to arabic numerals
Copyright   :  (c) Alexander Hakki
License     :  BSD3

Maintainer  :  ahk@ahakki.xyz
Stability   :  experimental
Portability :  portable
-}



module Roman.Decode
    ( fromRoman
    ) where

import           Data.Char


{- |
takes a roman number and converts it to an arabic number.

throws exception if argument contains anything other than i, v, x, l, c, d, m

is case insensitive
-}

fromRoman :: Integral a => String -> a

fromRoman a
    | checkRoman a =
        fromRoman' $ map toUpper a

    | otherwise =
        error "not a roman numeral"
  where
    fromRoman' :: Integral a => String -> a
    fromRoman' (x:y:ys)
        | fromRoman'' x < fromRoman'' y =
            fromRoman'' y - fromRoman'' x + fromRoman' ys
        | otherwise =
            fromRoman'' x + fromRoman' (y:ys)

    fromRoman' (x:xs) =
        fromRoman'' x + fromRoman' xs

    fromRoman' _ =
        0


    fromRoman'' :: Integral a => Char -> a
    fromRoman'' 'I' =
        1

    fromRoman'' 'V' =
        5

    fromRoman'' 'X' =
        10

    fromRoman'' 'L' =
        50

    fromRoman'' 'C' =
        100

    fromRoman'' 'D' =
        500

    fromRoman'' 'M' =
        1000

    fromRoman'' _ =
        0


checkRoman :: String -> Bool
checkRoman (x:xs) =
   toUpper x `elem` ['I', 'V', 'X', 'L', 'C', 'D', 'M'] && checkRoman xs

checkRoman _ =
    True
