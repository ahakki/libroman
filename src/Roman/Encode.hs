{- |
Module      :  $Header$
Description :  Converts arabic numerals to roman numerals
Copyright   :  (c) Alexander Hakki
License     :  BSD3

Maintainer  :  ahk@ahakki.xyz
Stability   :  experimental
Portability :  portable
-}



module Roman.Encode
    ( toRoman
    ) where

{- |
takes a Integral and converts it to a roman number.

throws an exception if string contains anything other than i, v, x, l, c, d, m

is case insensitive
-}

toRoman :: Integral a => a -> String

toRoman a
    | a >= 1000 =
        "M" ++ toRoman (a - 1000)

    | a >= 900 =
        "CM" ++ toRoman (a - 900)

    | a >= 500 =
        "D" ++ toRoman (a - 500)

    | a >= 400 =
        "CD" ++ toRoman (a - 400)

    | a >= 100 =
        "C" ++ toRoman (a - 100)

    | a >= 90 =
        "XC" ++ toRoman (a - 90)

    | a >= 50 =
        "L" ++ toRoman (a - 50)

    | a >= 40 =
        "XL" ++ toRoman (a - 40)

    | a >= 10 =
        "X" ++ toRoman (a - 10)

    | a >= 9 =
        "IX" ++ toRoman (a - 9)

    | a >= 5 =
        "V" ++ toRoman (a - 5)

    | a == 4 =
        "IV"

    | a >= 1 =
        "I" ++ toRoman (a - 1)

    | a == 0 =
        ""

    | a < 0 =
        toRoman (negate a)

    | otherwise =
        error "why?"
