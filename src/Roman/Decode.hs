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


fromRoman :: Integral a => String -> a

fromRoman [] =
    0

fromRoman "I" =
    1

fromRoman "V" =
    5

fromRoman "X" =
    10

fromRoman "L" =
    50

fromRoman "C" =
    100

fromRoman "D" =
    500

fromRoman "M" =
    1000

fromRoman (f : s: xs) =
    undefined
