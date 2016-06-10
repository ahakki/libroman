--Internal.hs
{- |
Module      :  $Header$
Description :  libroman internal functions
Copyright   :  (c) Alexander Hakki
License     :  BSD3

Maintainer  :  ahk@ahakki.xyz
Stability   :  experimental
Portability :  portable
-}

import Data.Roman

parseRoman :: String -> RomanNumeral
parseRoman (x:xs) =
    (read [x]) : (parseRoman xs)
parseRoman [] =
    []