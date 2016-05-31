-- Roman.hs
{- |
Module      :  $Header$
Description :  Integral Datatype for Roman Numerals
Copyright   :  (c) Alexander Hakki
License     :  BSD3

Maintainer  :  ahk@ahakki.xyz
Stability   :  experimental
Portability :  portable
-}

module Data.Roman
    ( Roman (..)
    ) where

{- |
The Class Roman implements a single Method, fromRoman, to convert to an Integral Type"
-}
class Roman r where
    fromRoman :: Integral b =>  r -> b