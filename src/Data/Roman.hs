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

class Roman r where
    fromRoman :: Integral b =>  r -> b