-- Data.Roman.Extended.hs
{- |
Module      :  $Header$
Description :  Extended Roman Numerals
Copyright   :  (c) Alexander Hakki
License     :  BSD3

Maintainer  :  ahk@ahakki.xyz
Stability   :  experimental
Portability :  portable
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Roman.Extended (ExtendedRoman) where

import Data.Roman.Types ( Roman(..) )
import Data.Roman.Basic ( RomanNumeral,  RomanSymbol )

data Sign = Neg
          | Pos
          deriving
                ( Eq
                , Ord
                , Show
                , Enum
                )



type ExtendedRoman = (Sign, RomanNumeral)



instance Roman ExtendedRoman where
    fromRoman :: Integral b => ExtendedRoman -> b
    fromRoman (Pos, x) = fromRoman x
    fromRoman (Neg, x) = negate $ fromRoman x

instance Enum ExtendedRoman where
    fromEnum (Pos, x) = fromEnum x
    fromEnum (Neg, x) = fromEnum $ negate x

instance Num ExtendedRoman where
    (+) :: ExtendedRoman -> ExtendedRoman -> ExtendedRoman
    (+) a b =
        fromInteger $ fromRoman a + fromRoman b

    (-) :: ExtendedRoman -> ExtendedRoman -> ExtendedRoman
    (-) a b =
        fromInteger $ fromRoman a - fromRoman b

    (*) :: ExtendedRoman -> ExtendedRoman -> ExtendedRoman
    (*) a b =
        fromInteger $ fromRoman a * fromRoman b

    negate :: ExtendedRoman -> ExtendedRoman
    negate = id

    abs :: ExtendedRoman -> ExtendedRoman
    abs = id

    signum :: ExtendedRoman -> ExtendedRoman
    signum (_, x)
        | (fromIntegral x ::Integer) /= 0 =
             0
    signum (Pos, _) =
        1
    signum (Neg, _) =
        -1

    fromInteger :: Integer -> ExtendedRoman
    fromInteger i
        | abs i > 0 = (Pos, fromIntegral (abs i) ::RomanNumeral)
        | abs i < 0 = (Neg, fromIntegral (abs i) ::RomanNumeral)
        | otherwise = (Pos, fromIntegral (0::Int) ::RomanNumeral)



instance {-# OVERLAPPING #-} Ord ExtendedRoman where


instance Real ExtendedRoman where
    toRational (Pos, x) = toRational x
    toRational (Neg, x) = toRational $ negate x

instance Integral ExtendedRoman where