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

import Data.Type.Roman ( Roman(..) )
import Data.Roman.Basic ( RomanNumeral, RomanSymbol(NULLA))
import Data.Maybe ( fromMaybe )
import Text.Read ( readMaybe )
import Data.List.NonEmpty (nonEmpty)

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
    fromEnum :: ExtendedRoman -> Int
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
        | signum i > 0 = (Pos, fromIntegral (abs i) ::RomanNumeral)
        | signum i < 0 = (Neg, fromIntegral (abs i) ::RomanNumeral)
        | otherwise = (Pos, fromIntegral (0::Int) ::RomanNumeral)



instance {-# OVERLAPPING #-} Ord ExtendedRoman where


instance Real ExtendedRoman where
    toRational :: ExtendedRoman -> Rational
    toRational (Pos, x) = toRational x
    toRational (Neg, x) = toRational $ negate x

instance Integral ExtendedRoman where


instance {-# OVERLAPPING #-} Read ExtendedRoman where
    readsPrec :: Int -> ReadS ExtendedRoman
    readsPrec _ [] = []
    readsPrec _ ['-'] = [((Neg, [NULLA]), [])]
    readsPrec _ ('-':s) =
        case readMaybe s ::Maybe RomanNumeral of
            Just i -> [((Neg,  i :: RomanNumeral), [])]
            Nothing -> [((Neg, [NULLA]), [])]
    readsPrec _ c =
        case readMaybe c ::Maybe RomanNumeral of
            Just i -> [((Pos, i ::  RomanNumeral), [])]
            Nothing -> []

instance {-# OVERLAPPING #-} Show ExtendedRoman where
    show :: ExtendedRoman -> String
    show (Neg,num) =
        "MALUS " ++ show num
    show (Pos, num) =
        show num