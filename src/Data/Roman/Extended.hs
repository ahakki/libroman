-- Data.Roman.Extended.hs
{- |
-- |
-- Module      :  $Header$
-- Description :  Extended Roman Numerals
-- Copyright   :  (c) Alexander Hakki
-- License     :  BSD3
--
-- Maintainer  :  ahk@ahakki.xyz
-- Stability   :  experimental
-- Portability :  portable
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Roman.Extended (ExtendedRoman) where

import Data.Roman.Basic (RomanNumeral, RomanSymbol (..))
import Data.Type.Roman (Roman (..))
import Text.Read (readMaybe)

data Sign = Neg | Pos
  deriving ( Eq, Ord, Show, Enum)

type ExtendedRoman = (Sign, RomanNumeral)

instance Roman ExtendedRoman where
  fromRoman :: (Integral b) => ExtendedRoman -> b
  fromRoman (Pos, x) = fromRoman x
  fromRoman (Neg, x) = negate $ fromRoman x
  toRoman :: (Integral a) => a -> ExtendedRoman
  toRoman = fromIntegral

instance Enum ExtendedRoman where
  fromEnum :: ExtendedRoman -> Int
  fromEnum (Pos, x) = fromEnum x
  fromEnum (Neg, x) = fromEnum $ negate x
  toEnum :: Int -> ExtendedRoman
  toEnum = fromIntegral

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
  negate (Pos, r) = (Neg, r)
  negate (Neg, r) = (Pos, r)

  abs :: ExtendedRoman -> ExtendedRoman
  abs (Neg, r) = (Pos, r)
  abs r = r

  signum :: ExtendedRoman -> ExtendedRoman
  signum (Pos, x)
    | (fromIntegral x :: Integer) == 0 =
        (Pos, [NULLA])
    | otherwise =
        1
  signum (Neg, x)
    | (fromIntegral x :: Integer) == 0 =
        (Neg, [NULLA])
    | otherwise =
        -1 ::ExtendedRoman

  fromInteger :: Integer -> ExtendedRoman
  fromInteger i
    | signum i > 0 = (Pos, fromIntegral (abs i) :: RomanNumeral)
    | signum i < 0 = (Neg, fromIntegral (abs i) :: RomanNumeral)
    | otherwise = (Pos, fromIntegral (0 :: Int) :: RomanNumeral)

instance {-# OVERLAPPING #-} Ord ExtendedRoman where
  compare :: ExtendedRoman -> ExtendedRoman -> Ordering
  compare x y =
    compare (toInteger x) (toInteger y)

  (<=) :: ExtendedRoman -> ExtendedRoman -> Bool
  (<=) x y =
    (<=) (toInteger x) (toInteger y)
  
  (>=) :: ExtendedRoman -> ExtendedRoman -> Bool
  (>=) x y =
    (>=) (toInteger x) (toInteger y)



instance Real ExtendedRoman where
  toRational :: ExtendedRoman -> Rational
  toRational (Pos, x) = toRational x
  toRational (Neg, x) = toRational $ negate x

instance Integral ExtendedRoman where
  toInteger :: ExtendedRoman -> Integer
  toInteger = fromRoman

  quotRem :: ExtendedRoman -> ExtendedRoman -> (ExtendedRoman, ExtendedRoman)
  quotRem (Pos, r1) (Pos, r2) = ((Pos, quot r1 r2),(Pos, rem r1 r2))
  quotRem (Pos, r1) (Neg, r2) = ((Neg, quot r1 r2),(Pos, rem r1 r2))
  quotRem (Neg, r1) (Pos, r2) = ((Neg, quot r1 r2),(Pos, rem r1 r2))
  quotRem (Neg, r1) (Neg, r2) = ((Pos, quot r1 r2),(Pos, rem r1 r2))

instance {-# OVERLAPPING #-} Read ExtendedRoman where
  readsPrec :: Int -> ReadS ExtendedRoman
  readsPrec _ [] = []
  readsPrec _ ['-'] = [((Neg, [NULLA]), [])]
  readsPrec _ ('-' : s) =
    case readMaybe s :: Maybe RomanNumeral of
      Just i -> [((Neg, i :: RomanNumeral), [])]
      Nothing -> [((Neg, [NULLA]), [])]
  readsPrec _ c =
    case readMaybe c :: Maybe RomanNumeral of
      Just i -> [((Pos, i :: RomanNumeral), [])]
      Nothing -> []

instance {-# OVERLAPPING #-} Show ExtendedRoman where
  show :: ExtendedRoman -> String
  show (Neg, num) =
    "MALUS " ++ show num
  show (Pos, num) =
    "BONUS " ++ show num