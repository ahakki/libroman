-- Basic.hs
{- |
Module      :  $Header$
Description :  Roman Numerals
Copyright   :  (c) Alexander Hakki
License     :  BSD3

Maintainer  :  ahk@ahakki.xyz
Stability   :  experimental
Portability :  portable
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Data.Roman.Basic
    ( RomanNumeral
    , RomanSymbol (..)
    ) where

import Data.Type.Roman

import Data.Char ( toUpper )
import Data.List.Split ( condense, dropBlanks, oneOf, split )
import Text.Read (readMaybe)
import Data.Maybe ( isJust )

-- Roman Symbols
{- |
RomanSymbols from I to M

Zero is represented as N for the latin word NULLA
-}
data RomanSymbol where
  NULLA :: RomanSymbol
  N :: RomanSymbol
  I :: RomanSymbol
  V :: RomanSymbol
  X :: RomanSymbol
  L :: RomanSymbol
  C :: RomanSymbol
  D :: RomanSymbol
  M :: RomanSymbol
  deriving (Eq, Ord, Show, Enum)

instance Roman RomanSymbol where
    fromRoman NULLA =       --NULLA is BACK
        0
    fromRoman N =           --Now we still support N!
        0
    fromRoman I =
        1
    fromRoman V =
        5
    fromRoman X =
        10
    fromRoman L =
        50
    fromRoman C =
        100
    fromRoman D =
        500
    fromRoman M =
        1000


{- |
Read is case insensitive
-}
instance Read RomanSymbol where
    readsPrec :: Int -> ReadS RomanSymbol
    readsPrec _ [] = []
    readsPrec _ ('N':'U':'L':'L':'A':_) =
        [(NULLA, [])] --we still read NULLA correctly as NULLA
    readsPrec _ (token:rest) =
      case toUpper token of
        'N' -> [(N, rest)] -- Entry of N is recorded as N
        'I' -> [(I, rest)]
        'V' -> [(V, rest)]
        'X' -> [(X, rest)]
        'L' -> [(L, rest)]
        'C' -> [(C, rest)]
        'D' -> [(D, rest)]
        'M' -> [(M, rest)]
        _   -> []
    readsPrec _ _ =
        []

{- |
Roman Numerals are represented as Lists of RomanSymbols
-}
type RomanNumeral =
    [RomanSymbol]

{- |
fromRoman on a RomanNumeral also returns the expected result, if the Roman
Number is not stricly "correct", such as XIIX -> 18.
-}
instance Roman RomanNumeral where
    fromRoman :: Integral b => RomanNumeral -> b
    fromRoman input
        | elem N input && elem NULLA input = 0
    fromRoman input =
        sum . negateSubs . fromSplit . splitRn $ input
      where
        negateSubs :: (Num a, Ord a) => [a] -> [a]
        negateSubs (x:y:ys)
            | x >= y =
                x : negateSubs (y : ys)
            | x < y =
                [negate x, y] ++ negateSubs ys

        negateSubs [x] =
            [x]

        negateSubs _ =
            []

        fromSplit =
                fmap (sum . fmap fromRoman)

        splitRn rn =
            splitRn' (tail splitters) (head splitters rn)
              where
                splitRn' [] r =
                    r
                splitRn' sptr r =
                    splitRn' (tail sptr) ( head sptr =<< r)

        splitters =
            fmap (split . opts) delims

        opts =
            dropBlanks . condense

        delims =
            fmap oneOf [[I],[V],[X],[L],[C],[D],[L]]

{- |
Be aware that, Basic Roman Numerals can never be negative.
-}
instance Num RomanNumeral where
    (+) :: RomanNumeral -> RomanNumeral -> RomanNumeral
    (+) a b =
        fromInteger $ fromRoman a + fromRoman b

    (-) :: RomanNumeral -> RomanNumeral -> RomanNumeral
    (-) a b
        | a >= b =
            fromInteger $ fromRoman a - fromRoman b
        | otherwise =
            negate $ fromInteger $ fromRoman a - fromRoman b

    (*) :: RomanNumeral -> RomanNumeral -> RomanNumeral
    (*) a b =
        fromInteger $ fromRoman a * fromRoman b

    negate :: RomanNumeral -> RomanNumeral
    negate = id
    abs :: RomanNumeral -> RomanNumeral
    abs = id
    signum :: RomanNumeral -> RomanNumeral
    signum _ = 1

    fromInteger :: Integer -> RomanNumeral
    fromInteger 0 =
        [NULLA]
    fromInteger r =
        fromInteger' r
      where
        fromInteger' a
            | a >= 1000 =
                M     : fromInteger' (a - 1000)

            | a >= 900 =
                C : M : fromInteger' (a - 900)

            | a >= 500 =
                D     : fromInteger' (a - 500)

            | a >= 400 =
                C : D : fromInteger' (a - 400)

            | a >= 100 =
                C     : fromInteger' (a - 100)

            | a >= 90 =
                X : C : fromInteger' (a - 90)

            | a >= 50 =
                L     : fromInteger' (a - 50)

            | a >= 40 =
                X : L : fromInteger' (a - 40)

            | a >= 10 =
                X     : fromInteger' (a - 10)

            | a >= 9 =
                I : X : fromInteger' (a - 9)

            | a >= 5 =
                V     : fromInteger' (a - 5)

            | a == 4 =
                I : V : fromInteger' (a - 4)

            | a >= 1 =
                I     : fromInteger' (a - 1)

            | a == 0 =
                []

            | a < 0 =
                fromInteger' (negate a)

            | otherwise =
                error "Data.Roman: why?"


{-|
Overlaps instance Read [a] with a specific version,
so that "xxi" -> [X, X, I]
-}
instance {-# OVERLAPPING #-} Read RomanNumeral where
    readsPrec :: Int -> ReadS RomanNumeral
    readsPrec _ [] = []
    readsPrec _ input =
        if (readsPrec 1 input ::[(RomanSymbol, String)]) == []
        then []
        else if (fst . head)(readsPrec 1 input ::[(RomanSymbol, String)]) == NULLA
        then [(([NULLA]),(drop 5 input))]
        else part [] input
      where
        part:: RomanNumeral -> String -> [(RomanNumeral, String)]
        part [] [] = []
        part [] (x:xs) =
             case (readsPrec 1 (x:xs)) ::[(RomanSymbol, String)] of
                [] -> error "Data.Roman.Basic: ERROR in Instance Read RomanNumeral"
                [(N,_)] -> [([N], xs)]
                [(a,_)] -> part ((read [x] ::RomanSymbol):[]) xs
        part acc [] =
            [(reverse acc, [])]
        part acc xs =
            case (readsPrec 1  xs ::[(RomanSymbol,String)]) of
                [] -> [(reverse acc, xs)]
                [(N,_)]  -> [(reverse acc, xs)]
                [(NULLA,_)]-> [(reverse acc, xs)]
                [(num,_)] -> part (num:acc) $ tail xs

parseRoman :: String -> RomanNumeral
parseRoman (x:xs) =
    (read [x] :: RomanSymbol) : parseRoman xs 
parseRoman [] =
            []



    -- readsPrec _ a
    --     | fmap toUpper a == "NULLA" =
    --         [([NULLA], [])]
    --     | fmap toUpper a == "N" =
    --         [([N], [])]
    --     | otherwise =
    --         [(parseRoman a, [])]

instance {-# OVERLAPPING #-} Show RomanNumeral where
    show :: RomanNumeral -> String
    show [] = "EMPTY ROMAN"
    show input =
            show' input
      where
        show' [] =
             []
        show' (x:xs) = show x ++ show' xs

instance {-# OVERLAPPING #-} Ord RomanNumeral where
    compare :: RomanNumeral -> RomanNumeral -> Ordering
    compare x y=
        compare (toInteger x) (toInteger y)

    (<=) :: RomanNumeral -> RomanNumeral -> Bool
    (<=) x y=
        (<=) (toInteger x) (toInteger y)

instance Real RomanNumeral where
    toRational :: RomanNumeral -> Rational
    toRational a =
        toRational (fromRoman a :: Integer)

instance Integral RomanNumeral where
    quotRem :: RomanNumeral -> RomanNumeral -> (RomanNumeral, RomanNumeral)
    quotRem x y =
        tupleConv $ quotRem (fromRoman x :: Integer) (fromRoman y :: Integer)
          where
            tupleConv :: Integral a => (a, a) ->(RomanNumeral, RomanNumeral)
            tupleConv (m, n) =
                (fromIntegral m, fromIntegral n)

    toInteger :: RomanNumeral -> Integer
    toInteger =
        fromRoman

instance Enum RomanNumeral where
    toEnum :: Int -> RomanNumeral
    toEnum =
        fromIntegral

    fromEnum :: RomanNumeral -> Int
    fromEnum =
        fromIntegral
