-- Roman.hs
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

module Data.Roman
    ( Roman (..)
    , RomanSymbol (..)
    , RomanNumeral
    ) where


import           Data.Char
import           Data.List.Split
import           Control.Exception

-- Type class Roman
{- |
A type class for all types that can represent roman numerals
-}
class Roman r where
    {- |
    The Class Roman implements a single Method, fromRoman, to convert to an
    Integral Type
    -}
    fromRoman :: Integral b => r -> b


-- Roman Symbols
{- |
RomanSymbols from I to M

-}
data RomanSymbol
    = I
    | V
    | X
    | L
    | C
    | D
    | M
    deriving
        ( Eq
        , Ord
        , Show
        , Enum
        )


instance Roman RomanSymbol where
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
    readsPrec _ (a : []) =
      case toUpper a of
        'I' ->
            [(I,     [])]
        'V' ->
            [(V,     [])]
        'X' ->
            [(X,     [])]
        'L' ->
            [(L,     [])]
        'C' ->
            [(C,     [])]
        'D' ->
            [(D,     [])]
        'M' ->
            [(M,     [])]
        _   ->
            error "Data.Roman: Parse Error"
    readsPrec _ _ =
        error "Data.Roman: Parse Error"


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
    fromRoman =
        sum . negateSubs . fromSplit . splitRn
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
Be aware that, Roman Numerals can never be negative.
-}
instance Num RomanNumeral where
    (+) a b =
        fromInteger $ fromRoman a + fromRoman b

    (-) a b
        | a >= b =
            fromInteger $ fromRoman a - fromRoman b

        | otherwise =
            throw ( Underflow :: ArithException )

    (*) a b =
        fromInteger $ fromRoman a * fromRoman b

    negate = throw ( Underflow :: ArithException )
    abs = id
    signum _ = 1


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
    readsPrec _ a =
        [(parseRoman a, [])]
      where
        parseRoman :: String -> RomanNumeral
        parseRoman (x:xs) =
            (read [x] :: RomanSymbol) : (parseRoman xs)
        parseRoman [] =
            []

instance {-# OVERLAPPING #-} Show RomanNumeral where
    show (x:xs) =
            show x ++ show xs
    show [] =
         []

instance {-# OVERLAPPING #-} Ord RomanNumeral where
    compare x y=
        compare (toInteger x) (toInteger y)

    (<=) x y=
        (<=) (toInteger x) (toInteger y)

instance Real RomanNumeral where
    toRational a =
        toRational (fromRoman a :: Integer)

instance Integral RomanNumeral where
    quotRem x y =
        tupleConv $ quotRem (fromRoman x :: Integer) (fromRoman y :: Integer)
          where
            tupleConv :: Integral a => (a, a) ->(RomanNumeral, RomanNumeral)
            tupleConv (m, n) =
                (fromIntegral m, fromIntegral n)

    toInteger =
        fromRoman

instance Enum RomanNumeral where
    toEnum =
        fromIntegral

    fromEnum =
        fromIntegral
