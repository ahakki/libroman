-- Roman.hs
{- |
Module      :  $Header$
Description :  From Roman to Arabic and back
Copyright   :  (c) Alexander Hakki
License     :  BSD3

Maintainer  :  ahk@ahakki.xyz
Stability   :  experimental
Portability :  portable
-}

{-# LANGUAGE FlexibleInstances    #-}

module Data.Roman
    ( Roman (..)
    , RomanSymbol (..)
    , RomanList
    ) where

import Data.Char
import Data.List.Split

{- |
The Class Roman implements a single Method, fromRoman, to convert to an Integral Type"
-}
class Roman r where
    fromRoman :: Integral b =>  r -> b

{- |
RomanSymbols from I to M

Zero is represented as the latin word Nulla
-}
data RomanSymbol
    = Nulla
    | I
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

type RomanList =
    [RomanSymbol]

instance Roman RomanSymbol where
    fromRoman Nulla =
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
fromRoman on a RomanList also returns the expected result, if the Roman Number is not
stricly "correct", such as XIIX -> 18.
-}
instance Roman RomanList where
    fromRoman =
        sum . negateSubs . fromSplit . splitRn
      where
        negateSubs (x:y:ys)
            | x >= y =
                [x] ++ negateSubs (y:ys)
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
                splitRn' [] rn =
                    rn
                splitRn' sptr rn =
                    splitRn' (tail sptr) ( head sptr =<< rn)

        splitters =
            fmap (split . opts ) delims

        opts =
            dropBlanks . condense

        delims =
            fmap oneOf [[I],[V],[X],[L],[C],[D],[L]]

{- |
Roman Symbols implement Num. This has some issues, as the result of operations such as
I + I can not be represented as a single Roman Numeral. Use RomanList instead.
-}
instance Num RomanSymbol where
    (+) a b =
        fromInteger $ (fromRoman a) + (fromRoman b)

    (-) a b =
        fromInteger $ (fromRoman a) - (fromRoman b)

    (*) a b =
        fromInteger $ (fromRoman a) * (fromRoman b)

    negate = id                 -- Roman Symbols are always positive
    abs = id                    -- Roman Symbols are always positive
    signum a = 1                -- Roman Symbols are always positive

    -- fromInteger 0 =
    --     Nulla
    fromInteger 1 =
        I
    fromInteger 5 =
        V
    fromInteger 10 =
        X
    fromInteger 50 =
        L
    fromInteger 100 =
        C
    fromInteger 500 =
        D
    fromInteger 1000 =
        M
    fromInteger a
        | a < 0 =
            fromInteger $ negate a
        | otherwise =
            error $ "Data.Roman: There is no Roman Symbol for " ++ show a

{- |
Unlike single Roman Symbols, lists of them can implement Num in all cases. However,
Roman Numerals can never be negative.
-}
instance Num RomanList where
    (+) a b =
        fromInteger $ (fromRoman a) + (fromRoman b)

    (-) a b =
        fromInteger $ (fromRoman a) - (fromRoman b)

    (*) a b =
        fromInteger $ (fromRoman a) * (fromRoman b)

    negate = id                 -- Roman Symbols are always positive
    abs = id                    -- Roman Symbols are always positive
    signum a = 1                -- Roman Symbols are always positive

    fromInteger a
        | a >= 1000 =
            M     : fromInteger (a - 1000)

        | a >= 900 =
            C : M : fromInteger (a - 900)

        | a >= 500 =
            D     : fromInteger (a - 500)

        | a >= 400 =
            C : D : fromInteger (a - 400)

        | a >= 100 =
            C     : fromInteger (a - 100)

        | a >= 90 =
            X : C : fromInteger (a - 90)

        | a >= 50 =
            L     : fromInteger (a - 50)

        | a >= 40 =
            X : L : fromInteger (a - 40)

        | a >= 10 =
            X     : fromInteger (a - 10)

        | a >= 9 =
            I : X : fromInteger (a - 9)

        | a >= 5 =
            V     : fromInteger (a - 5)

        | a == 4 =
            I : V : []

        | a >= 1 =
            I     : fromInteger (a - 1)

        | a == 0 =
            []

        | a < 0 =
            fromInteger (negate a)

        | otherwise =
            error "Data.Roman: why?"

{- |
Read is case insensitive
-}
instance Read RomanSymbol where
    readsPrec p a =
      case fmap toUpper a of
        "I" ->
            [(I, "")]
        "V" ->
            [(V, "")]
        "X" ->
            [(X, "")]
        "L" ->
            [(L, "")]
        "C" ->
            [(C, "")]
        "D" ->
            [(D, "")]
        "M" ->
            [(M, "")]