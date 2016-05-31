-- RomanList.hs

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Data.Roman.RomanList
    ( RomanList (..)
    ) where

import           Data.Roman
import           Data.Roman.RomanSymbol

import           Data.List.Split

type RomanList =
    [RomanSymbol]

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
            I    : fromInteger (a - 1)

        | a == 0 =
            []

        | a < 0 =
            fromInteger (negate a)

        | otherwise =
            error "Data.Roman: why?"
