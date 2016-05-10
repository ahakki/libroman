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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Roman where

class Roman r where
    fromRoman :: Integral b =>  r -> b
   

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
        , Read
        , Show
        , Enum
        )
        
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
        
    
instance Num RomanSymbol where
    (+) a b =
        fromInteger $ (fromRoman a) + (fromRoman b)
        
    (-) a b =
        fromInteger $ (fromRoman a) - (fromRoman b)
    
    (*) a b = 
        fromInteger $ (fromRoman a) * (fromRoman b)
        
    negate = id                 -- | Roman Symbols are always positive
    abs = id                    -- | Roman Symbols are always positive
    signum a = 1                -- | Roman Symbols are always positive

    fromInteger 0 =
        Nulla
    fromInteger 1 =
        I
    fromInteger 5 =
        V
    fromInteger 10 =
        X
    fromInteger 50 =
        L
    fromInteger 100 =
        X
    fromInteger 500 =
        D
    fromInteger 1000 =
        M
    fromInteger a 
        | a < 0 = 
            fromInteger $ negate a
        | otherwise = 
            error "Data.Roman: Won't work"
        
type RomanList =
    [RomanSymbol]
    
instance Roman RomanList where
    fromRoman (x:y:ys)
        | fromRoman x < fromRoman y =
            fromRoman y - fromRoman x + fromRoman ys
        | otherwise =
            fromRoman x + fromRoman (y:ys)

    fromRoman (x:xs) =
        fromRoman x + fromRoman xs

    fromRoman _ = 
        0
        
instance Num RomanList where
    (+) a b =
        fromInteger $ (fromRoman a) + (fromRoman b)
        
    (-) a b =
        fromInteger $ (fromRoman a) - (fromRoman b)
    
    (*) a b = 
        fromInteger $ (fromRoman a) * (fromRoman b)
        
    negate = id                 -- | Roman Symbols are always positive
    abs = id                    -- | Roman Symbols are always positive
    signum a = 1                -- | Roman Symbols are always positive

    fromInteger a
        | a >= 1000 =
            [M] ++ fromInteger (a - 1000)

        | a >= 900 =
            [C , M] ++ fromInteger (a - 900)

        | a >= 500 =
            [D] ++ fromInteger (a - 500)

        | a >= 400 =
            [C , D] ++ fromInteger (a - 400)

        | a >= 100 =
            [C] ++ fromInteger (a - 100)

        | a >= 90 =
            [X , C] ++ fromInteger (a - 90)

        | a >= 50 =
            [L] ++ fromInteger (a - 50)

        | a >= 40 =
            [X , L] ++ fromInteger (a - 40)

        | a >= 10 =
            [X] ++ fromInteger (a - 10)

        | a >= 9 =
            [I , X] ++ fromInteger (a - 9)

        | a >= 5 =
            [V] ++ fromInteger (a - 5)

        | a == 4 =
            [I , V]

        | a >= 1 =
            [I] ++ fromInteger (a - 1)

        | a == 0 =
            []

        | a < 0 =
            fromInteger (negate a)

        | otherwise =
            error "Data.Roman why?"