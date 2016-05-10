-- Roman-hs

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
    (        
    ) where
     
import Roman.Encode
        
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
        , Read
        , Show
        , Enum
        )
             
type RomanNumeral =
    [RomanSymbol]

instance Num RomanSymbol where
    (+) a b = 
        read . toRoman $ toInteger a + toInteger b

instance Real RomanSymbol where
    toRational _ = 
        undefined
    
instance Integral RomanSymbol where
    toInteger I =
        1 

    toInteger V =
        5

    toInteger X =
        10

    toInteger L =
        50

    toInteger C =
        100

    toInteger D =
        500

    toInteger M =
        1000

    toInteger _ =
        0