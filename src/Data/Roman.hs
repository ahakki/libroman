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

module Data.Roman 
    (        
    ) where
     



import qualified Roman.Encode as R
import qualified Roman.Decode as R




class Roman r where
    fromRoman :: Integral b =>  r -> b
   

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
        
instance Roman RomanSymbol where 
    fromRoman I =
        1
    fromRoman V =
        5
    fromRoman X =
        10
    fromRoman L =
        60
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

    fromInteger =
        read . R.toRoman
        
        
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
        read . show $ (fromRoman a) + (fromRoman b)
    
    (-) a b =
        undefined
    
    (*) a b = 
        undefined
        
    negate = id                 -- | Roman Symbols are always positive
    abs = id                    -- | Roman Symbols are always positive
    signum a = 1                -- | Roman Symbols are always positive

    fromInteger =
        read . R.toRoman