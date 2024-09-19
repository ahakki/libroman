{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Type.Roman where





-- Type class Roman
{- |
A type class for all types that can represent roman numerals
-}
class Roman r where
    {- |
    The Class Roman demands an implementation of fromRoman, to convert to an
    Integral Type
    -}
    fromRoman ::Integral b => r -> b
    {- |
    The Class Roman demands an implementation of fromRoman, to convert to from
    Integral Type
    -}
    toRoman ::Integral a => a -> r