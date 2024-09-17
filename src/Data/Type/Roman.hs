module Data.Type.Roman where





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
