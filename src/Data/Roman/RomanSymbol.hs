-- RomanSymbol.hs

module Data.Roman.RomanSymbol
    ( RomanSymbol ( .. )
    ) where

import Data.Roman
import Data.Char

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
        -- , Read
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
            error "Data.Roman: Won't work"
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