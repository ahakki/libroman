module Roman
    ( toRoman
    ) where

data Roman = I | V | X | L | C | D | M
    deriving (Show, Eq, Ord)


romanToChar :: Roman -> Char

romanToChar I =
    'I'
romanToChar V =
    'V'
romanToChar X =
    'X'
romanToChar L =
    'L'
romanToChar C =
    'C'
romanToChar D =
    'D'
romanToChar M =
    'M'

toRoman :: Int -> String

toRoman 0 =
    ""
toRoman arabic =
    replicate arabic (romanToChar I)
