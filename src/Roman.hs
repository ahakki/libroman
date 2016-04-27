module Roman
    ( toRoman
    ) where

toRoman a
    | a >= 1000 =
        "M" ++ toRoman (a - 1000)

    | a >= 500 =
        "D" ++ toRoman (a - 500)

    | a >= 100 =
        "C" ++ toRoman (a - 100)

    | a >= 50 =
        "L" ++ toRoman (a - 50)

    | a >= 10 =
        "X" ++ toRoman (a - 10)

    | a >= 5 =
        "V" ++ toRoman (a - 5)

    | a >= 1 =
        "I" ++ toRoman (a - 1)

    | a == 0 =
        ""
