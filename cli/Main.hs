module Main where

import Data.Roman
import Text.Read ( readMaybe ) 

main = do  
    putStrLn "What's your input, User?"  
    x <- getLine  
    let test1 = readMaybe x ::Maybe Integer
    let test2 = readMaybe x :: Maybe RomanNumeral
    if test1 == Nothing && test2 == Nothing
        then putStrLn "User input is unreadable!!"
    else if test1 == Nothing
        then putStrLn (show (fromRoman (read x ::RomanNumeral) ::Integer)  )
    else putStrLn (show (fromInteger (read x ::Integer) ::RomanNumeral))
    main

