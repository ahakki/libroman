module Main where

import Data.Roman
import Text.Read ( readMaybe ) 

main = do  
    putStrLn "whats ur numbaa"  
    x <- getLine  
    let test = readMaybe x ::Maybe Integer
    if test == Nothing
    then putStrLn (show (fromRoman (read x ::RomanNumeral) ::Integer)  )
    else putStrLn (show (fromInteger (read x ::Integer) ::RomanNumeral))


