module Main where

import Data.Roman

main = do  
    putStrLn "whats ur numbaa"  
    x <- getLine  
    putStrLn (show (fromInteger (read x ::Integer)::RomanNumeral)  )



