{-# LANGUAGE BangPatterns #-}


module Main where

import Data.Roman
import Text.Read ( readMaybe ) 

main = do  
    ask1
  
  where   
    ask1 = do
        putStrLn "What's your first input, User?"  
        x <- getLine  
        let !test1 = readMaybe x :: Maybe Integer
        let !test2 = readMaybe x :: Maybe ExtendedRoman
        if test1 == Nothing && test2 == Nothing then
          do
            putStrLn $ "User input is unreadable!!" ++ (show x)
            main
        else if test1 == Nothing then
            ask2 (read x :: ExtendedRoman) 
        else  
            ask2 $ (fromIntegral (read x :: Integer) :: ExtendedRoman)
    ask2 a = do
        putStrLn "What's your second input, User"
        y <- getLine
        let test3 = readMaybe y :: Maybe Integer
        let test4 = readMaybe y :: Maybe ExtendedRoman
        if test3 == Nothing && test4 == Nothing then 
          do
            putStrLn "User input is unreadable!!"
            main
        else if test3 == Nothing
            then putStrLn (show (a + (read y ::ExtendedRoman))  )
        else putStrLn (show (a + (fromIntegral(read y :: Integer)::ExtendedRoman)))
        main

