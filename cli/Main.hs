{-# LANGUAGE BangPatterns #-}


module Main where

import Data.Roman
import Text.Read ( readMaybe ) 

main = do  
    ask1
  
  where   
    ask1 = do
        putStrLn "What's your first input, User?"  
        !x <- getLine  
        case x of
          [] -> main
          _ -> do
            let !test1 = readMaybe x :: Maybe Integer
            let !test2 = readMaybe x :: Maybe ExtendedRoman
            case (test1, test2) of
              (Nothing, Nothing) -> do
                putStrLn $ "User input is unreadable!!" ++ (show x)
                main
              (Nothing, Just a) ->
                ask2 a 
              (Just a, _) -> do
                putStrLn $ show (fromIntegral a ::ExtendedRoman)
                ask2 (fromIntegral a ::ExtendedRoman)
    ask2 a = do
        putStrLn "What's your second input, User"
        !y <- getLine
        case y of 
          [] -> do 
            putStrLn (show a)
            ask2 a
          _  -> do
            let !test3 = readMaybe y :: Maybe Integer
            let !test4 = readMaybe y :: Maybe ExtendedRoman
            if test3 == Nothing && test4 == Nothing then 
              do
                putStrLn "User input is unreadable!!"
                main
            else if test3 == Nothing
                then putStrLn (show (a + (read y ::ExtendedRoman))  )
            else putStrLn (show (a + (fromIntegral(read y :: Integer)::ExtendedRoman)))
            main

