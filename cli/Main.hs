-- Main.hs
{- |
Module      :  $Header$
Description :  Roman Numerals
Copyright   :  (c) Alexander Hakki
License     :  BSD3

Maintainer  :  ahk@ahakki.xyz
Stability   :  experimental
Portability :  portable
-}

module Main where

import Data.Roman (ExtendedRoman, RomanNumeral)
import Text.Read (readMaybe)

type OutputFormat = ExtendedRoman

main :: IO ()
main = putStrLn "What's your first input, User?"
     >> getLine
     >>= \x ->
       case x of
         [] -> main
         _  -> case (readMaybe x :: Maybe Integer, readMaybe x :: Maybe OutputFormat) of
               (Nothing, Nothing) -> putStrLn ("User input is unreadable!!" ++ show x) 
                                  >> main
               (Nothing, Just a)  -> ask2 a
               (Just a, _)        -> print (fromIntegral a :: OutputFormat)
                                  >> ask2 (fromIntegral a :: OutputFormat)
  where
    ask2 :: OutputFormat -> IO ()
    ask2 a 
      = putStrLn "Provide the second input." 
      >> getLine 
      >>= \y 
      -> case y of 
         [] -> print a
            >> ask2 a
         _  -> case (readMaybe y :: Maybe Integer, readMaybe y :: Maybe OutputFormat) of
               (Nothing, Nothing) -> putStrLn ("User input is unreadable!!" ++ show y) 
                                 >> ask2 a
               (Nothing, Just b)  -> print (a + b)
                                 >> main
               (Just b, _)        -> print (a + (fromIntegral b :: OutputFormat))
                                 >> main
