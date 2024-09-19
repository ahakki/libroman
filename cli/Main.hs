module Main where

import Data.Roman (ExtendedRoman, RomanNumeral)
import Text.Read (readMaybe)

type OutputFormat = ExtendedRoman

main:: IO ()
main = do
  ask1
  where
    ask1 = do
      putStrLn "What's your first input, User?"
      x <- getLine
      case x of
        [] -> main
        _ -> do
          let test1 = readMaybe x :: Maybe Integer
          let test2 = readMaybe x :: Maybe OutputFormat
          case (test1, test2) of
            (Nothing, Nothing) -> do
              putStrLn $ "User input is unreadable!!" ++ show x
              main
            (Nothing, Just a) ->
              ask2 a
            (Just a, _) -> do
              print (fromIntegral a :: OutputFormat)
              ask2 (fromIntegral a :: OutputFormat)
    ask2:: OutputFormat -> IO ()
    ask2 a =
      putStrLn "Provide the second input." >>
      getLine >>= \y -> 
      case y of 
        [] -> 
          print a >>
          ask2 a
        _ -> 
          case (readMaybe y :: Maybe Integer , readMaybe y :: Maybe OutputFormat) of
            (Nothing, Nothing) -> 
              putStrLn ("User input is unreadable!!" ++ show y) >>
              ask2 a
            (Nothing, Just b) ->
              print (a + b) >>
              main
            (Just b, _) ->
              print (a + (fromIntegral b :: OutputFormat)) >>
              main
    -- ask2 a = do
    --   putStrLn "What's your second input, User"
    --   y <- getLine
    --   case y of
    --     [] -> do
    --       print a
    --       ask2 a
    --     _ -> do

    --       main
