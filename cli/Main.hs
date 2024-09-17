module Main where

import Data.Roman
import Text.Read (readMaybe)

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
          let test2 = readMaybe x :: Maybe ExtendedRoman
          case (test1, test2) of
            (Nothing, Nothing) -> do
              putStrLn $ "User input is unreadable!!" ++ show x
              main
            (Nothing, Just a) ->
              ask2 a
            (Just a, _) -> do
              print (fromIntegral a :: ExtendedRoman)
              ask2 (fromIntegral a :: ExtendedRoman)
    ask2:: ExtendedRoman -> IO ()
    ask2 a = do
      putStrLn "What's your second input, User"
      y <- getLine
      case y of
        [] -> do
          print a
          ask2 a
        _ -> do
          let test3 = readMaybe y :: Maybe Integer
          let test4 = readMaybe y :: Maybe ExtendedRoman
          case (test3, test4) of
            (Nothing, Nothing) -> do
              putStrLn $ "User input is unreadable!!" ++ show y
              main
            (Nothing, Just b) ->
              print (b + (read y :: ExtendedRoman))
            (Just b, _) -> do
              print (a + (fromIntegral b :: ExtendedRoman))
          main
