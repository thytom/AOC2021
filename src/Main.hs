module Main where

import Tests
import TestT

import DayTests

import System.Environment

main :: IO ()
main = do 
        args <- getArgs
        case args of
          [] -> do putStrLn "Defaulting to current day..."
                   testall currentDay
          ["tests"]      -> do putStrLn "Running all tests..."
                               assertall allTests
          ["days"]       -> do putStrLn "Running all days..."
                               testall allRun
          ["everything"] -> do putStrLn "Running all tests..."
                               assertall allTests
                               putStrLn "Running all days..."
                               testall allRun
          ["current"]    -> do putStrLn "Running current day..."
                               testall currentDay
          _              -> putStrLn "Incorrect arguments. Valid arguments: tests, days, everything, current"
