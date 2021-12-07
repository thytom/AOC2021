module Main where

import Tests
import TestT

import DayTests

main :: IO ()
main = do 
        putStrLn "Running all completed day tests..."
        testall allTests
        putStrLn "Running all completed days..."
        testall allRun
        putStrLn "Running current day..."
        testall currentDay
        putStrLn "Done."
