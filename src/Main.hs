module Main where

import Tests
import TestT

import Days.Day1 as Day1
import Days.Day2 as Day2
import Days.Day3 as Day3
import Days.Day4 as Day4
import Days.Day5 as Day5
import Days.Day6 as Day6

unit_tests :: [Test]
unit_tests = concat [ Day1.tests
                    , Day2.tests
                    , Day3.tests 
                    , Day4.tests 
                    , Day5.tests 
                    ]

all_days :: [Test]
all_days = concat [ Day1.run
                      , Day2.run
                      , Day3.run 
                      , Day4.run 
                      , Day5.run 
                      ]

latest :: [Test]
latest = Day6.all

main :: IO ()
main = do putStrLn "Running unit tests..."
          testall unit_tests
          putStrLn "Running completed days..."
          testall all_days
          putStrLn "Running current day..."
          testall latest
          putStrLn "Done."
