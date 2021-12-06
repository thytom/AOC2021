module Main where

import Tests
import TestT

import Days.Day1 as Day1
import Days.Day2 as Day2
import Days.Day3 as Day3
import Days.Day4 as Day4
import Days.Day5 as Day5
import Days.Day6 as Day6
import Days.Day7 as Day7
-- import Days.Day8 as Day8
-- import Days.Day9 as Day9
-- import Days.Day10 as Day10
-- import Days.Day11 as Day11
-- import Days.Day12 as Day12
-- import Days.Day13 as Day13
-- import Days.Day14 as Day14
-- import Days.Day15 as Day15
-- import Days.Day16 as Day16
-- import Days.Day17 as Day17
-- import Days.Day18 as Day18
-- import Days.Day19 as Day19
-- import Days.Day20 as Day20
-- import Days.Day21 as Day21
-- import Days.Day22 as Day22
-- import Days.Day23 as Day23
-- import Days.Day24 as Day24
-- import Days.Day25 as Day25

unit_tests :: [Test]
unit_tests = concat [ Day1.tests
                    , Day2.tests
                    , Day3.tests 
                    , Day4.tests 
                    , Day5.tests 
                    , Day6.tests
                    ]

all_days :: [Test]
all_days = concat [ Day1.run 
                  , Day2.run 
                  , Day3.run 
                  , Day4.run 
                  , Day5.run 
                  , Day6.run 
                  ]

latest :: [Test]
latest = Day7.all

main :: IO ()
main = do 
        putStrLn "Running unit tests..." 
        testall unit_tests
        putStrLn "Running completed days..."
        testall all_days
        putStrLn "Running current day..."
        testall latest
        putStrLn "Done."
