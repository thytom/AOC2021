module DayTests where

import Tests

import qualified Days.Day1 as Day1
import qualified Days.Day2 as Day2
import qualified Days.Day3 as Day3
import qualified Days.Day4 as Day4
import qualified Days.Day5 as Day5
import qualified Days.Day6 as Day6
import qualified Days.Day7 as Day7
import qualified Days.Day8 as Day8
import qualified Days.Day9 as Day9
import qualified Days.Day10 as Day10
import qualified Days.Day11 as Day11
import qualified Days.Day12 as Day12
import qualified Days.Day13 as Day13

days = [ Day1.day
       , Day2.day
       , Day3.day
       , Day4.day
       , Day5.day
       , Day6.day
       , Day7.day
       , Day8.day
       , Day9.day
       , Day10.day
       , Day11.day
       , Day12.day
       , Day13.day
       ]

allTests = concatMap (take 2 . testsFrom) days

allRun = concatMap (drop 2 . testsFrom) days

currentDay = testsFrom $ last days

dayTests = concatMap (testsFrom) days
