module Main where

import Tests
import TestT

import Days.Day1 as Day1
import Days.Day2 as Day2
import Days.Day3 as Day3
import Days.Day4 as Day4
import Days.Day5 as Day5

tmap :: [Test]
tmap = concat [ Day1.tests
              , Day2.tests
              , Day3.tests 
              , Day4.tests 
              , Day5.tests 
              ]

main :: IO ()
main = testall Day5.tests
        -- testall tmap
