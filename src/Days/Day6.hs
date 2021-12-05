module Days.Day6 where

import TestT

tests = [ 
          Test {name="Day 6 Part 1"  , input="day6_test.txt"  , subject=part1, assert=Just "<++>"}
        , Test {name="Day 6 Part 2"  , input="day6_test.txt"  , subject=part2, assert=Just "<++>"}
        ]

run = [
          Test {name="Day 6 Part 1", input="day6_actual.txt", subject=part1, assert=Just "<++>"}
        , Test {name="Day 6 Part 2", input="day6_actual.txt", subject=part2, assert=Just "<++>"} 
      ]

all = tests ++ run


part1 :: String -> String
part1 input = "incomplete."

part2 :: String -> String
part2 input = "incomplete."
