module Days.Day8 where

import qualified TestT as T
import qualified Days.Day as D

day = D.Day { D.name = "Day 8"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day8_test.txt"   "<++>" "<++>"
            , D.input     = T.mkAssertion "day8_actual.txt" "<++>" "<++>"}

part1 :: String -> String
part1 input = "incomplete."

part2 :: String -> String
part2 input = "incomplete."
