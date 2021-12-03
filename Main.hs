import Tests
import TestT

import Day1.Day1 as Day1
import Day2.Day2 as Day2
import Day3.Day3 as Day3

tmap :: [Test]
tmap = concat [ Day1.tests
              , Day2.tests
              , Day3.tests ]

main :: IO ()
main = testall tmap
