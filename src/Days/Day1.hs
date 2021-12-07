module Days.Day1 (day, tests, run, Days.Day1.all) where

import qualified TestT as T
import qualified Days.Day as D

tests = [ T.Test {T.name = "Day 1 Part 1", T.input = "day1_test.txt"  , T.subject = part1, T.assert = Just "7"} 
        , T.Test {T.name = "Day 1 Part 2", T.input = "day1_test.txt"  , T.subject = part2, T.assert = Just "5"} ]
run   = [ T.Test {T.name = "Day 1 Part 1", T.input = "day1_actual.txt", T.subject = part1, T.assert = Just "1681"}
        , T.Test {T.name = "Day 1 Part 2", T.input = "day1_actual.txt", T.subject = part2, T.assert = Just "1704"} ]
all = tests ++ run

day = D.Day { D.name = "Day 1"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day1_test.txt" "7" "5"
            , D.input     = T.mkAssertion "day1_actual.txt" "1681" "1704"}

part1 :: String -> String
part1 = p1 . parse

part2 :: String -> String
part2 = p1 . (flip sumtrip) [] . parse

p1 :: [Int] -> String
p1 x = show $ length $ filter (>0) $ zipWith (-) (tail x) x

parse :: String -> [Int]
parse = map (\x -> read x :: Int) . lines

sumtrip :: [Int] -> [Int] -> [Int]
sumtrip (b:c:[])   ys = reverse ys
sumtrip (a:b:c:xs) ys = sumtrip (b:c:xs) (a+b+c:ys)
