module Day1.Day1 where

import TestT
tests = [ Test {name = "Day 1 Part 1 Test"  , input = "day1_test.txt"   , subject = part1, assert = Just "7"} 
        , Test {name = "Day 1 Part 1 Actual", input = "day1_actual.txt" , subject = part1, assert = Just "1681"}
        , Test {name = "Day 1 Part 2 Test"  , input = "day1_test.txt"   , subject = part2, assert = Just "5"}
        , Test {name = "Day 1 Part 2 Actual", input = "day1_actual.txt" , subject = part2, assert = Just "1704"} ]

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
