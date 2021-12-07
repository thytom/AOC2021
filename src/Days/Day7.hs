module Days.Day7 where

import TestT

import Data.List.Split (splitOn)
import Data.List (sort)

tests = [ Test {name="Day 7 Part 1", input="day7_test.txt"  , subject=part1, assert=Just "37"}
        , Test {name="Day 7 Part 2", input="day7_test.txt"  , subject=part2, assert=Just "<++>"} ]
run   = [ Test {name="Day 7 Part 1", input="day7_actual.txt", subject=part1, assert=Just "<++>"}
        , Test {name="Day 7 Part 2", input="day7_actual.txt", subject=part2, assert=Just "<++>"} ]
all = tests ++ run

parse :: String -> [Int]
parse = map (read) . splitOn "," . head . lines 

range :: [Int] -> (Int, Int)
range ns = (minimum ns, maximum ns)

diffs :: [Int] -> Int -> [Int]
diffs ns n = map (abs . (n-)) ns

diffs' :: [Int] -> Int -> [Int]
diffs' ns n = map ((\x->sum [1..x]) . abs . (n-)) ns

best_position :: ([Int] -> Int -> [Int]) -> [Int] -> Int
best_position diff_f ns = head . sort $ [sum $ diff_f ns n | n <- [mn..mx]]
        where (mn, mx) = range ns

part1 :: String -> String
part1 = show . best_position diffs . parse

part2 :: String -> String
part2 = show . best_position diffs' . parse
