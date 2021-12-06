module Days.Day6 where

import TestT

import Data.List (nub, sort)
import Data.List.Split (splitOn)

tests = [ Test {name="Day 6 Part 1", input="day6_test.txt"  , subject=part1, assert=Just "5934"}
        , Test {name="Day 6 Part 2", input="day6_test.txt"  , subject=part2, assert=Just "26984457539"} ]
run   = [ Test {name="Day 6 Part 1", input="day6_actual.txt", subject=part1, assert=Just "351188"}
        , Test {name="Day 6 Part 2", input="day6_actual.txt", subject=part2, assert=Just "1595779846729"} ]
all = tests ++ run

-- Convert a list into frequencies
occurences :: (Eq a, Ord a) => [a] -> [(a, Integer)]
occurences xs = sort $ zip (nub xs) (map (\x -> fromIntegral . length . Prelude.filter (==x) $ xs)  (nub xs))

parse :: String -> [Integer]
parse = map read . splitOn ","

-- Max length of list is 9, so should make sure that's the amt of elems
padlist :: [Integer] -> [Integer]
padlist xs | length xs < 9 = padlist (xs ++ [0])
           | otherwise = xs

bufferstart :: Int -> [Integer] -> [Integer]
bufferstart n xs = replicate n 0 ++ xs

-- Take off head (0 values). Add this number to "6", then add "8"
lanterncycle :: [Integer] -> [Integer]
lanterncycle (n:ns) = concat [take 6 ns, [(ns !! 6) + n], drop 7 ns, [n]]

iteratecycle input n = iterate (lanterncycle) ns !! n
        where ns = padlist . bufferstart smallest . map snd $ freqs
              smallest = fromIntegral.  fst . head $ freqs
              freqs = occurences . parse $ input


part1 :: String -> String
part1 input = show . sum$ iteratecycle input 80

part2 :: String -> String
part2 input = show . sum $ iteratecycle input 256
