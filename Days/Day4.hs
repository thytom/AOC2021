module Days.Day4 where

import TestT

import Data.List (transpose, sort)
import Data.List.Split (chunksOf, splitOn)
import Control.Monad.State.Lazy

tests = [ 
           Test {name="Day 4 Part 1 Test"  , input="day4_test.txt"  , subject=part1, assert=Just "4512"}
        ,  Test {name="Day 4 Part 1 Actual", input="day4_actual.txt", subject=part1, assert=Just "39902"}
        ,  Test {name="Day 4 Part 2 Test"  , input="day4_test.txt"  , subject=part2, assert=Just "1924"}
        ,  Test {name="Day 4 Part 2 Actual", input="day4_actual.txt", subject=part2, assert=Just "26936"} 
        ]

type Board = [[Int]]

-- The pool, the played numbers
type BingoState = ([Int], [Int])
-- The board that won and the number it got called on.
type BingoResults = (Board, Int)

rowfilled :: [Int] -> [Int] -> Bool
rowfilled ns = all (==True) . map (`elem` ns)

-- Checks if a bingo board is winning.
winningboard :: [Int] -> Board -> Bool
winningboard ns b = any (==True) . map (any (==True) . map (rowfilled ns)) $ [b, transpose b]

-- Get the sum of all unmarked tiles
unmarkedsum :: [Int] -> Board -> Int
unmarkedsum ns = sum . filter (not . (`elem` ns)) . concat

draws :: [Int] -> [[Int]]
draws ns = reverse . map (\(d, xs) -> drop d xs) $ zip [0..] $ replicate (length ns) (reverse ns)

-- After how many draws and which number does a board win
whenwin :: [Int] -> Board -> (Int, Int)
whenwin ns b = (\(n, (ds, _)) -> (n, ds)) . head . dropWhile (\(n, (ds, w)) -> w==False) . 
        zip [1..] . zip ns . map (\ns' -> winningboard ns' b) $ (draws ns)

parseboard :: [String] -> Board
parseboard = map (map (\x -> read x :: Int) . words)

parse :: String -> ([Int], [Board])
parse = (\(head:boards) -> (map (\x -> read x :: Int) . splitOn "," $ head,
                                map (parseboard) . chunksOf 5 $ boards
                             )) . filter (/= []) . lines 

generalbingo f ns = (\(_, n, b) -> n * unmarkedsum (n : takeWhile (/= n) ns) b) . 
                head . f . map (\b-> (\(x, y) -> (x, y, b)) $ whenwin ns b)

playbingo' = generalbingo (sort)
reversebingo' = generalbingo (reverse . sort)

part1 :: String -> String
part1 s = show . playbingo' ns $ boards
        where (ns, boards) = parse s

part2 :: String -> String
part2 s = show . reversebingo' ns $ boards
        where (ns, boards) = parse s
