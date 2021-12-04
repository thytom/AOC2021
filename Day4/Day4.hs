module Day4.Day4 where

import TestT

import Data.List (transpose)
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

unmarkedsum :: [Int] -> Board -> Int
unmarkedsum ns = sum . filter (not . (`elem` ns)) . concat

playbingo :: [Board] -> State BingoState BingoResults
playbingo bs = do (pool, played) <- get
                  let p = head pool
                  let w = filter (winningboard (p:played)) bs
                  put (tail pool, (p:played))
                  if length w > 0
                     then return (head w, p)
                     else playbingo bs

-- Set subtraction
without :: Eq a => [a] -> [a] -> [a]
without as bs = [b | b<-bs, not $ elem b as]

-- Last board to win.
reversebingo :: [Board] -> State BingoState BingoResults
reversebingo bs = do (pool, played) <- get
                     let p = head pool
                     let w = filter (winningboard (p:played)) bs
                     put (tail pool, (p:played))
                     -- If we have a winner and there's only one card left in play
                     if length w > 0 && length bs == 1
                        then return (head w, p)
                        else reversebingo (without w bs)

result = (\((winner, n), (_, ns)) -> n * unmarkedsum ns winner)

parseboard :: [String] -> Board
parseboard = map (map (\x -> read x :: Int) . words)

parse :: String -> ([Int], [Board])
parse = (\(head:boards) -> (map (\x -> read x :: Int) . splitOn "," $ head,
                                map (parseboard) . chunksOf 5 $ boards
                             )) . filter (/= []) . lines 

part1 :: String -> String
part1 s = show $ result $ runState (playbingo boards) (ns, [])
        where (ns, boards) = parse s

part2 :: String -> String
part2 s = show $ result $ runState (reversebingo boards) (ns, [])
        where (ns, boards) = parse s
