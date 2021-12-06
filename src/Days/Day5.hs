module Days.Day5 where

import TestT
import Data.List.Split (splitOn)
import Data.Set (Set, insert, empty, member, size)
import Text.Regex
import Data.Maybe(fromJust)

tests = [ 
          Test {name="Day 5 Part 1"  , input="day5_test.txt"  , subject=part1, assert=Just "5"}
        , Test {name="Day 5 Part 2"  , input="day5_test.txt"  , subject=part2, assert=Just "12"}
        ]

run = [
        Test {name="Day 5 Part 1", input="day5_actual.txt", subject=part1, assert=Just "5294"}
      , Test {name="Day 5 Part 2", input="day5_actual.txt", subject=part2, assert=Just "21698"} 
      ]

type Point = [Int]
type Line = [Int]

mtc = mkRegex "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)"

parse :: String -> [Line]
parse = map (map (read :: String -> Int) . fromJust . matchRegex mtc) . lines

is_perpendicular :: Line -> Bool
is_perpendicular p = any (==True) [vertical p, horizontal p]

vertical :: Line -> Bool
vertical [x1, _, x2, _] = x1 == x2

horizontal :: Line -> Bool
horizontal [_, y1, _, y2] = y1 == y2

points :: Line -> [Point]
points p@[x1, y1, x2, y2]
  | vertical p   = [[x1, y] | y<-[min y1 y2..max y1 y2]]
  | horizontal p = [[x, y1] | x <- [min x1 x2..max x1 x2]]
  | otherwise = [[x1 + n*xcomp, y1 + n*ycomp]| n <- [0..(max x1 x2)-(min x1 x2)]]
        where xcomp = (x2 - x1) `div` (abs (x2 - x1))
              ycomp = (y2 - y1) `div` (abs (y2 - y1))

insert_ :: (Set Point, Set Point) -> Point -> (Set Point, Set Point)
insert_ (s1, s2) p
  | member p s1 = (s1, insert p s2)
  | otherwise = (insert p s1, s2)

intersections :: [Line] -> Int
intersections = size . snd . foldl (insert_) (empty, empty) . concatMap points

part1 :: String -> String
part1 = show . intersections . filter (is_perpendicular) . parse

part2 :: String -> String
part2 = show . intersections . parse
