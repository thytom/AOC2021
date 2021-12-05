module Days.Day5 where

import TestT
import Data.List.Split (splitOn)
import Data.Map as DM (elems, Map, insertWith', empty)
import Data.Map.Strict(insertWith)

tests = [ 
        Test {name="Day 5 Part 1 Test"  , input="day5_test.txt"  , subject=part1, assert=Just "5"},
        Test {name="Day 5 Part 1 Actual", input="day5_actual.txt", subject=part1, assert=Just "5294"},
        Test {name="Day 5 Part 2 Test"  , input="day5_test.txt"  , subject=part2, assert=Just "12"},
        Test {name="Day 5 Part 2 Actual", input="day5_actual.txt", subject=part2, assert=Just "21698"} 
        ]

type Point = (Int, Int)
type Line = (Point, Point)

type PointMap = Map Point Int

parse :: String -> [Line]
parse = map (list2tuple . map (list2tuple . map (read :: String -> Int) . splitOn ",") . splitOn " -> ") .  lines
        where list2tuple = \[x, y] -> (x, y)

is_perpendicular :: Line -> Bool
is_perpendicular ((x1, y1), (x2, y2)) = any (==True) [x1 == x2, y1 == y2]

-- insertWith p 1 map -- increment counter

points :: Line -> [Point]
points p@((x1, y1), (x2, y2)) 
  | is_perpendicular p = [(x, y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]]
  | otherwise = [( if x1 > x2 then x1 - n else x1 + n
                 , if y1 > y2 then y1 - n else y1 + n) | n <- [0..(max x1 x2)-(min x1 x2)]]

intersections :: [Point] -> PointMap
intersections = Prelude.foldl (\pm p -> insertWith (+) p 1 pm) DM.empty

part1 :: String -> String
part1 input = show . length . filter (>1) . DM.elems . intersections . concatMap (points) $ lines_list
        where lines_list = filter (is_perpendicular) $ parse input

part2 :: String -> String
part2 input = show . length . filter (>1) . DM.elems . intersections . concatMap (points) $ lines_list
        where lines_list = parse input
