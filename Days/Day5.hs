module Days.Day5 where

import TestT
import Data.List.Split (splitOn)

tests = [ 
        Test {name="Day 5 Part 1 Test"  , input="day5_test.txt"  , subject=part1, assert=Just "5"},
        Test {name="Day 5 Part 1 Actual", input="day5_actual.txt", subject=part1, assert=Just "5294"},
        Test {name="Day 5 Part 2 Test"  , input="day5_test.txt"  , subject=part2, assert=Just "12"},
        Test {name="Day 5 Part 2 Actual", input="day5_actual.txt", subject=part2, assert=Just "21598"} 
        ]

type Point = (Int, Int)
type Line = (Point, Point)

type Surface = [Int]

parse :: String -> [Line]
parse = map (list2tuple . map (list2tuple . map (read :: String -> Int) . splitOn ",") . splitOn " -> ") .  lines
        where list2tuple = \[x, y] -> (x, y)

is_perpendicular :: Line -> Bool
is_perpendicular ((x1, y1), (x2, y2)) = any (==True) [x1 == x2, y1 == y2]

splitxys :: Line -> ([Int], [Int])
splitxys ((x1, y1), (x2, y2)) = ([x1, x2], [y1, y2])

tuplecombine :: ([a], [a]) -> ([a], [a]) -> ([a], [a])
tuplecombine (xs1, ys1) (xs2, ys2) = (xs1 ++ xs2, ys1 ++ ys2)

maxtup (xs, ys) = (maximum xs + 1, maximum ys + 1)

-- Get the total size of the surface being measured
find_range :: [Line] -> ((Int), (Int))
find_range ls = maxtup <$> foldl (tuplecombine) ([], []) $ map (splitxys) ls

point_in_line :: Point -> Line -> Bool
point_in_line (px, py) ((x1, y1), ((x2, y2))) = all (==True) [ in_range px x1 x2, in_range py y1 y2 , collinear_triangle_method (px, py) ((x1, y1), (x2, y2))] 
        where in_range n x y = n <= (max x y) && n >= (min x y)

collinear_triangle_method :: Point -> Line -> Bool
collinear_triangle_method (x1, y1) ((x2, y2), ((x3, y3))) = x1* (y2-y3) + x2* (y3-y1) + x3 *(y1-y2) == 0

generate_board :: (Int, Int) -> [Line] -> [Int]
generate_board (x_size, y_size) ls = [length $ filter (point_in_line (x, y)) ls | x <- [0..x_size] , y <- [0..y_size]]

part1 :: String -> String
part1 input = show . length . filter (> 1) . flip (generate_board) lines_list . find_range $ lines_list
        where lines_list = filter (is_perpendicular) $ parse input

part2 :: String -> String
part2 input = show . length . filter (> 1) . flip (generate_board) lines_list . find_range $ lines_list
        where lines_list = parse input
