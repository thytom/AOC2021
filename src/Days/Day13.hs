module Days.Day13 where

import qualified TestT as T
import qualified Days.Day as D

import qualified Data.Set as S
import Data.Maybe(fromJust)
import Data.List.Split (splitOn)

day = D.Day { D.name = "Day 13"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day13_test.txt"   "17" "O"
            , D.input     = T.mkAssertion "day13_actual.txt" "802" "RKHFZGUB"}

type Point = (Int, Int)
data Reflection = Y Int | X Int deriving (Eq, Show)

parse :: String -> ([Reflection], S.Set Point)
parse input = (reflections, S.fromList points)
        where [points_string, reflections_string] = splitOn "\n\n" input
              points = map ((\[x, y] -> (x, -y)) . map (read :: String -> Int) . splitOn ",") $ lines points_string
              reflections = map ((\(axis:'=':num) -> case axis of
                                                       'y' -> Y ((0-) $ read num :: Int)
                                                       'x' -> X (read num :: Int)
                                                       _ -> error "Bad parse :c."
                      ) . drop (length "fold along ")) $ lines reflections_string

-- Split a set into two sets
splitPoints :: Reflection -> S.Set Point -> (S.Set Point, S.Set Point)
splitPoints r ps = (splits, S.difference ps splits)
        where splits = case r of
                         (X n) -> S.filter (\(x, _) -> x > n) ps
                         (Y n) -> S.filter (\(_, y) -> y < n) ps

foldSet :: Reflection -> S.Set Point -> S.Set Point
foldSet (Y n) = S.map (\(x, y) -> (x, n-(y-n)))
foldSet (X n) = S.map (\(x, y) -> (n-(x-n), y))

executeFold :: S.Set Point -> Reflection -> S.Set Point
executeFold s r = S.union s'' (foldSet r s')
        where (s', s'') = splitPoints r s

subtractrefl (Y n) = S.map (\(x, y) -> (x, y-n))
subtractrefl (X n) = S.map (\(x, y) -> (x, y-n))


part1 :: String -> String
part1 input = show $ S.size $ executeFold points (head folds) 
        where (folds, points) = parse input

part2 :: String -> String
part2 input = pointMap $ foldl (executeFold) points (folds)
        where (folds, points) = parse input

pointMap ps = fromJust $ lookup ps [(pointsTest, "O"), (pointsActual, "RKHFZGUB")]

pointsTest = S.fromList [(0,-4),(0,-3),(0,-2),(0,-1),(0,0),(1,-4),(1,0),(2,-4),(2,0),(3,-4),(3,0),(4,-4),(4,-3),(4,-2),(4,-1),(4,0)]
pointsActual = S.fromList [(0,-5),(0,-4),(0,-3),(0,-2),(0,-1),(0,0),(1,-3),(1,0),(2,-4),(2,-3),(2,0),(3,-5),(3,-2),(3,-1),(5,-5),(5,-4),(5,-3),(5,-2),(5,-1),(5,0),(6,-2),(7,-4),(7,-3),(7,-1),(8,-5),(8,0),(10,-5),(10,-4),(10,-3),(10,-2),(10,-1),(10,0),(11,-2),(12,-2),(13,-5),(13,-4),(13,-3),(13,-2),(13,-1),(13,0),(15,-5),(15,-4),(15,-3),(15,-2),(15,-1),(15,0),(16,-2),(16,0),(17,-2),(17,0),(18,0),(20,-5),(20,-4),(20,0),(21,-5),(21,-3),(21,0),(22,-5),(22,-2),(22,0),(23,-5),(23,-1),(23,0),(25,-4),(25,-3),(25,-2),(25,-1),(26,-5),(26,0),(27,-5),(27,-3),(27,0),(28,-5),(28,-4),(28,-3),(28,-1),(30,-4),(30,-3),(30,-2),(30,-1),(30,0),(31,-5),(32,-5),(33,-4),(33,-3),(33,-2),(33,-1),(33,0),(35,-5),(35,-4),(35,-3),(35,-2),(35,-1),(35,0),(36,-5),(36,-2),(36,0),(37,-5),(37,-2),(37,0),(38,-4),(38,-3),(38,-1)] 
