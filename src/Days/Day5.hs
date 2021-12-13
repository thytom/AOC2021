module Days.Day5 where

import qualified TestT as T
import qualified Days.Day as D

import Data.List.Split (splitOn)
import Text.Regex
import Data.Maybe(fromJust)
import Data.HashTable.ST.Basic as H
import Control.Monad.ST
import Control.Monad

import qualified Data.Map as M

day = D.Day { D.name = "Day 5"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day5_test.txt" "5" "12"
            , D.input     = T.mkAssertion "day5_actual.txt" "5294" "21698"}

type Point = (Int, Int)
type Line = [Int]

type PointTable = H.HashTable Point Int

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
  | vertical p   = zip [x1, x1..] [min y1 y2..max y1 y2]
  | horizontal p = zip [min x1 x2..max x1 x2] [y1, y1..]
  | otherwise = zip [x1 , x1 + xcomp..x1 + xcomp * ((max x1 x2) - (min x1 x2))] 
                    [y1 , y1 + ycomp..y1 + ycomp * ((max y1 y2) - (min y1 y2))] 
        where xcomp = (x2 - x1) `div` (abs (x2 - x1))
              ycomp = (y2 - y1) `div` (abs (y2 - y1))

intersections :: [Point] -> Int
intersections ps = runST $ do ht1 <- newSized (length ps)
                              ht2 <- newSized (length ps)
                              Control.Monad.forM_ ps $ \p -> do
                                      seen <- H.lookup ht1 p
                                      case seen of
                                        Just _  -> insert ht2 p 0
                                        Nothing -> insert ht1 p 0
                              sz <- size ht2
                              return sz

part1 :: String -> String
part1 = show . intersections . concatMap points . filter (is_perpendicular) . parse

part2 :: String -> String
part2 = show . intersections . concatMap points . parse
