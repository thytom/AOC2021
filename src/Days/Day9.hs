module Days.Day9 where

import qualified TestT as T
import qualified Days.Day as D

import Data.List(sort, nub)

import Control.Monad.State.Lazy
import Control.Monad

day = D.Day { D.name = "Day 9"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day9_test.txt"   "15" "1134"
            , D.input     = T.mkAssertion "day9_actual.txt" "502" "1330560"}

parse :: String -> [[Int]]
parse = map (map (\x-> read [x] :: Int)) . lines

-- Return a point if it exists, [] if not
get_point_safe :: (Int, Int) -> [[Int]] -> [Int]
get_point_safe (x, y) xs = if all (==True) [x >= 0, x < length xs, y >= 0, y < length (head xs)]
                              then [xs!!x!!y]
                              else []

get_adjacents :: (Int, Int) -> [[Int]] -> (Int, [Int])
get_adjacents (x, y) xs = (head $ get_point_safe (x, y) xs
                                                , concat [ get_point_safe (x-1, y) xs 
                                                         , get_point_safe (x+1, y) xs
                                                         , get_point_safe (x, y-1) xs
                                                         , get_point_safe (x, y+1) xs])

get_adjacents' :: Point -> [[Int]] -> [Point]
get_adjacents' (x, y) xs = concat [f (x-1, y), f (x+1, y), f (x, y-1), f(x, y+1)]
          where f :: Point -> [Point]
                f p = if get_point_safe p xs /= [] then [p] else [] 


getminimumlocations :: [[Int]] -> [Point]
getminimumlocations hm = map (\(p, _) -> p) . filter (\(p, (x, xs)) -> x < minimum xs) $
        [((x, y), get_adjacents (x, y) hm) | x<-[0..length hm - 1], y <-[0..length (head hm) - 1]]

part1 :: String -> String
part1 input = show . sum . map (\(x, _) -> x+1) . filter (\(x, xs) -> x < minimum xs) $ 
        [get_adjacents (x, y) heightMap | x<-[0..length heightMap - 1], y <-[0..length (head heightMap) - 1]]
        where heightMap = parse input

{-
For each point in the heightMap:
        if it's a 9, or already seen, ignore it.
        take the list of basins, add 1 to the current basin.
        recursively check each of its adjacent neighbors
get the biggest 3
sum them
-}

type Point = (Int, Int)
type Points = [Point]

-- Give this function the lowest point and it recursively finds the whole basin.
getbsn :: [[Int]] -> Point -> State Points Points
getbsn hm p = if p_val /= 9
                 then do seen <- get
                         put (p:seen)
                         let unseenneighs = filter (not . (`elem` (seen))) neighbors
                         ns <- forM unseenneighs $ \n -> do
                                 seen <- get
                                 ps <- getbsn hm n
                                 return ps
                         return (p:concat ns)
                 else return []
        where p_val = head $ get_point_safe p hm
              neighbors = get_adjacents' p hm

part2 :: String -> String
part2 input = show . foldl (*) 1 . take 3 . reverse . sort $ 
        map (\x -> length . nub . fst $ runState (getbsn heightMap x) []) points
        where points = getminimumlocations heightMap
              heightMap = parse input
