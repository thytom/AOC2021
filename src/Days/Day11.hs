module Days.Day11 where

import qualified TestT as T
import qualified Days.Day as D

import Data.List.Split (splitOn)
import Data.Char (digitToInt)

import Control.Monad.State.Lazy
import Control.Monad (forM_)

import Data.Maybe (fromJust)

import qualified Data.Map as M

day = D.Day { D.name = "Day 11"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day11_test.txt"   "1656" "195"
            , D.input     = T.mkAssertion "day11_actual.txt" "1773" "494"}

parse :: String -> [[Int]]
parse = map (map (digitToInt)) . lines

type Board = M.Map (Int, Int) (Int, Int)

toboard :: [[Int]] -> Board
toboard b = foldl (\m (k, v) -> M.insert k v m) M.empty [((x, y), (b!!y!!x, 0)) | x <- [0..(length $ head b)-1], y <- [0..length b-1]]

increment :: (Int, Int) -> Board -> Board
increment p b = M.adjust (\(c, fs) -> (c+1, fs)) p b

increment_nozero p b = M.adjust (\(c, fs) -> (if c == 0 then c else c+1, fs)) p b

flash :: (Int, Int) -> State Board Board
flash p = do b <- get
             put (M.adjust (\(_, fs) -> (0, fs+1)) p b)
             forM_ (getneighbors p b) $ \n -> do
                     b <- get
                     put $ increment_nozero n b
             b <- get
             return b

-- Needs the board so it can filter neighbors that don't exist
getneighbors :: (Int, Int) -> Board -> [(Int, Int)]
getneighbors (x, y) b = filter (\k -> M.member k b) [ (x-1, y-1)
                                                    , (x-1, y)
                                                    , (x-1, y+1)
                                                    , (x, y-1)
                                                    , (x, y+1)
                                                    , (x+1, y-1)
                                                    , (x+1, y)
                                                    , (x+1, y+1)]
                                                    


-- Flat board increment.
-- Go through again, find numbers larger than 9, re-increment and re-set these numbers.
--      Filtering out 0 values here avoids re-flashing octopuses.
-- Repeat this process until no numbers larger than 9 exist.

iterate :: Board -> Board
iterate b = evalState (iterate_) b

iterate_ :: State Board Board
iterate_ = do board <- get
              let points = map (\(k, v) -> k) $ M.toList board
              forM_ points $ \p -> do
                      board <- get
                      put (increment p board)
              board <- get
              let nb = until (\x -> onlynines x == []) (evalState (donines)) board
              -- let nb = evalState (donines) board
              return nb
                      where onlynines bd = filter (\(k, (c, v)) -> c>9) $ M.toList bd
                            
donines :: State Board Board
donines = do b <- get
             let nines = M.keys $ M.filter (\(c, _) -> c > 9) b
             forM_ nines $ \p -> do
                     flash p
             b <- get
             return b

count :: Board -> Int
count = sum . map (\(_, (_, f)) -> f) . M.toList 

nicevis :: Board -> [((Int, Int), Int)]
nicevis = map (\(k, (c, _)) -> (k, c)) . M.toList

part1 :: String -> String
part1 input = show $ count $ it_inf !! 100
        where octmap = toboard . parse $ input
              it_inf = Prelude.iterate (Days.Day11.iterate) octmap

countallzeros :: Board -> Int
countallzeros b = length . filter (==0) . map (\(k, (c, _)) -> c) $ M.toList b

part2 :: String -> String
part2 input = show . fst . head . filter (\(n, b) -> countallzeros b == targetsize) $ zip [0..] it_inf
        where octmap = toboard . parse $ input
              it_inf = Prelude.iterate (Days.Day11.iterate) octmap
              targetsize = length $ M.toList octmap
