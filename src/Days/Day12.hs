module Days.Day12 where

import qualified TestT as T
import qualified Days.Day as D

import Data.Char (isUpper, isLower)
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.List (nub)

import Control.Monad.State.Lazy
import Control.Monad

day = D.Day { D.name = "Day 12"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day12_test.txt"   "10" "36"
            , D.input     = T.mkAssertion "day12_actual.txt" "3230" "83475"}

data Node k = START | END | Big k | Small k deriving (Eq, Show)
type Edge = (Node String, Node String)
type Graph = ([Node String], [Edge])
type Path = [Node String]

parse :: String -> Graph
parse input = (nodes, edges)
        where nodes = map (readnode) . nub . splitOn "-" . intercalate "-" . lines $ input
              edges = filter ((/=START).snd) . concat . map (combs . map (readnode) . splitOn "-") . lines $ input

combs :: [a] -> [(a, a)]
combs [] = []
combs [x] = []
combs (x:xs) = combs xs ++ concat [[(x, x'), (x', x)] | x' <- xs ]

readnode :: String -> Node String
readnode "start" = START
readnode "end" = END
readnode xs 
  | all (isUpper) xs = Big xs
  | all (isLower) xs = Small xs

isSmall :: Node a -> Bool
isSmall (Small a) = True
isSmall _         = False

countPaths :: Node String -> Graph -> [Node String] -> Int
countPaths n grph vst = sum $ countps n grph vst

countPaths' :: Node String -> Graph -> [Node String] -> Int
countPaths' n grph vst = sum $ countps' n grph vst False

countps :: Node String -> Graph -> [Node String] -> [Int]
countps END _ _ = [1]
countps n grph@(vs, es) vst = concatMap (\e -> countps e grph (if isSmall n then n:vst else vst)) fromHere
       where fromHere = map (snd) . filter (\(n',x)-> n == n' && not (x `elem` vst)) $ es -- Where can we get to from here?

-- Exclude large nums from the list. Eliminating any edges that result in going back to START should also prevent hat.
countps' :: Node String -> Graph -> [Node String] -> Bool -> [Int]
countps' END _ _ _ = [1]
countps' n grph@(vs, es) vst doubleUsed = concatMap (\e -> countps' e grph (if isSmall n then n:vst else vst) (doubleUsed || e `elem` vst)) fromHere
        where fromHere 
                | doubleUsed = map (snd) . filter (\(n',x)->n'==n && not (x `elem` vst)) $ es -- Where can we get to from here?
                | otherwise  = map (snd) . filter (\(n',x)->n'==n) $ es

part1 :: String -> String
part1 input = show $ countPaths START grph []
        where grph = parse input

part2 :: String -> String
part2 input = show $ countPaths' START grph []
        where grph = parse input
