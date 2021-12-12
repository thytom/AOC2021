module Days.Day12 where

import qualified TestT as T
import qualified Days.Day as D

import Data.Char (isUpper, isLower)
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.List (nub)
import Data.Maybe (fromJust)

import qualified Data.HashMap.Strict as HM

import Control.Monad.State.Lazy
import Control.Monad

day = D.Day { D.name = "Day 12"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day12_test.txt"   "10" "36"
            , D.input     = T.mkAssertion "day12_actual.txt" "3230" "83475"}

type Node = String
type Graph = HM.HashMap (Node) [Node]

parse :: String -> Graph
parse input = HM.fromList $ map (\n -> (n, fromHere n)) nodes
        where nodes = nub . splitOn "-" . intercalate "-" . lines $ input
              edges = filter ((/="start").snd) . concat . map (combs . splitOn "-") . lines $ input
              fromHere n = map (snd) . filter ((==n) . fst) $ edges

combs :: [a] -> [(a, a)]
combs [] = []
combs [x] = []
combs (x:xs) = combs xs ++ concat [[(x, x'), (x', x)] | x' <- xs ]

isSmall :: Node -> Bool
isSmall = all (isLower)

countPaths :: Node -> Bool -> Graph -> Int
countPaths n doublecaves = sum . countps n [] doublecaves

-- Exclude large nums from the list. Eliminating any edges that result in going back to START should also prevent hat.
countps :: Node -> [Node] -> Bool -> Graph -> [Int]
countps "end" _ _ _ = [1]
countps n vst doubleUsed grph = concatMap (\e -> countps e (if isSmall n then n:vst else vst) (doubleUsed || e `elem` vst) grph) fromHere
        where fromHere 
                | doubleUsed = filter (\x -> not (x `elem` vst)) $ fromJust $ HM.lookup n grph -- Where can we get to from here?
                | otherwise  = fromJust $ HM.lookup n grph

part1 :: String -> String
part1 input = show . countPaths "start" True $ parse input

part2 :: String -> String
part2 input = show . countPaths "start" False $ parse input
