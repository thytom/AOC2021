module Days.Day12 where

import qualified TestT as T
import qualified Days.Day as D

import Data.Char (isUpper, isLower)
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.List (nub)
import Data.Maybe (fromJust)

import Control.Monad.State.Lazy
import Control.Monad

day = D.Day { D.name = "Day 12"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day12_test.txt"   "10" "36"
            , D.input     = T.mkAssertion "day12_actual.txt" "3230" "83475"}

data Node k = START | END | Big k | Small k deriving (Eq, Show)
type Edge = (Node String, Node String)
type Graph = [(Node String, [Node String])]
type Path = [Node String]

parse :: String -> Graph
parse input = map (\n -> (n, fromHere n)) nodes
        where nodes = map (readnode) . nub . splitOn "-" . intercalate "-" . lines $ input
              edges = filter ((/=START).snd) . concat . map (combs . map (readnode) . splitOn "-") . lines $ input
              fromHere n = map (snd) . filter ((==n) . fst) $ edges

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

countPaths :: Node String -> Bool -> Graph -> Int
countPaths n doublecaves = sum . countps n [] doublecaves

-- Exclude large nums from the list. Eliminating any edges that result in going back to START should also prevent hat.
countps :: Node String -> [Node String] -> Bool -> Graph -> [Int]
countps END _ _ _ = [1]
countps n vst doubleUsed grph = concatMap (\e -> countps e (if isSmall n then n:vst else vst) (doubleUsed || e `elem` vst) grph) fromHere
        where fromHere 
                | doubleUsed = filter (\x -> not (x `elem` vst)) $ fromJust $ lookup n grph -- Where can we get to from here?
                | otherwise  = fromJust $ lookup n grph

part1 :: String -> String
part1 input = show . countPaths START True $ parse input

part2 :: String -> String
part2 input = show . countPaths START False $ parse input
