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
              edges = concat . map (combs . map (readnode) . splitOn "-") . lines $ input

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
countPaths n grph vst = snd $ runState (countps n grph vst) 0

countPaths' :: Node String -> Graph -> [Node String] -> Int
countPaths' n grph vst = snd $ runState (countps' n grph vst False) 0

countps :: Node String -> Graph -> [Node String] -> State Int ()
countps END _ _ = do count <- get
                     put (count +1)
countps n grph@(vs, es) vst = forM_ fromHere $ \e -> countps e grph (n:vst)
        where fromHere = map (snd) . filter (\(_,x)->x /= START && not (isSmall x && x `elem` vst)) . filter ((==n) . fst ) $ es -- Where can we get to from here?

countps' :: Node String -> Graph -> [Node String] -> Bool -> State Int ()
countps' END _ _ _ = do count <- get
                        put (count +1)
countps' n grph@(vs, es) vst doubleUsed = forM_ fromHere $ \e -> countps' e grph (n:vst) (doubleUsed || if isSmall e then e `elem` vst else False)
        where fromHere = map (snd) . filter (\(_,x)->x /= START && not (isSmall x && x `elem` vst && doubleUsed)) . filter ((==n) . fst ) $ es -- Where can we get to from here?

part1 :: String -> String
part1 input = show $ countPaths START grph []
        where grph = parse input

part2 :: String -> String
part2 input = show $ countPaths' START grph []
        where grph = parse input

test :: IO ()
test = do input <- readFile "inputs/day12_actual.txt"
          putStrLn $ part2 input
