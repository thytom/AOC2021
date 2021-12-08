module Days.Day8 where

import qualified TestT as T
import qualified Days.Day as D

import Data.List.Split (splitOn)
import Data.List (nub, intersect, sort, (\\))

import Data.Maybe (fromJust)
import qualified Data.Map as M

day = D.Day { D.name = "Day 8"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day8_test.txt"   "26" "61229"
            , D.input     = T.mkAssertion "day8_actual.txt" "367" "974512"}

parse :: String -> [[[String]]]
parse str = [splitOn ["|"] . map sort . words $ l | l <- lines str]

easydigits :: String -> Int
easydigits s
  | length s == 2 = 1
  | length s == 4 = 4
  | length s == 3 = 7
  | length s == 7 = 8
  | otherwise = -1
        where l = length s

part1 :: String -> String
part1 input = show . length . filter (>0) . concatMap (map (easydigits) . last) . parse $ input

-- Get the list of ones we definitely know.
buildmap :: [String] -> M.Map Int String -> M.Map Int String
buildmap [] mp = mp
buildmap (s:ss) mp = case length s of
                    2 -> buildmap ss (M.insert 1 s mp)
                    3 -> buildmap ss (M.insert 7 s mp)
                    4 -> buildmap ss (M.insert 4 s mp)
                    7 -> buildmap ss (M.insert 8 s mp)
                    _ -> buildmap ss mp

deducerest :: [String] -> M.Map Int String -> M.Map Int String
deducerest [] mp = mp
deducerest (s:ss) mp = case length s of
                         5 -> case length (s \\ one) of
                                3 -> deducerest ss $ M.insert 3 s mp
                                _ -> case length (s \\ four) of
                                       2 -> deducerest ss $ M.insert 5 s mp
                                       _ -> deducerest ss $ M.insert 2 s mp
                         6 -> case length (s \\ seven) of
                                4 -> deducerest ss $ M.insert 6 s mp
                                _ -> case length (s \\ four) of
                                       3 -> deducerest ss $ M.insert 0 s mp
                                       2 -> deducerest ss $ M.insert 9 s mp
        where one = fromJust $ M.lookup 1 mp 
              seven = fromJust $ M.lookup 7 mp
              four  = fromJust $ M.lookup 4 mp

decode :: M.Map Int String -> String -> Int
decode mp s = fst . head $ filtered 
        where filtered = filter (\(k, v) -> sort v == sort s) ls
              ls = M.assocs mp

p2_indiv :: [[String]] -> String
p2_indiv [pattern, out] = concatMap (show . decode knownMap) $ out
        where easy = buildmap pattern M.empty 
              knownMap = deducerest (pattern \\ (M.elems easy)) easy

part2 :: String -> String
part2 = show . sum . map ((read :: String -> Int) . p2_indiv) . parse
