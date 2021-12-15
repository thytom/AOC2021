module Days.Day14 where

import qualified TestT as T
import qualified Days.Day as D

import Data.List.Split (splitOn)
import Data.List (nub, sort, transpose)
import Data.Maybe (fromJust)

import qualified Data.HashMap.Strict as M
import Control.Applicative

day = D.Day { D.name = "Day 14"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day14_test.txt"   "1588" "2188189693529"
            , D.input     = T.mkAssertion "day14_actual.txt" "2408" "2651311098752"}

type Rule = (String, Char)
type Rules = [Rule]
type PairMap = M.HashMap String Int
type Pair = (String, Int)
type CountMap = M.HashMap Char Int
type Polymer = String

-- Convert a list into frequencies
occurences :: (Eq a, Ord a) => [a] -> [(Int, a)]
occurences xs = sort $ zip (map (\x -> length . filter (==x) $ xs)  (nub xs)) (nub xs)

parse :: String -> (Polymer, Rules)
parse input = (head $ lines input, rules)
        where rules = map ((\[x, y] -> (x, head y)) .splitOn " -> ") . drop 2 $ lines input

pairs :: String -> PairMap -> PairMap
pairs (a:b:ss) m = case ss of
        [] -> m'
        _  -> pairs (b:ss) m'
        where m' = M.insertWith (+) [a, b] 1 m

count :: String -> CountMap
count s = foldl (\m c -> M.insertWith (+) c 1 m) M.empty s

applyrule :: Rule -> String -> Maybe ([String], Char)
applyrule (l, r) s@(a:bs) = if l==s then Just ([a:r:[], r:bs], r) else Nothing

tryrules :: Rules -> String -> Maybe ([String], Char)
tryrules rs s = foldl (<|>) Nothing $ map (\r -> applyrule r s) rs

matchrule :: Rules -> PairMap -> (PairMap, CountMap) -> (PairMap, CountMap)
matchrule [] _ p = p
matchrule ((s, c):rs) original_map (pm, cm) = 
        case M.lookup s original_map of
           Just n -> matchrule rs original_map (foldl (\m p -> M.insertWith (+) p n m) 
                        pm [head s : [c], c:tail s], M.insertWith (+) c n cm)
           Nothing -> matchrule rs original_map (pm, cm)

update :: Rules -> (PairMap, CountMap) -> (PairMap, CountMap)
update rs (pm, cm) = matchrule rs pm (M.empty, cm)

iter :: Int -> String -> Int
iter n input = (\is -> maximum is - minimum is) $ map (snd) $ M.toList final
        where (initial, rules) = parse input
              (ps, final) = (!!n) $ iterate (update rules) (pairs initial M.empty, count initial)

part1 :: String -> String
part1 = show . iter 10

part2 :: String -> String
part2 = show . iter 40
